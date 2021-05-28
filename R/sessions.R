#' Session Time
#' 
#' Compute session time.
#' 
#' @param data Mixpanel data, an object of class \code{mixpanel_list_data}
#' as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
#' set to \code{FALSE}.
#' @param timeout Number of seconds before the user is considered logged out.
#' @inheritParams events_by
#' 
#' @return A \link[tibble]{tibble} of sessions containing the start and end time
#' of the session as well as the unique user id, the session duration in seconds,
#' and the number of clicks done during each session.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' session_time(sample_data)
#' }
#' 
#' @name session_time
#' @export
session_time <- function(data, timeout = 15 * 60, events = ".") UseMethod("session_time")

#' @export
session_time.default <- function(data, timeout = 15 * 60, events = "."){
  
  sessions <- data %>% 
    filter(!is.na(distinct_id)) %>% 
    filter(grepl(events, event)) %>% 
    arrange(time) %>% 
    group_split(distinct_id)

  fmt <- paste0(
    crayon::underline("  Computing session time "),
    "[:bar] :current/:total (:percent) eta: :eta in :elapsed"
  )
  N <- length(sessions)
  pb <- progress::progress_bar$new(
    total = N, 
    format = fmt,
    clear = FALSE
  )

  sessions %>% 
    map_dfr(function(x, timeout, pb){
      pb$tick()
      x <- x %>% 
        mutate(
          time_lag  = lag(time),
          diff = as.numeric(time - time_lag),
          session = 1
        )

      for(i in 1:nrow(x)){
        if(!is.na(x$diff[i]))
          if(x$diff[i] < timeout)
            x$session[i] <- x$session[i - 1]
          else
            x$session[i] <- x$session[i - 1] + 1
      }
      return(x)
    }, timeout, pb = pb) %>% 
      select(-time_lag) %>% 
      group_by(distinct_id, session) %>% 
      summarise(
        start = min(time),
        end = max(time),
        events = n(),
        duration = as.difftime(end - start)
      ) %>% 
      ungroup() %>% 
      mutate(
        duration = as.numeric(duration, units = "secs")
      ) %>% 
      select(-session)
} 

#' @rdname session_time
#' @export
sessionise <- function(data, timeout = 15 * 60, events = ".") UseMethod("sessionise")

#' @export
sessionise.default <- function(data, timeout = 15 * 60, events = "."){
  
  sessions <- data %>% 
    filter(!is.na(distinct_id)) %>% 
    filter(grepl(events, event)) %>% 
    arrange(time) %>% 
    group_split(distinct_id)

  fmt <- paste0(
    crayon::underline("  Computing sessions "),
    "[:bar] :current/:total (:percent) eta: :eta in :elapsed"
  )
  N <- length(sessions)
  pb <- progress::progress_bar$new(
    total = N, 
    format = fmt,
    clear = FALSE
  )

  sessions %>% 
    map_dfr(function(x, timeout, pb){
      pb$tick()
      x <- x %>% 
        mutate(
          time_lag  = lag(time),
          diff = as.numeric(time - time_lag),
          session = 1
        )

      for(i in 1:nrow(x)){
        if(!is.na(x$diff[i]))
          if(x$diff[i] < timeout)
            x$session[i] <- x$session[i - 1]
          else
            x$session[i] <- x$session[i - 1] + 1
      }
      return(x)
    }, timeout, pb = pb) %>% 
    select(-diff)
} 

#' Events
#' 
#' Count number of events by users.
#' 
#' @inheritParams session_time
#' @inheritParams events_by
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' users_events(sample_data, events = "Browse page", sort = TRUE)
#' }
#' 
#' @export
users_events <- function(data, events = ".", sort = FALSE) UseMethod("users_events")

#' @export
users_events.default <- function(data, events = ".", sort = FALSE){

  data %>% 
    filter(grepl(events, event)) %>% 
    filter(!is.na(distinct_id)) %>% 
    count(distinct_id, name = "events", sort = sort)
}
