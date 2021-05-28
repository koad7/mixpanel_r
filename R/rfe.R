#' Recency, Frequency, Engagement
#' 
#' Computes the RFE score of users.
#' 
#' @inheritParams session_time
#' @param since Since when to compute recency.
#' @param units Passed to \link[base]{round} and applied to argument 
#' \code{since} as well.
#' 
#' @export
mixpanel_rfe <- function(data, units = c("mins", "hours", "days", "months"),
  since = Sys.time()) UseMethod("mixpanel_rfe")

#' @export
mixpanel_rfe.default <- function(data, units = c("mins", "hours", "days", "months"),
  since = Sys.time()){

  cat(crayon::blue("1."), "Computing", crayon::underline("recency"), "\n")
  recency <- mixpanel_recency(data, since = since, units = units)
  cat(crayon::blue("2."), "Computing", crayon::underline("engagement"), "\n")
  engagement <- mixpanel_engagement(data)
  cat(crayon::blue("3."), "Computing", crayon::underline("frequency"), "\n")
  frequency <- mixpanel_frequency(data, units = units)

  recency %>% 
    select(distinct_id, recency) %>% 
    left_join(frequency, by = "distinct_id") %>% 
    left_join(engagement, by = "distinct_id") %>% 
    mutate(
      recency = as.numeric(recency),
      recency = scales::rescale(recency, to = c(10, 1)),
      engagement = scales::rescale(engagement, to = c(1, 10)),
      frequency = scales::rescale(frequency, to = c(1, 10))
    ) %>% 
    mutate(
      score = scales::rescale(recency + frequency + engagement, to = c(1, 10))
    ) %>% 
    arrange(-score)
}

#' Latest Activity
#' 
#' Computes when a user was last active.
#' 
#' @inheritParams session_time
#' 
#' @return A \link[tibble]{tibble} of user ids and time at 
#' which they were last seen on the paltform.
#' 
#' @note This is based on any kind of event being triggered
#' rather than the inaccurate log in event.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' mixpanel_latest_activity(sample_data)
#' }
#' 
#' @export
mixpanel_latest_activity <- function(data) UseMethod("mixpanel_latest_activity")

#' @export
mixpanel_latest_activity.default <- function(data) {
  users <- extract_property(data, "distinct_id")
  time <- extract_property(data, "time")

  data %>% 
    group_by(distinct_id) %>% 
    filter(time == max(time)) %>% 
    ungroup() %>% 
    distinct() %>% 
    filter(!is.na(distinct_id)) %>% 
    filter(!is.na(time)) %>% 
    .construct_latest_activity()
}

#' Recency
#' 
#' Computes how recently a user was last active.
#' 
#' @param data Mixpanel data, an object of class \code{mixpanel_list_data}
#' as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
#' set to \code{FALSE} or the output of \code{\link{mixpanel_latest_activity}}.
#' @param since Since when to compute recency.
#' @param units Passed to \link[base]{round} and applied to argument 
#' \code{since} as well.
#' 
#' @return A \link[tibble]{tibble} of user ids and recency as 
#' \code{difftime} object.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' mixpanel_latest_activity(sample_data) %>% 
#'   mixpanel_recency()
#' }
#' 
#' @export
mixpanel_recency <- function(data, since = Sys.time(), 
  units = c("mins", "hours", "days", "months")) UseMethod("mixpanel_recency")

#' @export
mixpanel_recency.default <- function(data, since = Sys.time(), 
  units = c("mins", "hours", "days", "months")){
  unit <- match.arg(units)

  data %>% 
    mutate(
      since = as.POSIXct(round(since, unit)),
      time = as.POSIXct(round(time, unit))
    ) %>% 
    mutate(
      recency = since - time
    ) %>% 
    group_by(distinct_id) %>% 
    filter(recency == min(recency)) %>% 
    ungroup() %>% 
    select(distinct_id, recency) %>% 
    distinct()
}

#' Engagement
#'
#' Computes total engagement of user.
#' 
#' @param data Mixpanel data, an object of class \code{mixpanel_list_data}
#' as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
#' set to \code{FALSE} or the output of \code{\link{mixpanel_latest_activity}}.
#' @param events Regular expression of events to keep where \code{.}
#' keeps all events, passed to \link[base]{grepl}.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' mixpanel_engagement(sample_data)
#' }
#' 
#' @return A \link[tibble]{tibble} of user ids and frequency.
#' 
#' @export
mixpanel_engagement <- function(data, events = ".") UseMethod("mixpanel_engagement")

#' @export
mixpanel_engagement.default <- function(data, events = "."){
  data %>% 
    filter(grepl(events, event)) %>% 
    filter(!is.na(distinct_id)) %>% 
    count(distinct_id, name = "engagement")
}

#' Frequency
#'
#' Computes frequency at which user engages with the platform.
#' 
#' @param data Mixpanel data, an object of class \code{mixpanel_list_data}
#' as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
#' set to \code{FALSE} or the output of \code{\link{mixpanel_latest_activity}}.
#' @param units Passed to \link[base]{round} and applied to argument 
#' \code{since} as well.
#' @param events Regular expression of events to keep where \code{.}
#' keeps all events, passed to \link[base]{grepl}.
#' 
#' @return A \link[tibble]{tibble} of user ids and frequency.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' mixpanel_frequency(sample_data)
#' }
#' 
#' @export
mixpanel_frequency <- function(data, events = ".", units = c("mins", "hours", "days", "months")) UseMethod("mixpanel_frequency")

#' @export
mixpanel_frequency.default <- function(data, events = ".", units = c("mins", "hours", "days", "months")){
  unit <- match.arg(units)

  # compute number of freq
  mn <- as.POSIXct(round(min(data$time), unit))
  mx <- as.POSIXct(round(max(data$time), unit))
  L <- length(seq(mn, mx, by = unit))

  data %>% 
    filter(grepl(events, event)) %>% 
    filter(!is.na(distinct_id)) %>% 
    mutate(
      time = as.POSIXct(round(time, unit))
    ) %>% 
    group_by(distinct_id) %>% 
    summarise(
      frequency = n_distinct(time) / L
    )
}
