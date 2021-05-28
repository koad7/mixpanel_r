#' Count events
#' 
#' Count the number of events by various groups.
#' 
#' @inheritParams session_time
#' @param events Regular expression of events to keep where \code{.}
#' keeps all events, passed to \link[base]{grepl}.
#' @param sort Whether to sort the countries by number of events.
#' @param units Passed to \link[base]{round}.
#' @param vars The level of granularity desired, the full url or the domain only.
#' 
#' @return A \link[tibble]{tibble} with the country code and the number 
#' of events.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' 
#' # by country
#' cns <- events_by_country(sample_data)
#' plot(cns)
#' 
#' # browse page events by hour
#' ts <- events_by_ts(
#'   sample_data, 
#'   events = "Browse page",
#'   units = "hours"
#' )
#' plot(ts)
#' }
#' 
#' @name events_by
#' @export
events_by_country <- function(data, events = ".", sort = FALSE) UseMethod("events_by_country")

#' @export
events_by_country.default <- function(data, events = ".", sort = FALSE){
  data %>% 
    filter(!is.na(mp_country_code)) %>% 
    filter(grepl(events, event)) %>% 
    count(mp_country_code, sort = sort, name = "events") %>% 
    select(country = mp_country_code, events) %>% 
    .construct_events_by("country")
}

#' @rdname events_by
#' @export
events_by_ts <- function(data, events = ".", sort = FALSE, 
  units = c("mins", "hours", "days", "months")) UseMethod("events_by_ts")

#' @export
events_by_ts.default <- function(data, events = ".", sort = FALSE, 
  units = c("mins", "hours", "days", "months")){

  units <- match.arg(units)

  data %>% 
    mutate(
      time = as.POSIXct(round(time, units))
    ) %>% 
    filter(!is.na(time)) %>% 
    filter(grepl(events, event)) %>% 
    count(time, sort = sort, name = "events") %>% 
    .construct_events_by("time")
}


#' @rdname events_by
#' @export
events_by_referrer <- function(data, events = ".", sort = FALSE, 
  vars = c("domain", "url")) UseMethod("events_by_referrer")

#' @export
events_by_referrer.default <- function(data, events = ".", sort = FALSE,
  vars = c("domain", "url")){

  vars <- match.arg(vars)
  vars <- ifelse(vars == "domain", "initial_referring_domain", "initial_referrer")
 
  data %>%
    select("event", referrer = vars) %>%  
    filter(!is.na(referrer)) %>% 
    filter(grepl(events, event)) %>% 
    count(referrer, sort = sort, name = "events") %>% 
    .construct_events_by("referrer")
}