#' Language Changes
#' 
#' Assess from which to which languages user switch.
#' 
#' @inheritParams session_time
#' @inheritParams events_by
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' language_change(sample_data, events = ".")
#' }
#' 
#' @export
language_change <- function(data, events = "Change Language") UseMethod("language_change")

#' @export
language_change.default <- function(data, events = "Change Language") {

  data %>% 
    filter(grepl(events, event)) %>% 
    count(language, sort = TRUE)
}