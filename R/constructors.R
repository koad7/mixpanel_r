.construct_mixpanel_data <- function(x){
  structure(x, class = c("mixpanel_data", class(x)))
}

.construct_mixpanel_list_data <- function(x){
  structure(x, class = c("mixpanel_list_data", class(x)))
}

.construct_events_by <- function(x, var){
  cl <- paste0("events_by_", var)
  structure(x, class = c(cl, class(x)))
}

.construct_latest_activity <- function(x){
  structure(x, class = c("mixpanel_latest_activity", class(x)))
}

.construct_tm <- function(x){
  structure(x, class = c("mixpanel_tm", class(x)))
}