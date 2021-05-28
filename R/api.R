#' Get Data
#' 
#' Get raw Mixpanel data.
#' 
#' @param from_date,to_date Object of class \code{date} from which, and to which, to query 
#' for the event. Both are inclusive.
#' @param limit the max number of events to be returned.
#' @param event The event or events that you wish to get data for, 
#' as a \code{vector} or a \code{list}. 
#' @param where An expression to filter events by. 
#' See the \href{https://developer.mixpanel.com/docs/data-export-api#section-segmentation-expressions}{expression section} 
#' on the main data export API page.
#' @param jsonl If \code{NULL} a temp JSON file is created to temporarily hold the results, 
#' if a path is passed then the intermediate results are stored on specified file.
#' Unlike the temp file the latter is not deleted. 
#' This option may be ueful if querying a lot of events.
#' @param read Whether to read the data back from the the \code{jsonl} file.
#' If \code{FALSE} the function invisibly return \code{jsonl}.
#' @param api_secret Your API secret defaults to an environment variable 
#' (\code{MIXPANEL_API_SECRET}).
#' @param verbose Whether to print helpful messages to the console.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{get_mixpanel_data} - Fetch Mixpanel data.}
#'   \item{\code{get_mixpanel_data_by_day}, \code{get_mixpanel_data_by_day} - Splits the call by day or week.}
#'   \item{\code{as_mixpanel_data} - Converts an existing \code{jsonl} file to an object of class \code{mixpanel_data}.}
#' }
#' 
#' @examples
#' \dontrun{
#' datasets <- get_mixpanel_data_by_day() 
#' }
#' 
#' @import purrr
#' @import dplyr
#' @import httr
#' @import assertthat
#' 
#' @seealso \href{https://developer.mixpanel.com/docs/exporting-raw-data#section-export-api-reference}{Official documentation} of the API call implmented here.
#' 
#' @name mixpanel_data
#' @export
get_mixpanel_data <- function(from_date = Sys.Date(), to_date = Sys.Date(), 
  limit = NULL, event = NULL, where = NULL, jsonl = NULL, read = TRUE,
  api_secret = Sys.getenv("MIXPANEL_API_SECRET"), verbose = TRUE){

  # assertions & checks
  assert_that(is_secret_valid(api_secret))
  assert_that(inherits(from_date, "Date"), inherits(to_date, "Date"))
  assert_that(is_range_valid(from_date, to_date))
  assert_that(is_output_sensible(jsonl, read))
  validate_that(is_range_long(to_date, from_date))

  # build call
  uri <- parse_url(BASE_URL)
  uri$path <- BASE_PATH

  # print feedback b/c function can take a while to run
  ev <- if(is.null(event)) "all events" else event
  if(verbose)
    cat(
      crayon::blue(cli::symbol$pointer),
      "Fetching", crayon::blurred(paste0(ev, collapse = ", ")), 
      "from", crayon::underline(format(from_date, "%d %B %Y")), 
      "to", crayon::underline(format(to_date, "%d %B %Y")), "\n"
    )
  
  # preprocess shitty input from mixpanel
  # well done to the engineer there!
  event <- .event(event)
  limit <- NULL
  # basic call
  # https://developer.mixpanel.com/docs/exporting-raw-data#section-export-api-reference
  uri$query <- list(
    from_date = from_date,
    to_date = to_date,
    limit = limit,
    event = event,
    where = where
  )
  uri <- build_url(uri)

  # call API
  response <- GET(uri, authenticate(api_secret, password = ""))
  stop_for_status(response) # check response
  content <- content(response) # extract content

  # process resultsq
  if(is.null(jsonl)) jsonl <- tempfile(fileext = ".jsonl")
  write(content, file = jsonl)
  
  # read content
  if(read) 
    content <- readLines(jsonl)
  else
    invisible(jsonl)

  content <- content[content != ""]

  # delete temp
  if(is.null(jsonl)) unlink(jsonl, force = TRUE)

  .construct_mixpanel_data(content)
}

#' @rdname mixpanel_data
#' @export
as_mixpanel_data <- function(jsonl){
  assert_that(!missing(jsonl), msg = "Missing `jsonl`.")
  content <- readLines(jsonl)
  .construct_mixpanel_data(content)
}

#' @name mixpanel_data
#' @export
get_mixpanel_data_by_day <- function(from_date = Sys.Date() - 3, to_date = Sys.Date(), 
  limit = NULL, event = NULL, where = NULL, jsonl = NULL, read = TRUE,
  api_secret = Sys.getenv("MIXPANEL_API_SECRET"), verbose = TRUE){

  assert_that(is_range_valid(from_date, to_date))

  # create sequence to loop over
  sequence <- seq.Date(from_date, to_date, by = "day")

  sequence %>% 
    map(function(date, event, where, jsonl, read, api_secret, verbose){
      Sys.sleep(.get_sleep())
      get_mixpanel_data(date, date, event, where, jsonl, read, api_secret, verbose = verbose)
    }, event = event, where = where, jsonl = jsonl, read = read,
      api_secret = api_secret, verbose = verbose) %>% 
      flatten() %>% 
      .construct_mixpanel_data()

}

#' @name mixpanel_data
#' @export
get_mixpanel_data_by_week <- function(from_date = Sys.Date() - 7, to_date = Sys.Date(), 
  limit = NULL, event = NULL, where = NULL, jsonl = NULL, read = TRUE,
  api_secret = Sys.getenv("MIXPANEL_API_SECRET"), verbose = TRUE){

  assert_that(is_range_valid(from_date, to_date))

  # create sequence to loop over
  sequence <- seq.Date(from_date, to_date, by = "week")
  from_to <- make_sequence(sequence)

  from_to %>% 
    map(function(range, event, where, jsonl, read, api_secret, verbose){
      Sys.sleep(.get_sleep())
      get_mixpanel_data(range$from, range$to, event, where, jsonl, read, api_secret, verbose = verbose)
    }, event = event, where = where, jsonl = jsonl, read = read,
      api_secret = api_secret, verbose = verbose) %>% 
      flatten() %>% 
      .construct_mixpanel_data()

}

#' Formatted Event
#' 
#' Returns formatted data from the "Segmentation" endpoint.
#' 
#' @inheritParams mixpanel_data
#' 
#' @examples
#' \dontrun{get_formatted_mixpanel_data()}
#' 
#' @export
get_formatted_mixpanel_data <- function(from_date = Sys.Date() - 7, to_date = Sys.Date(), 
  event = "Browse page", api_secret = Sys.getenv("MIXPANEL_API_SECRET")){

  # assertions & checks
  assert_that(is_secret_valid(api_secret))
  assert_that(inherits(from_date, "Date"), inherits(to_date, "Date"))
  assert_that(is_range_valid(from_date, to_date))
  validate_that(is_range_long(to_date, from_date))

  url <- "https://mixpanel.com/api/2.0/segmentation"
  url <- httr::parse_url(url)
  url$query <- list(
    from_date = from_date,
    to_date = to_date,
    event = event
  )
  url <- httr::build_url(url)

  response <- httr::GET(
    url,
    httr::authenticate(user = api_secret, password = "")
  )

  content <- httr::content(response)

  tibble::tibble(
    date = unlist(content$data$serie),
    value = unlist(content$data$value)
  )
}