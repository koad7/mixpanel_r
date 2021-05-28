#' Convert Timestamp
#' 
#' Convert the Mixpanel timestamp to an object of clas \code{POSIXct}.
#' 
#' @param ts Vector or bare column name of timestamp (\code{time}).
#' 
#' @export
convert_timestamp <- function(ts) {
  assert_that(!missing(ts), msg = "Missing `ts`.")
  converted <- tryCatch(
    as.POSIXct(ts, origin = "1970-01-01", tz = "GMT"),
    error = function(e) e
  )

  if(inherits(converted, "error")){
    warning("Datetime conversion did not work", call. = FALSE)
    return(ts)
  }

  return(converted)
}