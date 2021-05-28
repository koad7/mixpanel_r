#' Parse Mixpanel Data
#' 
#' Parse results of \code{\link{get_mixpanel_data}} or \code{\link{as_mixpanel_data}}.
#' 
#' @param mixpanel_data An object of class \code{mixpanel_data} as 
#' returned by code{\link{get_mixpanel_data}} and \code{\link{as_mixpanel_data}}.
#' @param dir Directory to hold JSON files, if \code{NULL} temp is created and 
#' subsequently deleted.
#' @param n Batch size to process, if \code{NULL} the function attempts to set
#' a sensible batch size.
#' 
#' @examples
#' \dontrun{
#' md <- get_mixpanel_data()  
#' parse_mixpanel_data(md)
#' }
#' 
#' @name parse_mixpanel_data
#' @export
parse_mixpanel_data <- function(mixpanel_data, dir = NULL, n = NULL) UseMethod("parse_mixpanel_data")

#' @rdname parse_mixpanel_data
#' @method parse_mixpanel_data mixpanel_data
#' @export
parse_mixpanel_data.mixpanel_data <- function(mixpanel_data, dir = NULL, n = NULL){
  
  directory <- dir
  if(is.null(dir))
    directory <- tempdir()

  if(is.null(n))
    n <- round(length(mixpanel_data) / log(length(mixpanel_data)) * 2)
  mixpanel_data_to_multiple_json(mixpanel_data, directory, n = n)
  data <- mixpanel_data_from_json_dir(directory)
  if(is.null(dir)) unlink(directory, TRUE, TRUE)

  # convert time
  if("time" %in% names(data))
    data[["time"]] <- convert_timestamp(data[["time"]])
  tibble::as_tibble(data)
}

#' @name parse_mixpanel_data
#' @export
parse_mixpanel_data2 <- function(mixpanel_data) UseMethod("parse_mixpanel_data2")

#' @rdname parse_mixpanel_data
#' @method parse_mixpanel_data2 mixpanel_data
#' @export
parse_mixpanel_data2.mixpanel_data <- function(mixpanel_data){
  lst <- purrr::map(mixpanel_data, jsonlite::fromJSON)

  dropNulls <- function(x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }

  data <- purrr::map_dfr(lst, function(data){
    df <- tibble::as_tibble(dropNulls(data$properties))
    df$event <- data$event
    return(df)
  })

  names(data) <- gsub("\\$", "", names(data))

  return(data)
}

#' Mixpanel to JSON
#' 
#' Save results of \code{\link{get_mixpanel_data}} or \code{\link{as_mixpanel_data}} as JSON.
#' 
#' @param mixpanel_data An object of class \code{mixpanel_data} as 
#' returned by code{\link{get_mixpanel_data}} and \code{\link{as_mixpanel_data}}.
#' @param dir,file Directory or JSON file to save or read JSON files.
#' @param n Size of batches.
#' 
#' @examples
#' \dontrun{
#' md <- get_mixpanel_data()  
#' mixpanel_data_to_json(md)
#' }
#' 
#' @name mixpanel_data_to_json
#' @export
mixpanel_data_to_json <- function(mixpanel_data, file) UseMethod("mixpanel_data_to_json")

#' @rdname mixpanel_data_to_json
#' @method mixpanel_data_to_json mixpanel_data
#' @export
mixpanel_data_to_json.mixpanel_data <- function(mixpanel_data, file){

  assert_that(!missing(file), msg = "Missing `file`")

  if(!file.exists(file)) file.create(file)

  n_files <- 1:length(mixpanel_data)
  max <- length(mixpanel_data)

  # progress bar
  fmt <- paste0(
    crayon::underline("  Writing files "),
    "[:bar] :current/:total (:percent) eta: :eta in :elapsed"
  )
  pb <- progress::progress_bar$new(
    format = fmt,
    total = length(mixpanel_data), clear = FALSE
  )

  map2(mixpanel_data, n_files, function(x, n, file, max, pb){
    pb$tick()
    if(n == 1)
      x <- paste0("[", x)
    if(n != max)
      x <- paste0(x, ",")
    else
      x <- paste0(x, "]")
    write(x, file = file, append = TRUE)
  }, file = file, max = max, pb = pb)

  invisible()
}

#' @rdname mixpanel_data_to_json
#' @export
mixpanel_data_to_multiple_json <- function(mixpanel_data, dir, n) UseMethod("mixpanel_data_to_multiple_json")

#' @method mixpanel_data_to_multiple_json mixpanel_data
#' @export
mixpanel_data_to_multiple_json.mixpanel_data <- function(mixpanel_data, dir, n){

  assert_that(!missing(dir), msg = "Missing `dir`")
  assert_that(!missing(n), msg = "Missing `n`")

  if(!dir.exists(dir)) dir.create(dir)

  mixpanel_data <- split(mixpanel_data, (1:length(mixpanel_data) %/% (n + 1)))
  for(i in 1:length(mixpanel_data)){
    file <- paste0(dir, "/mixpanel_", i, ".json")
    cat(
      crayon::green(cli::symbol$tick),
      " Writing batch ", crayon::blue(i), "/", 
      length(mixpanel_data), "\n", sep = ""
    )
    mixpanel_data_to_json(.construct_mixpanel_data(mixpanel_data[[i]]), file)
  }

  invisible()
}

#' @rdname mixpanel_data_to_json
#' @export
mixpanel_data_from_json_dir <- function(dir){
  
  assert_that(!missing(dir), msg = "Missing `dir`")
  assert_that(dir.exists(dir), msg = "Cannot find `dir`")

  dir <- normalizePath(dir)
  files <- list.files(dir) %>% 
    .[grepl("\\.json", .)] %>% 
    paste0(dir, "/", .) %>% 
    normalizePath()

  cat(
    crayon::blue(cli::symbol$info),
    length(files), "JSON files found",
    "\n"
  )

  # progress bar
  fmt <- paste0(
    crayon::blue("  Reading files "),
    "[:bar] :percent eta: :eta"
  )
  pb <- progress::progress_bar$new(
    format = fmt,
    total = length(files), clear = FALSE, width= 60
  )

  rez <- map(files, function(x, pb){
    pb$tick()
    jsonlite::fromJSON(x)
  }, pb = pb) %>% 
    map_dfr(function(x){
      df <- x$properties
      df$event <- x$event
      df$`Font Scale` <- NULL
      df$`$browser_version` <- NA
      df$`$language` <- NULL
      df$`language` <- NULL
      return(df)
    })

  n <- names(rez)
  n <- gsub("\\$", "", n)
  set_names(rez, n)
}


#' @rdname mixpanel_data_to_json
#' @export
mixpanel_data_from_json <- function(file){
  
  assert_that(!missing(file), msg = "Missing `file`")
  assert_that(file.exists(file), msg = "Cannot find `file`")

  file <- normalizePath(file)

  json <- jsonlite::fromJSON(file)
  data <- json$properties
  data$event <- json$event

  n <- names(data)
  n <- gsub("\\$", "", n)
  set_names(data, n)
}
