BASE_URL <- "https://data.mixpanel.com/"
BASE_PATH <- c("api", "2.0", "export")

globalVariables(
  c(
    ".",
    "start",
    "end",
    "time_lag",
    "session",
    "country",
    "event",
    "events",
    "duration",
    "timestamp",
    "time",
    "referrer",
    "color",
    "user",
    "language",
    "distinct_id",
    "mp_country_code",
    "recency",
    "score",
    "clean_name_ins",
    "clean_name_iss",
    "clean_name_top",
    "client",
    "edges",
    "ins_id",
    "iss_id",
    "name",
    "name_ins",
    "name_iss",
    "name_top",
    "sfid",
    "target",
    "top_id",
    "entityName",
    "secondaryEntityName"
  )
)

.to_tibble <- function(props){
  props <- map(props, function(x){
    if(is.null(x))
      x <- NA
    return(x)
  })
  tibble::as_tibble(props)
}

.clean <- function(x, func = purrr::map){
  # rename
  xn <- names(x)
  xn <- gsub("^\\$|^\\@", "", xn)
  xn <- strsplit(xn, "(?<=[a-z])(?=[A-Z])", perl = TRUE)
  xn <- func(xn, paste0, collapse = "_") %>% unlist %>% tolower
  xn <- make.names(xn, unique = TRUE)
  xn <- gsub(" ", "_", xn)
  names(x) <- xn

  # clean
  x$time <- convert_timestamp(x$time)

  return(x)
}

.event <- function(event = NULL){
  if(is.null(event)) return(event)

  event <- paste0(event, collapse = '", "')
  paste0('["', event, '"]')
}

.get_sleep <- function(){
  x <- getOption("mixpanel_sleep")
  if(is.null(x))
    return(1L)
  return(x)
}

extract_property <- function(data, var){
  data %>% 
    map("properties") %>% 
    map(function(x, var){
      if(is.null(x[[var]]))
        return(NA)
      return(x[[var]])
    }, var) %>% 
    unlist()
}

make_sequence <- function(sequence){
  from_to <- list()
  for(i in 1:(length(sequence) - 1)){
    diff <- ifelse(i > 1, 1, 0)
    l <- list(
      from = sequence[i] + diff,
      to = sequence[i + 1]
    )
    from_to <- append(from_to, list(l))
  }
  return(from_to)
}

clean_name <- function(x){
  x %>% 
    gsub("[[:punct:]]", "_", .) %>% 
    gsub("[[:space:]]", "_", .) %>% 
    tolower()
}