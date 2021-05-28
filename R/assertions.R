# api secret
is_secret_valid <- function(x) {
  nchar(x) > 1
}

on_failure(is_secret_valid) <- function(call, env) {
  paste0(
    "`", crayon::blue("api_secret"), "`",
    " is ",
    crayon::red("not valid"),
    "."
  )
}

# dates
is_range_valid <- function(x, y) {
  x <= y
}

on_failure(is_range_valid) <- function(call, env) {
  paste0(
    crayon::red(call$y), 
    " is greater than ", 
    call$x,
    "`."
  )
}

is_range_long <- function(x, y) {
  !abs(x - y) > 3
}

on_failure(is_range_long) <- function(call, env) {
  paste(
    "Calling",
    crayon::red("more than"),
    "3 days of data: hang tight, this it'll take a while."
  )
}

# return
is_output_sensible <- function(x, y) {
  if(is.null(x) && !isTRUE(y))
    FALSE
  else
    TRUE
}

on_failure(is_output_sensible) <- function(call, env) {
  paste0(
    "`", crayon::blue("jsonl"), "` is set to `NULL` and `",
    crayon::blue("read"), "` is set to `FALSE`: function has no output.\n",
    "Either specify a path/to/a/file in `", crayon::blue("jsonl"), "` or set `",
    crayon::blue("read"), "` to `TRUE`."
  )
}