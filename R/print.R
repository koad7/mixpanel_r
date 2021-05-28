#' @export
print.mixpanel_data <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info),
    prettyNum(length(x), big.mark = ","), 
    "files of Mixpanel data.\n",
    ...
  )
}

#' @export
print.mixpanel_list_data <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info),
    prettyNum(length(x), big.mark = ","), 
    "files of Mixpanel data.\n",
    ...
  )
}
