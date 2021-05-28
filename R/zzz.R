.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the ", 
    crayon::underline("mixpanel"),
    " package.\n",
    "Feel free to contact the author at ",
    crayon::blue("jcoe@weforum.org"),
    " if you have any question."
  )

  # set sleep
  sleep <- getOption("mixpanel_sleep")
  if(is.null(sleep)) options(mixpanel_sleep = 1L)
}