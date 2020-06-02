
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to RAWR!\n", 
    "To use this package you need to install_praw() and init_reddit()\n",
    "You must register for an application at https://www.reddit.com/prefs/apps/"
  )
}

praw <- NULL
py_index <- NULL

.onLoad <- function(libname, pkgname) {
  praw <<- reticulate::import("praw", delay_load = TRUE)
  py_index <<- reticulate::import_builtins()$slice(NULL)
}


