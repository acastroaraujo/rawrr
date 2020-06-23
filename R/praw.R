
# Set up ------------------------------------------------------------------

#' Import PRAW
#' 
#' This a simple wrapper for the reticulate::py_install() function.
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment. Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda The path to a conda executable. Use "auto" to allow reticulate to automatically find an appropriate conda binary. 
#'
#' @export
#'
install_praw <- function(method = "auto", conda = "auto") {
  reticulate::py_install("praw", method = method, conda = conda)
}


init_message <- paste0("Use init_reddit() to start a Reddit instance.")

#' Initialize Reddit instance
#' 
#' You must register for an application at https://www.reddit.com/prefs/apps/
#' Follow these steps https://github.com/reddit-archive/reddit/wiki/OAuth2-Quick-Start-Example#first-steps
#'
#' @param username username
#' @param password password
#' @param client_id client_id
#' @param client_secret client_secret
#'
#' @export
#'
init_reddit <- function(username, password, client_id, client_secret) {

  assign(
    x = "reddit", 
    value = praw$Reddit(
      client_id = client_id,
      client_secret = client_secret,
      password = password,
      user_agent = paste0('comment extraction (by /u/', username,')'),
      username = username), 
    envir = globalenv()
  )
  
  message("Connected as <<", as.character(reddit$user$me()), ">>")
  
}


globalVariables("reddit")


