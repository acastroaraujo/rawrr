
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
#' @param username username
#' @param password password
#'
#' @export
#'
init_reddit <- function(username, password) {

  # makeActiveBinding(
  #   sym = "reddit", 
  #   env = globalenv(),
  #   fun = {
  #     function() rawr:::praw$Reddit(
  #       client_id = "YO_Hqf9k3SB3sg",
  #       client_secret = "vwfEPm-4cz0ARWBEFkRdgmt2mhU",
  #       password = password,
  #       user_agent = 'Comment Extraction (by acastroaraujo)',
  #       username = username
  #       )
  #     }
  #   )
  
  
  assign(
    x = "reddit", 
    value = praw$Reddit(
      client_id = "YO_Hqf9k3SB3sg",
      client_secret = "vwfEPm-4cz0ARWBEFkRdgmt2mhU",
      password = password,
      user_agent = 'Comment Extraction (by acastroaraujo)',
      username = username), 
    envir = globalenv()
  )
  
  
}

# 
# reddit <<- praw$Reddit(client_id = "YO_Hqf9k3SB3sg",
#                        client_secret = "vwfEPm-4cz0ARWBEFkRdgmt2mhU",
#                        password = password,
#                        user_agent = 'Comment Extraction (by acastroaraujo)',
#                        username = username)


