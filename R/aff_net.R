

#' Affiliation network
#'
#' @param user_list a list of unique users
#'
#' @return A named list. Names are users and values are subreddits.
#' @export
#'
create_aff_net <- function(user_list) {
  
  if (all(table(user_list) != 1)) warning("You're looking at duplicated users. Something must be wrong!")
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  
  message("Downloading subreddit lists for ", length(user_list), " users")
  
  output <- vector("list", length(user_list))
  
  pb <- txtProgressBar(0, length(user_list), style = 3)
  
  for (i in seq_along(user_list)) {
    
    output[[i]] <- sub_list_safe(user_list[[i]])
    
    setTxtProgressBar(pb, i)
    
  }
  
  names(output) <- user_list
  result <- purrr::transpose(output)$result
  
  diff <- length(output) - length(result)
  
  if (diff > 0) message("\nUnable to download information for ", diff, " users")
  
  return(result)
}


sub_list <- function(user) {
  
  reticulate::iterate(
    it = reddit$redditor(user)$new(), 
    f = function(x) x[["subreddit"]], 
    simplify = TRUE
  ) %>% 
    purrr::map_chr(as.character)
  
}

sub_list_safe <- purrr::safely(sub_list)
