
sub_list <- function(user) {
  
  reticulate::iterate(
    it = reddit$redditor(user)$new(), 
    f = function(x) x[["subreddit"]], 
    simplify = TRUE
  ) %>% 
    purrr::map_chr(as.character)
  
}

sub_list_safe <- purrr::safely(sub_list)

get_aff_net <- function(list_of_unique_users) {
  
  if (all(table(list_of_unique_users) != 1)) warning("You're looking at duplicated users. Something must be wrong!")
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  
  output <- vector("list", length(list_of_unique_users))
  pb <- dplyr::progress_estimated(length(list_of_unique_users))
  
  for (i in seq_along(list_of_unique_users)) {
    #output[[i]] <- try(sub_list(list_of_unique_users[[i]]))
    output[[i]] <- sub_list_safe(list_of_unique_users[[i]])
    
    try(pb$tick()$print())
  }
  
  names(output) <- list_of_unique_users
  result <- purrr::transpose(output)$result
  message(length(output) - length(result), " users were lost")
  return(result)
}
