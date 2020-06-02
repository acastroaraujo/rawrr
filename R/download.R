
#' Download urls by subreddit
#'
#' @param sub a string representing a subreddit
#' @param limit a number no greater than 1,000
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- download_sub_urls("rstats", 10)
#' }
download_sub_urls <- function(sub, limit = 1e3) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  if (limit > 1e3) stop("Max. limit is 1,000", call. = FALSE)
  
  message("Pulling from /r/", sub)
  
  subreddit <- reddit$subreddit(sub)
  top <- subreddit$top(limit = limit) ## 1000 is maximum limit
  
  reticulate::iterate(
    it = top, 
    f = url_info
    ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC"),
                  subreddit = sub)
  
}


#' Download urls by subreddit
#'
#' @param keyword a search query, for more information building a search query go here: https://www.reddit.com/wiki/search
#' @param sub a subreddit, unless you want to search in all subreddits, in which case the value is "all" (default)
#' @param sort_by  Can be one of "top" (default), "relevance", "hot", "new", or "comments".
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- download_keyword_urls(keyword = "reticulate", sub = "rstats")
#' }
download_keyword_urls <- function(keyword, sub = "all", sort_by = "top") {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  message(paste0("Searching for <<", keyword, ">> in '", sub, "'"))
  
  subreddit <- reddit$subreddit(sub)
  top <- subreddit$search(keyword, limit = NULL, sort = sort_by) ## maximum limit
  
  output <- reticulate::iterate(
    it = top, 
    f = url_info
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC"),
                  subreddit = sub)
  
  if (sub == "all") {
    output <- output %>% 
      dplyr::mutate(subreddit = stringr::str_replace(.data$path, "\\/r\\/([A-Za-z\\d]+)\\/.*", "\\1"))
  }
  
  return(output)
  
}


url_info <- function(x) {
  
  tibble::tibble(
    title = x$title,
    post_score = x$score,
    num_comments = x$num_comments,
    date = x$created_utc,
    path = x$permalink
  )
}