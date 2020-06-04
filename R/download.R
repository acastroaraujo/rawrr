
#' Download urls by subreddit
#'
#' @param sub a string representing a subreddit
#' @param limit a number no greater than 1,000
#' @param sort_by  Can be one of "top" (default), "hot", or "new".
#' @param time_filter  can be one of: "all" (default), "day", "hour", "month", "week", or "year"
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- download_sub_urls("rstats", 10)
#' }
download_sub_urls <- function(
  sub, 
  sort_by = c("top", "hot", "new"), 
  time_filter = c("all", "day", "hour", "month", "week", "year"),
  limit = 1e3
  ) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  if (limit > 1e3) stop("`limit` must not exceed 1,000", call. = FALSE)
  
  type <- match.arg(sort_by)
  time <- match.arg(time_filter)
  subreddit <- reddit$subreddit(sub)
  
  input <- switch (type,
    "top" = subreddit$top(limit = limit, time_filter = time),
    "hot" = subreddit$hot(limit = limit, time_filter = time),
    "new" = subreddit$new(limit = limit, time_filter = time)
  )
  
  message("Pulling from /r/", sub)
  
  output <- reticulate::iterate(
    it = input, 
    f = url_info
    ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC"),
                  subreddit = sub)
  
  return(output)
  
}


#' Download urls by search query
#'
#' @param query a search query, for more information building a search query go here: https://www.reddit.com/wiki/search
#' @param sub a subreddit, unless you want to search in all subreddits, in which case the value is "all" (default)
#' @param sort_by  Can be one of "top" (default), "relevance", "hot", "new", or "comments".
#' @param limit a number, the API won't give you more than 250 results
#' @param time_filter  can be one of: "all" (default), "day", "hour", "month", "week", or "year"
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- download_keyword_urls(query = "reticulate", sub = "rstats")
#' }
download_keyword_urls <- function(
  query, sub = "all", 
  sort_by = c("top", "hot", "new", "relevance", "comments"), 
  limit = NULL, 
  time_filter = c("all", "day", "hour", "month", "week", "year")
  ) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  if (limit > 250) warning("The API will only allow limit = 250", call. = FALSE)
  type <- match.arg(sort_by)
  time <- match.arg(time_filter)
  
  message(paste0("Searching for <<", query, ">> in '", sub, "'"))
  subreddit <- reddit$subreddit(sub)
  input <- subreddit$search(query, limit = limit, sort = sort_by, time_filter = time) 
  
  output <- reticulate::iterate(
    it = input, 
    f = url_info
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC"),
                  subreddit = sub)
  
  if (sub == "all") {
    output <- output %>%
      dplyr::mutate(subreddit = stringr::str_replace(.data$path, "\\/r\\/([A-Za-z\\d_]+)\\/.*", "\\1"))
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