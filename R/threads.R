
#' Extract a thread from a path
#'
#' @param path a character string that starts with /r/, which you obtain using either the download_sub_urls() or the download_keyword_urls() functions
#'
#' @return Two tibbles: information on nodes and an edge list
#' @export
#'
#' @examples
#' \dontrun{
#' df <- extract_thread("/r/rstats/comments/f5bxyk/r_will_use_stringsasfactors_false_by_default_in/")
#' }
extract_thread <- function(path) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  
  message("Extracting comments from ", path, "\n")
  
  thread <- paste0("https://www.reddit.com", path, ")")
  submission <- reddit[["submission"]](url = thread)
  submission[["comments"]][["replace_more"]](limit = NULL)
  
  root_df <- tibble::tibble(
    name = submission[["id"]],
    author = as.character(submission[["author"]]),
    date = get_date(submission),
    children = length(submission[["comments"]][py_index]),
    descendents = length(submission[["comments"]]$list()),
    text = submission[["selftext"]],
    title = submission[["title"]],
    media = submission[["url"]],
    subreddit = as.character(submission[["subreddit"]]),
    path = submission[["permalink"]]
  )
  
  nodes <- submission$comments$list()  ## all nodes
  
  branches_df <- tibble::tibble(
    name = purrr::map_chr(nodes, ~ .x[["id"]]),
    author = purrr::map_chr(nodes, get_author),
    date = purrr::map_dbl(nodes, get_date),
    children = purrr::map_int(nodes, ~ length(.x[["replies"]][py_index])),
    descendents = purrr::map_int(nodes, ~ length(.x[["replies"]]$list())),
    text = purrr::map_chr(nodes, ~ .x[["body"]]),
    score = purrr::map_int(nodes, ~ .x[["score"]]),
    title = submission$title,
    subreddit = as.character(submission[["subreddit"]]),
    path = submission[["permalink"]]
  ) %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01"))
  
  df <- dplyr::full_join(root_df, branches_df, by = c("name", "author", "date", "children", "descendents", "text", "title", "subreddit", "path")) %>% 
    dplyr::select(.data$name, .data$author, .data$score, .data$text, .data$children, .data$descendents, dplyr::everything())
  
  output <- vector("list", length(nodes))
  for (i in seq_along(output)) {
    output[[i]] <- list(from = nodes[[i]][["parent_id"]], to = as.character(nodes[[i]]))
  }
  
  edge_list <- dplyr::bind_rows(output) %>%
    dplyr::mutate(from = stringr::str_sub(.data$from, 4))
  
  list(nodes = df, edges = edge_list)  ## consider adding S3 class attribute with structure() in the future

}


#' Add thread information to a data frame
#'
#' @param df a tibble created by download_sub_urls() or download_keyword_urls()
#'
#' @return a modified tibble, with two extra list columns: nodes and edges
#' @export
#'
add_threads <- function(df) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  if (!"path" %in% colnames(df)) stop("data frame must contain a column called <<path>>", call. = FALSE)
  if ("nodes" %in% colnames(df)) stop("data frame can't contain a column called <<nodes>>", call. = FALSE)
  if ("edges" %in% colnames(df)) stop("data frame can't contain a column called <<edges>>", call. = FALSE)
  
  output <- purrr::map(df$path, rawr::extract_thread) %>% 
    purrr::transpose()
  
  df$doc_id <- seq_len(nrow(df))
  df$nodes <- output$nodes
  df$edges <- output$edges
  
  return(df)
}

get_date <- function(x) {
  as.POSIXct(x$created_utc, origin = "1970-01-01", tz = "UTC")
}

get_author <- function(x) {
  output <- x$author
  if (is.null(output)) return("[deleted]")
  as.character(output)
}


