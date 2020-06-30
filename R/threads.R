
# TO DO: ADD A FUNCTION THAT ESTIMATES THE TIME REQUIRED FOR DOWNLOAD

#' Extract a thread from a path
#'
#' @param path a character string that starts with /r/, which you obtain using either the download_sub_urls() or the download_keyword_urls() functions
#' @param replace_limit either `NULL` (default) or a positive integer. This corresponds to the `limit` argument in PRAW's `replace_more()` method. See here: https://praw.readthedocs.io/en/latest/code_overview/other/commentforest.html#praw.models.comment_forest.CommentForest.replace_more Currently, this is what causes getting data to be slow in some cases. 
#'
#' @return A list with two tibbles: 1. information on nodes and 2. an edge list
#' @export
#'
#' @examples
#' \dontrun{
#' df <- extract_thread("/r/rstats/comments/f5bxyk/r_will_use_stringsasfactors_false_by_default_in/")
#' }
extract_thread <- function(path, replace_limit = NULL) {
  
  if (!exists("reddit", where = globalenv())) stop(init_message, call. = FALSE)
  if (!is.null(replace_limit) & is.numeric(replace_limit)) {
    if (!is_wholenumber(replace_limit) | replace_limit < 0) stop("<<replace_limit>> must be `NULL` or a whole number >= 0", call. = FALSE)
  }
  
  message("Extracting: ", path)
  
  thread <- paste0("https://www.reddit.com", path, ")")
  submission <- reddit[["submission"]](url = thread)
  submission$comments$replace_more(limit = replace_limit) ## this step can be extremely slow...
                                                  ## how can we fix this? https://praw.readthedocs.io/en/latest/tutorials/comments.html
  root_df <- tibble::tibble(
    name = submission$id,
    author = as.character(submission$author),
    date = get_date(submission),
    children = length(submission$comments[py_index]),
    descendents = length(submission$comments$list()),
    text = submission$selftext,
    title = submission$title,
    media = submission$url,
    subreddit = as.character(submission$subreddit),
    path = submission$permalink
  )
  
  nodes <- submission$comments$list()  ## all nodes
  
  branches_df <- tibble::tibble(
    name = purrr::map_chr(nodes, ~ .x$id),
    author = purrr::map_chr(nodes, get_author),
    date = purrr::map_dbl(nodes, get_date),
    children = purrr::map_int(nodes, ~ length(.x$replies[py_index])),
    descendents = purrr::map_int(nodes, ~ length(.x$replies$list())),
    text = purrr::map_chr(nodes, ~ .x$body),
    score = purrr::map_int(nodes, ~ .x$score),
    title = submission$title,
    subreddit = as.character(submission$subreddit),
    path = submission$permalink
  ) %>% 
    dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01"))
  
  df <- dplyr::full_join(root_df, branches_df, by = c("name", "author", "date", "children", "descendents", "text", "title", "subreddit", "path")) %>% 
    dplyr::select(.data$name, .data$author, .data$score, .data$text, .data$children, .data$descendents, dplyr::everything())
  
  output <- vector("list", length(nodes))
  for (i in seq_along(output)) {
    output[[i]] <- list(from = nodes[[i]][["parent_id"]], to = as.character(nodes[[i]]))
  }
  
  edge_list <- dplyr::bind_rows(output)
  
  if (length(nodes) > 0) {
    edge_list <- edge_list %>% 
      dplyr::mutate(from = stringr::str_sub(.data$from, 4))
  }
  
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
  
  output <- purrr::map(df$path, extract_thread_safely) %>% 
    purrr::transpose()
  
  df$nodes <- purrr::map(output$result, purrr::pluck, "nodes")
  df$edges <- purrr::map(output$result, purrr::pluck, "edges")
  
  num_errors <- sum(purrr::map_lgl(output$error, ~ !is.null(.x)))
  if (num_errors > 0) warning(num_errors, " errors downloading threads", call. = FALSE)
  
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

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

extract_thread_safely <- purrr::safely(extract_thread)


