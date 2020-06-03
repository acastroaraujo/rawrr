

#' User Network
#'
#' @param df a data frame, as produced by the add_threads() function
#'
#' @return an edge list, each tie means that the user responded to a comment made by another
#' @export
#'
user_net <- function(df) {

  if (!"nodes" %in% colnames(df)) stop("data frame must contain a column called <<nodes>>", call. = FALSE)
  if (!"edges" %in% colnames(df)) stop("data frame must contain a column called <<edges>>", call. = FALSE)
  
  lookup <- dplyr::bind_rows(df$nodes) %>% 
    dplyr::select(.data$name, .data$author) %>% 
    tibble::deframe()
  
  dplyr::bind_rows(df$edges) %>% 
    dplyr::mutate(
      from = lookup[.data$from],
      to = lookup[.data$to]
      ) %>% 
    dplyr::filter(.data$from != "[deleted]", .data$to != "[deleted]") 
  
}