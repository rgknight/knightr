#' sep_dups
#'
#' Return a list of dataframes two elements
#'
#' * .$unique - observations that have no duplicate elements
#' (i.e., it does not pick one from the duplicated elements)
#' * .$duplicates - observations that have any duplicate elements
#' (i.e., if there are two duplicates for an observation, it returns both of them)
#'
#' The `length(unique)` + `length(duplicates)` should equal the length of df
#' @param df The source dataframe
#' @param ... A select style set of variables to check for duplicates. Uses standard evaluation, so you need to wrap in quotes
#' @keywords duplicates
#' @export
#' @examples
#' sep_dups(iris, "Sepal.Width", "Sepal.Length")
#' is_complete(x)
sep_dups <- function(df, ...){
  target <- df %>% select_(.dots=...)
  dup_index <- duplicated(target) | duplicated(target, fromLast = TRUE)

  list(unique = df[!dup_index, ],
       duplicates = df[dup_index, ])
}
