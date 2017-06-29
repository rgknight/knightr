#' get_dups
#'
#' Return all instances of duplicated values from a dataframe.
#' That is, if a column is supposed to be unique, but there are two identical
#' values, it will filter the dataframe to both instances of that value.
#'
#' @param df The source dataframe
#' @param ... A select style set of variables to check for duplicates.
#' @keywords duplicates
#' @export
#' @examples
#' sep_dups(iris, "Sepal.Width", "Sepal.Length")
get_dups <- function(df, ...){
  vars <- lazyeval::lazy_dots(...)
  target <- df %>% select_(.dots = vars)
  dup_index <- duplicated(target) | duplicated(target, fromLast = TRUE)

  df[dup_index, ]
}


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
sep_dups <- function(df, ...){
  target <- df %>% select_(.dots=...)
  dup_index <- duplicated(target) | duplicated(target, fromLast = TRUE)

  list(unique = df[!dup_index, ],
       duplicates = df[dup_index, ])
}


#' count_dups
#'
#' Returns a count of duplicate values for each duplicated value.
#' That is, if you have a column that is supposed to be unique,
#' but there are two instances of a given value,
#' it will return a count of 2 for that value.
#'
#' @param df The source dataframe
#' @param ... A select style set of variables to check for duplicates.
#' @param sort whether to sort the results by the number of duplicates. Default is FALSE
#' @keywords duplicates
#' @export
#' @examples
#' count_dups(iris, Sepal.Width, Sepal.Length)
count_dups <- function(df, ..., sort = FALSE){
  vars <- lazyeval::lazy_dots(...)

  dplyr::count_(df, vars, sort = sort) %>%
    dplyr::filter(n>1) %>%
    dplyr::rename(duplicates=n)
}

