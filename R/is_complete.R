#' is_complete
#'
#' Check if observations are never NULL or NA.
#' @param x The thing to check for completeness
#' @export
#' @examples
#' x <- 1:10
#' is_complete(x)
is_complete <- function(x){
  num_na <- sum(is.na(x) | is.null(x))
  if (num_na > 0){
    stop(sprintf("There are %i incomplete values (NA or NULL) in %s", num_na, deparse(substitute(x))))
  }
}
