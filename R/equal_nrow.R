#' equal_nrow
#'
#' Check if two objects have equal number of rows.
#' If there are differences, print the sizes.
#' @param x First object to check
#' @param y Second object to check
#' @export
#' @examples
#' x <- 1:10
#' y <- 1:9
#' equal_nrow(x, y)
equal_nrow <- function(x, y){
  if (nrow(x) != nrow(y)){
    stop(sprintf("%s has %i rows; %s has %i rows",
                 deparse(substitute(x)), nrow(x),
                 deparse(substitute(y)), nrow(y)))
  }
}
