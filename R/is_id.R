#' is_id
#'
#' Check if observations are uniquely identified.
#' If there are duplicates, print the number of duplicates.
#' @param x potential unique identifier
#' @keywords duplicates, id, is_id
#' @export
#' @examples
#' id <- 1:10
#' is_id(id)
is_id <- function(x){
  numdups <- sum(duplicated(x))
  if (numdups > 0){
    stop(sprintf("There are %i duplicates in %s", numdups, deparse(substitute(x)))) 
  }
}