#' read_csv_folder
#'
#' Read csv for each file in a folder.
#' Useful if you have a datasource that was broken up into multiple files,
#' e.g., one file per year, and you want to combine the files into a single result.
#' Returns a list of tibbles (you will do the combining on your own).
#' @param path path to the folder you want to read
#' @param pattern passthrough pattern for list.files
#' @param col_types passthrough col_types to read_csv
#' @param ... Additional arguements to read_csv
#' @export
read_csv_folder <- function(path, pattern=".csv", col_types=NULL, ...){
  files <- list.files(path = path, pattern=pattern)
  list_all <- vector(mode="list", length=length(files))
  names(list_all) <- files

  for(i in files) {
    message(sprintf('Reading file: %s', i))
    X <- readr::read_csv(paste(path, i, sep = "/"), col_types = col_types, ...)
    X$.src <- i
    list_all[[i]] <- X
  }

  list_all
}
