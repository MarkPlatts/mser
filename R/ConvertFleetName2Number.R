library(stringr)

#' LookupInFile
#'
#' Looks up x in the first column of file and returns the value in the second column
#'
#' @param file path to the look up file
#' @param x the value to look up in the first column
#' @param header whether the lookup file contains column headers to skip
#'
#' @return the values in the second column corresponding to the value found in the first column
LookupInFile <- function(file, x, header = TRUE){

  y <- NULL

  lookup_file <- read.csv(file, skip = as.integer(header), stringsAsFactors = F)
  index <- which(stringr::str_detect(lookup_file[[1]], x))
  y <- lookup_file[[2]][index]

  return(y)

}






