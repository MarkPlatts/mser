#' Get Area
#'
#' @param path the path to where the StockAreas.csv file is held
#' @param file.name the name of the result file that is being plotted
#' @param area the area represented by the functional group found in the result file name
#'
#' @return the area as a numeric(1)
#' @export
get_area <- function(path, file.name, area){
  stock.areas.file.path <- paste0(plot.path, "StockAreas.csv")
  if(file.exists(stock.areas.file.path)){
    stock.areas <- read.csv(stock.areas.file.path, stringsAsFactors = F)
    n.areas <- dim(stock.areas)[1]
    for(irow in 1:n.areas){
      if(str_detect(str_to_upper(file.name), str_to_upper(stock.areas[irow,1]))) return (stock.areas[irow, 2])
    }
  }
  return (area)
}
