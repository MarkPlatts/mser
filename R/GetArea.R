#' Get Area
#'
#' @param path the path to where the StockAreas.csv file is held
#' @param func_group the name of the functional group that is being plotted
#' @param area the area of the model
#'
#' @return the area for which the functional group being plotted is applied: numeric(1)
#' @export
get_area <- function(path, func_group, area){
  stock.areas.file.path <- paste0(path, "StockAreas.csv")
  if(file.exists(stock.areas.file.path)){
    stock.areas <- read.csv(stock.areas.file.path, stringsAsFactors = F)
    n.areas <- dim(stock.areas)[1]
    for(irow in 1:n.areas){
      if(stringr::str_detect(stringr::str_to_upper(func_group), stringr::str_to_upper(stock.areas[irow,1]))) return (stock.areas[irow, 2])
    }
  }
  return (area)
}
