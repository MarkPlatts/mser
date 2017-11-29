#' LoadFile_ContainsListStrings
#'
#' Finds the file that contains any number of strings at a specified location and loads as a data.frame or table
#'
#' @param Dir.Path The path to the directory in which to search
#' @param StringsInFileName The vector of strings to search for
#' @param Read.Function What to use to load the file e.g. read.csv, fread
#'
#' @return the loeaded data.table or frame
LoadFile_ContainsListStrings = function(Dir.Path, StringsInFileName, Read.Function = read.csv)
  #Loads the file in folder specified containing all the strings in vector of strings
{

  #Get a list of all the files
  AllFiles <- list.files(Dir.Path)
  #Need to loop across all files so that we can extract
  for (iFile in AllFiles){
    #Find and load file that contains values for selected group and fleet
    FoundFile = StringContains_AllStrings(ContainingString = iFile, MultipleStrings2Check = StringsInFileName)
    if(FoundFile) {
      iFile.data <- Read.Function(paste(Dir.Path,iFile, sep=''),skip=7, head=T)
      return (iFile.data)
    }
  }
  return(NA)

}

