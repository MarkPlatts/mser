CreateListString <- function(x = NULL){
  if(is.null(x)) return (NULL)
  temp_string = x[1]
  if (length(x) > 2) {
    for(i in 2:(length(x) - 1)){
      temp_string <- paste0(temp_string, ", ", x[i])
    }
  }
  if (length(x) > 1) {
    temp_string <- paste0(temp_string, " and ", tail(x, n = 1))  
  }
  return(temp_string)
}

#Check that multiple strings all exist within another string
StringContains_AllStrings = function(ContainingString, MultipleStrings2Check)
{
  for(iString in MultipleStrings2Check){
    if(StringContains(ContainingString, iString)==FALSE) 
      return(FALSE)
  }
  return(TRUE)
}

#Find a string within another string
#This can be done in a single line but it is fairly unreadable
StringContains = function(ContainingString, String2Check)
{
  return(length(grep(String2Check, ContainingString, fixed=TRUE))>0)
}