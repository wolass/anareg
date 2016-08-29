#' Deletes the "UNKNOWN" variable levels from the factor.
#' 
#' It takes a vector of numbers referred to the variables you want to include. 
#' The variables have to be in the correct order to properly assign weights.
#' the 
#' 
#' @param x The value to check if "Unknown" is the in the variable levels
#' @return The changed value with NA insted of "unknown"
#' 
#' @export
#' 
#' @examples
#' sapply(x.vector,make.na) 
#' 

make.na <-function(x){
  if(is.na(x)){
    return(NA)
  } else if(x=="unknown"|x=="don?t know"){
    return(NA)
  } else {
    return(x)
  }
}