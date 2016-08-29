#' Tells which variable to eliminate to get more observations.
#' 
#' This function helps you to decide which variable is restricting your analysis
#' by having the highest number of missing values in the selected set. 
#' 
#' @param predictors Predictors are the known variables, based on which
#' we want to conduct our predicting. 
#' @param outcome is the character string specifying the outcome variable. 
#' This variable has to be a part of the database (db)
#' @param db The database on which we should perform the operations
#' 
#' @return The table of non-missing observations after eliminating the variable
#' to the left. 
#' 
#' @export
#' 
#' @examples
#' which.restricts(c("d_age","b_sex"),"ANAscore",data)

#### Funkcja która mówi ile zyskam obserwacji jak ją zastosuję 
which.restricts <- function(predictors,outcome,db){
  len <- length(predictors)
  obs <- rep(0,len)
  name <- rep(0,len)
  for (i in 1:len){
    obs[i] <- length(na.omit(db[c(outcome,predictors[-i])])[,1])
    name[i] <- predictors[i]
  }
  print(cbind(name,obs))
}