#' Make complete cases data base for AIC analysis. 
#' 
#' This function makes a list with three values FORMULA, TRAIN data set and 
#' TEST data set.  Very useful for regression modelling.
#' 
#' @param outcome is the character string specifying the outcome variable. 
#' This variable has to be a part of the database (db)
#' @param predictors Predictors are the known variables, based on which
#' we want to conduct our predicting. 
#' @param db The database on which we should perform the operations
#' @param single.set is a logical if TRUE then the function will provide only 
#' one training set, if FALSE if will return both training set and a test set. 
#' 
#' @export
#' 
#' @examples
#' complete.cases <- cc.restrictor("ANAscore",c("d_age","b_sex"),data)


cc.restrictor <- function(outcome,predictors,db,single.set=F){
  dat <-na.omit(db[,c(outcome,predictors)])
  set.seed(60439)
  if(single.set==T){
    train <- dat
    test <-NULL
  } else {
    sub <- sample(1:length(dat[,1]),size = length(dat[,1])*0.5,replace = F)
    train <- dat[sub,]
    test <- dat[-sub,]
  }
  formula <- as.formula(paste0(outcome,"~",paste0(predictors,collapse="+")))
  return(list(formula=formula,train=train,test=test))
}