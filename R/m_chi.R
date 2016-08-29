#' Quickly measure multiple chi sq tests out of many variables
#' 
#' It takes a vector of numbers referred to the variables you want to check
#' and prints out the chi test values in a data frame.
#' 
#' @param v The vector of character variable names or numbers corresponding to
#'      the said variables
#' @param data Data frame where the variables are to be calculated. 
#' 
#' @return The DATA FRAME of chisq.test values in every configuration of given 
#' set of variables
#' @import plyr
#' @export
#' 
#' @examples
#' multi.chis <- m_chi(c("d_age","b_sex"),data)



m_chi <-function(v,data){
  data.f <- do.call(cbind, lapply(v, function(i) factor(sapply(data[, i],make.na))))
  index <- 1:length(v)
  
  combos <- combn(index,2)
  
  
  
  adply(combos, 2, function(x) {
    
    test <- chisq.test(data.f[, x[1]],data.f[, x[2]])
    
    out <- data.frame("Row" = colnames(data)[v[x[1]]]
                      , "Column" = colnames(data[v[x[2]]])
                      , "Chi.Square" = round(test$statistic,3)
                      ,  "df"= test$parameter
                      ,  "p.value" = round(test$p.value, 5)
    )
    return(out)
    
  })  
}