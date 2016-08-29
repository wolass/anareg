#' Calculates the odds ratios for the predictors of regression models.
#' 
#' This function plots a table of odds ratios with 95% CI and p values for the 
#' predictors in a regression model. 
#' 
#' @param model The regression class model. 
#' @param digits The number of digits after 0 to print in the data frame.
#' 
#' @return The table of odds ratios with confidence intervals and p values, in 
#' character format for easier printing by the xtable.
#' 
#' @export
#' 
#' @examples
#' xtable(which.restricts(c("d_age","b_sex"),"ANAscore",data))


#odds.ratios out of model
show.odds <- function(model,digits){
  ctableb <- data.frame(coef(summary(model)))
  q        <- pnorm(abs(ctableb[, ifelse("t.value" %in% names(ctableb),which(names(ctableb)=="t.value"),which(names(ctableb)=="z.value"))]), lower.tail=FALSE) * 2
  (ctableb <- cbind(ctableb, "p value"=q))
  (cib <- confint.default(model)) 
  #confint.default(m.mu)  
  
  r<-cbind(exp(cbind(OR=coef(model), cib)), "p value"=q)
  r<-round(r,digits)
  r[,"p value"]<-sapply(r[,"p value"],function(x){ifelse(x<0.0001,"<0.0001",x)})
  return(r)
}