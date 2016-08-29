#' Makes a list of odds ratios between levels of two variables. 
#' 
#' It takes a model of a lgistic, linear, ordinal, regression and plots the odds
#' ratios where
#' 
#' @param x The model (formal class) for regression for which to plot odds 
#' ratios
#' 
#' @return The plot of odds ratios  
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @examples
#' plot_odds(regression.model)
#' 
AutomaticOR.proc <- function(x,y,alpha=0.05){
  #
  xtab <- table(x,y)
  n00 <- xtab[1,1]
  n01 <- xtab[1,2]
  n10 <- xtab[2,1]
  n11 <- xtab[2,2]
  #
  rawOR <- (n00*n11)/(n01*n10)
  if (rawOR < 1){
    n01 <- xtab[1,1]
    n00 <- xtab[1,2]
    n11 <- xtab[2,1]
    n10 <- xtab[2,2]
    iLevel <- 2
  }
  else{
    iLevel <- 1
  }
  outList <- vector("list",2)
  outList[[1]] <- paste("Odds ratio between the level [",dimnames(xtab)[[1]][1],"] of the first variable and the level [",dimnames(xtab)[[2]][iLevel],"] of the second variable:",sep=" ")
  outList[[2]] <- oddsratioWald.proc(n00,n01,n10,n11,alpha)
  outList
}