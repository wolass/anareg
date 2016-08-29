TableOR.proc <- function(x,y,alpha=0.05){
  #
  xtab <- table(x,y)
  n00 <- xtab[1,1]
  n01 <- xtab[1,2]
  n10 <- xtab[2,1]
  n11 <- xtab[2,2]
  #
  outList <- vector("list",2)
  outList[[1]] <- paste("Odds ratio between the level [",dimnames(xtab)[[1]][1],"] of the first variable and the level [",dimnames(xtab)[[2]][1],"] of the second variable:",sep=" ")
  outList[[2]] <- oddsratioWald.proc(n00,n01,n10,n11,alpha)
  outList
}