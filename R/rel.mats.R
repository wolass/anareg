rel.mats <-function(groupings,outcome){
  x <- lapply(groupings,FUN=function(x){rel.mat(x,outcome)})
  x2 <- data.frame(matrix(unlist(x),nrow=length(levels(outcome)),))
  row.names(x2) <- row.names(x[[1]])
  names(x2) <-przekladaniec(groupings)
  return(as.matrix(x2))
}