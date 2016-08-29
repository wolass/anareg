rel.mat <- function(grouping,outcome){
  l.g <- levels(grouping)
  l.o <- levels(outcome) 
  no <- sapply(l.o,function(x){
    length(which(outcome[grouping==l.g[1]]==x))
  })
    
  yes <- sapply(l.o,function(x){
    length(which(outcome[grouping==l.g[2]]==x))
  })
  s.n <- no/sum(no)
  s.y <- yes/sum(yes) 
  
  x <- data.frame(s.n[order(4:1)],s.y[order(4:1)])
  names(x) <- c(l.g[1],l.g[2])
  
  return(x)
}
