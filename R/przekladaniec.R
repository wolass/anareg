przekladaniec <- function(names){
  names <- names(names)
  c <- sapply(names,function(x){
    a<- paste0(x,":no")
    b<- paste0(x,":yes")
    return(c(a,b))
  })
  return(as.vector(c))
}
