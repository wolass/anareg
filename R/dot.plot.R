dot.plot<-function(x,y,ylab=NA,xlab=NA){
    if(is.na(ylab)==T){
        l.y <- deparse(substitute(y))    
    } else{
        l.y <- ylab
    }
    
    if(is.na(xlab)==T){
        l.x <- deparse(substitute(x))
    } else {
        l.x <- xlab
    }
    print(plot(jitter(as.numeric(x),factor=0.3),y,xaxt="n",ylab=l.y,xlab=l.x)+
              axis(1,at=1:length(levels(x)),labels=levels(x)))
    
}