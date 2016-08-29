rel.heatmap <- function(x){
  lmat <- rbind( c(0,3,0), c(2,1,4) )
   lhei <- c(0.1, 1)
   lwid <- c(0.1, 1,0.1)
  heatmap.2(as.matrix(x),dendrogram = "none",key=F,col=colorRampPalette(c("white", "black"))(50), reorderfun =function(x){x},Rowv=F,Colv=F,lhei=lhei,lwid=lwid,lmat=lmat,margins=c(13,3),scale="none",trace='none',distfun=function(x){x},colsep=seq(2, ncol(x),by=2), sepcolor="cyan",)
}
