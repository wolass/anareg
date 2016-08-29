#' Plots a base boxplot with significant multiple comparisons. 
#' 
#' Plot for x- grouping variable and y - continous variable with lines 
#' illustrating significant differences among groups in multiple comparisons 
#' with Dunn's test.  
#' 
#' @param x The grouping variable (of a 'data' data frame) for the box plot 
#' as character to fit scheme: data[,x]
#' 
#' @param y The outcome variable (of a 'data' data frame) for the box plot 
#' as character to fit scheme: data[,y]
#' 
#' @param ylab The label of the y axis
#'  
#' @return The plot with significant multiple comparisons.  
#' @import dunn.test
#' 
#' @export
#' 
#' @examples
#' data<-mtcars
#' data$cyl<-as.factor(data$cyl)
#' all.f("cyl","mpg")
#' 

all.f <-function(x,y,ylab=""){
    if(ylab==""){
        y.lab <- y
    } else {
        y.lab <- ylab
    }
    #boxplot(data[,y]~data[,x],ylab=y,xlab="")
    kruskal.test(data[,y],data[,x])
    #dunn.test(data[,y],data[,x],method="bonferroni")
    dt <- dunn.test(data[,y],data[,x],method="bonferroni")
    ### Split outcome by group
    out.sp <- split(data[,y],data[,x])
    len.vars <- sapply(out.sp,length)
    len.all <- length(len.vars)
    imp.dt<-which(dt$P.adjusted<0.05)
    check.pairs <- function(x){
        if(x==1){
            return(cbind(1,2))
        }else if(x==2){
            return(cbind(1,3))
        }else if(x==3){
            return(cbind(2,3))
        }else if(x==4){
            return(cbind(1,4))
        }else if(x==5){
            return(cbind(2,4))
        }else if(x==6){
            return(cbind(3,4))
        }else if(x==7){
            return(cbind(1,5))
        }else if(x==8){
            return(cbind(2,5))
        }else if(x==9){
            return(cbind(3,5))
        }else if(x==10){
            return(cbind(4,5))
        }else{
            return(NA)
        }
    }
    connections <- sapply(imp.dt,check.pairs)
    n.connections <- length(imp.dt)
    df.con <- list(NULL)
    my <-max(data[,y],na.rm=T)
    
    #make.df <- function(i){
    #   data.frame(a=c(connections[1,i],connections[1,i]:connections[2,i],connections[2,i]),b=c(0.99*my,rep(my,length(connections[1,i]:connections[2,i])),0.99*my),middle=rep((connections[2,i]+connections[1,i])/2,length(connections[1,i]:connections[2,i])+2))
    #}
    
    cord <- NULL
    for(i in 1:n.connections){
        cord <- c(cord,my*1.2-((0+i)*my*0.04))
    }
    
    #coor <- NULL
    #for( i in 1:n.connections){    
    #    coor[[i]]<-make.df(i)
    #    coor[[i]][,2]<-a[[i]][,2]-(0.03*my*(i-1))
    #}
    
    connect<-matrix(rbind(connections,cord),nrow=3)
    
    boxplot(data[,y]~data[,x],ylab=y.lab,xlab="",ylim=c(0,1.2*my))
    #bxp(boxplot(data[,y]~data[,x]),ylim=c(0,1.10*my),ylab=y,xlab="")
    if(!n.connections==0){
        for(i in 1:n.connections){
            segments(connect[1,i],connect[3,i]*0.99,connect[1,i],connect[3,i]) 
            segments(connect[1,i],connect[3,i],connect[2,i],connect[3,i])
            segments(connect[2,i],connect[3,i]*0.99,connect[2,i],connect[3,i])
        }
    } else {
        NULL
    }
    if(!n.connections==0){
        for(i in 1:n.connections){
            text((connect[1,i]+connect[2,i])/2,connect[3,i]*1.02,"*") 
        }
    } else {
        NULL
    }
}
