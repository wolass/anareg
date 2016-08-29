#' Makes ANAscore variable with the given set of predefined variables 
#' 
#' It takes a vector of numbers referred to the variables you want to include. 
#' The variables have to be in the correct order to properly assign weights.
#' the 
#' 
#' @param symptoms The vector of character variable names or numbers 
#' corresponding to the variables in correct order corresponding to proper 
#' weights.
#' @param weights The vector of the same lenght as symptoms, defining the 
#' impact symptom have on the severity of anaphylaxis.
#' @param grades The vector of the same lenght as symptoms, defining the 
#' severity grades of the symptoms. Try not to make more than 5 grades.
#' @param data Data frame where the variables are to be calculated. 
#' 
#' @return The vector of ANAscore values. 
#' 
#' @export
#' 
#' @examples
#' data$ANAscore <- make.anascore()
#MAKING ANAScore3
make.anascore <- function(symptoms=c(17:21,23:29,31:39,41:55,65),
                          weights=c(3,2,1,2,2,3,1,4,3,4,12,3,8,4,1,20,4,2,8,8,1,20,18,20,10,6,18,12,10,18,2,8,3,15,20,6,50),
                          grades=c(1,1,1,1,1,1,1,2,1,2,3,1,2,2,1,4,2,1,2,2,1,4,3,4,2,2,3,3,2,3,1,2,1,3,4,2,5),
                          data=data){
  sympts <- names(data)[symptoms]
  #sym.sev <- c(1,1,1,1,1,3,2,5,3,5,5,3,4,5,2,20,5,1,5,6,1,18,16,20,1,1,5,7,7,5,1,2,1,5,30,1,50)
  #ana.multi <- c(3,2,1,2,2,3,1,4,3,4,12,3,8,4,1,20,4,2,8,8,1,20,18,20,10,6,18,12,10,18,2,8,3,15,20,6,50)
  ana.multi <-weights  
  #cbind(sympts,ana.multi)
  
  #Stating which questions belong to which severity grade:
  #GRADES <- c(1,1,1,1,1,1,1,2,1,2,3,1,2,2,1,4,2,1,2,2,1,4,3,4,2,2,3,3,2,3,1,2,1,3,4,2,5)
  GRADES <- as.factor(grades)
  f1 <- sum(ana.multi[which(GRADES=="1")])
  f2 <- sum(ana.multi[which(GRADES=="2")])
  f3 <- sum(ana.multi[which(GRADES=="3")])
  f4 <- sum(ana.multi[which(GRADES=="4")])
  f5 <- sum(ana.multi[which(GRADES=="5")])
  
  make.na0 <- function(x){
    if(is.na(x)){
      return(1)
    } else if(x=="unknown"|x=="don?t know"){
      return(1)
    } else {
      return(x)
    }
  }
  
  
  temp.df <- rep(0,length(data[,1]))
  for(i in 1:length(symptoms)){
    temp.df <- data.frame(temp.df,(as.numeric(sapply(data[,symptoms[i]],make.na0))-1)*ana.multi[i])
    #names(temp.df)<- c(names(temp.df),paste0(i)) 
  }
  
  temp.all <- data.frame(apply(temp.df[,which(GRADES=="1")+1],2,FUN=function(y){return(y/f1)}),apply(temp.df[,which(GRADES=="2")+1],2,FUN=function(y){return(y/f2*2)}),apply(temp.df[,which(GRADES=="3")+1],2,FUN=function(y){return(y/f3*3)}),apply(temp.df[,which(GRADES=="4")+1],2,FUN=function(y){return(y/f4*4)}),temp.df[,which(GRADES=="5")+1]/f5*5)
  ANAscore3 <- apply(temp.all,1,sum)
  return(ANAscore3)
}