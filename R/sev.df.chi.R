### Ta funkcja jest ciekawa. 
sev.df_chi <- function(v){
  ldply(v, function(x) {
    test <- chisq.test(factor(sapply(sev.df$q_1621_ever_severe_v4,make.na),labels=c("yes","no")), factor(sapply(sev.df[, x],make.na),labels=c("no","yes")))
    
    out <- data.frame("Row" = colnames(sev.df[x])
                      , "Chi.Square" = round(test$statistic,3)
                      ,  "p.value" = round(test$p.value, 5)
    )
    return(out)
  })  
}