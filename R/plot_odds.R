#' Makes a plot of odds ratios from a regression model. 
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


plot_odds <-function(x,title=NULL,desc=""){
  tmp<- data.frame(cbind(exp(coef(x)),exp(confint(x))))
  odds<-tmp[-1,]
  names(odds)<-c("OR","lower","upper")
  require(car)
  odds$vars <- row.names(odds)
  odds$vars <- recode(odds$vars,
                      "'q_423_aceyes'='ACE-I';
                      'b_sexmale'='Male sex';
                      'd_elicitor_groupdrug'='Drugs as elicitors';
                      'q_410_masto_curyes'='Concomitant mastocytosis';
                      'q_421_exercisevigorous'='Vigorous exercise';
                      'q_421_exercisemoderate'='Moderate exercise';
                      'q_425_alcoholyes'='Alcohol abuse';
                      'd_elicitor_groupinsects'='Insects as elicitors';
                      'q_422_stressyes(likely)'='Psychological stress';
                      'q_421_exercisemild'='Mild exercise';
                      'q_423_otheryes'='Other concomitant medication';
                      'q_160_ever_reactyes'='Anaphylaxis episodes in the past';
                      'd_age'='Age';
                      'q_410_asthma_curyes'='Concomitant asthma';
                      'd_elicitor_groupfood'='Food as elicitor';
                      'q_410_ad_curyes'='Concomitant AD';
                      'q_410_thyroid_curyes'='Concomitant thyroid disease';
                      'q_423_betayes'='Betablockers';
                      'q_410_cardio_curyes'='Concomitant cardiologic disease';
                      'q_410_rhinitis_curyes'='Concomitant rhinitis';
                      'q_423_at2yes'='Angiotensin 2 inhibitors';
                      'q_423_asayes'='Acetylsalicylic acid'")
  ticks<-c(seq(.1,0.9,by=.1),seq(1,9,by=1),seq(10,100,by=10))
  more.one <- odds$OR>1
  ggplot(odds,aes(y=OR,x=vars))+
    geom_point()+
    geom_errorbar(aes(ymin=lower,ymax=upper),width=.2)+
    scale_y_log10(breaks=ticks,labels=ticks)+
    # scale_x_discrete(limits=reorder(odds$vars[odds$OR>1],odds$OR[odds$OR>1]))+
    scale_x_discrete(limits=odds$vars[more.one][order(odds$OR[more.one])])+
    geom_hline(yintercept=1,linetype=2)+
    coord_flip()+
    labs(title,x="",y="OR")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
