#tworzy tabelę która zawiera porównanie OR w różnych modelach 
polr.model.var.comp <- function(model,variable){
  mod <- cbind(exp(cbind(OR=coef(model), confint(model))),
               "p.value"=pnorm(abs(coef(summary(model)))[, "t value"],
                               lower.tail=FALSE * 2))
  return(mod[3,])
}