#' A function to calculate mean, standard deviation and CIs for model fit stats across imputed datasets
#'
#' @usage get_stats_on_fit_from_MI(x, y)
#'
#' @param x A mids object
#'
#' @param y Model to be evaluated as a string
#'
#' @return A numeric vector containing the weights mean, SD & upper and lower values of 95% interval for deviance and AIC
#'
#' @examples
#' get_stats_on_fit_from_MI(mi.res,"age5_standardised ~ gender")
#' @export
get_stats_on_fit_from_MI <- function(miobj,modelasstring){
AICvalues=vector()
Deviancevalues=vector()
values_to_return=data.frame()
for(i in 1:miobj$m){
dataset = mice::complete(miobj,i)
m1 = paste("-2*logLik(lm(",modelasstring,",data=dataset))",sep="")
Deviancevalues[i] = eval(parse(text=m1))
m2 = paste("AIC(lm(",modelasstring,",data=dataset))",sep="")
AICvalues[i] = eval(parse(text=m2))

}
values_to_return[1,1]=mean(Deviancevalues)
values_to_return[2,1]=sd(Deviancevalues)
values_to_return[3,1]=mean(Deviancevalues)-sd(AICvalues)/sqrt(length(Deviancevalues))*1.96
values_to_return[4,1]=mean(Deviancevalues)+sd(AICvalues)/sqrt(length(Deviancevalues))*1.96
values_to_return[1,2]=mean(AICvalues)
values_to_return[2,2]=sd(AICvalues)
values_to_return[3,2]=mean(AICvalues)-sd(AICvalues)/sqrt(length(AICvalues))*1.96
values_to_return[4,2]=mean(AICvalues)+sd(AICvalues)/sqrt(length(AICvalues))*1.96
row.names(values_to_return) <- c("Mean","SD","Upper","Lower")
names(values_to_return) <- c("Deviance","AIC")
return(values_to_return)
}
