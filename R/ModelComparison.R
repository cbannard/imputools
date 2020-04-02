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

#' A function to compare lrm models estimated over imputed datasets
#'
#' @usage pool.compare.lrm(fit1,fit0,data,method = c("wald", "likelihood"))
#'
#' @param model1
#'
#' @param model0
#'
#' @param dataset dataset analyses are run on
#'
#' @param method method used for comparison - wald or likelihood
#'
#' @return  A list containing Dm, dfs and pvalue
#'
#' @examples
#' Mh1_sampleweight_lrm <- with(imputed_mcs2, lrm(recoded_mh_d1 ~ standardised_bw + standardised_gest + sex_combine + ethnic_combine + new_langcombine + highested + standardised_wealth, weights = weight_combine))
#' Mh2_sampleweight_lrm <- with(imputed_mcs2, lrm(recoded_mh_d1 ~ standardised_bw + standardised_gest + sex_combine + ethnic_combine + new_langcombine + highested + standardised_wealth + teen_mum2 +  ms_combine + standardised_maternal_mh + standardised_conduct + standardised_emotional,weights = weight_combine))
#' pool.compare.lrm(Mh2_sampleweight_lrm,Mh1_sampleweight_lrm,imputed_mcs2)
#' @export
pool.compare.lrm <- function(fit1,fit0,data,method = c("wald", "likelihood")){

 method <- match.arg(method)

 m = length(fit1$analyses)

 est1_lrm = pool.lrm(fit1,nrow(data$data))
 est0_lrm = pool.lrm(fit0,nrow(data$data))
 dimQ1 <- length(est1_lrm$pooled$estimate)
 dimQ2 <- dimQ1 - length(est0_lrm$pooled$estimate)
 formula1 <- formula(getfit(fit1, 1L))
 formula0 <- formula(getfit(fit0, 1L))
 vars1 <- row.names(est1_lrm$pooled)
 vars0 <- row.names(est0_lrm$pooled)


 if (is.null(vars1) || is.null(vars0))
   stop("coefficients do not have names", call. = FALSE)
 if (dimQ2 < 1L)
   stop("Model 'fit1' not larger than 'fit0'", call. = FALSE)
 if (!setequal(vars0, intersect(vars0, vars1)))
   stop("Model 'fit0' not contained in 'fit1'", call. = FALSE)
 if (method == "wald") {
   Q <- diag(dimQ1)
   where_new_vars = which(!(vars1 %in% vars0))
   Q <- Q[where_new_vars, , drop = FALSE]
   qbar <- Q %*% est1_lrm$pooled$estimate
   Ubar <- Q %*% diag(est1_lrm$pooled$ubar) %*% (t(Q))
   Bm <- Q %*% diag(est1_lrm$pooled$b) %*% (t(Q))
   rm <- (1 + 1/m) * sum(Bm %*% (solve(Ubar)))/dimQ2
   Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar/(dimQ2 * (1 + rm))
   deviances <- NULL
 }

 if (method == "likelihood"){

   dev1.M = vector()
   dev1.L = vector()
   qbar1 <- est1_lrm$pooled$estimate
   for(i in 1:m){
     dev1.M[i] = fit1$analyses[[i]]$deviance[2]
     mod=fit1$analyses[[i]]
     X = as.data.frame(mod$x)
     nr=length(X[,1])
     coefmat = matrix(rep(qbar1[2:length(qbar1)],each=nr),nrow=nr)
     dev1.L[i]=lrm.fit(y=mod$y, offset=rowSums(coefmat*X),maxit=10000)$deviance[2]


   }

   dev0.M = vector()
   dev0.L = vector()
   qbar0 <- est0_lrm$pooled$estimate
   for(i in 1:m){
     dev0.M[i] = fit0$analyses[[i]]$deviance[2]
     mod=fit0$analyses[[i]]
     X = as.data.frame(mod$x)
     nr=length(X[,1])
     coefmat = matrix(rep(qbar0[2:length(qbar0)],each=nr),nrow=nr)
     dev0.L[i]=lrm.fit(y=mod$y, offset=rowSums(coefmat*X),maxit=10000)$deviance[2]

   }

   dev.M <- mean(dev0.M - dev1.M)
   dev.L <- mean(dev0.L - dev1.L)
   rm <- ((m + 1)/(dimQ2 * (m - 1))) * (dev.M - dev.L)
   Dm <- dev.L/(dimQ2 * (1 + rm))
 }

 v <- dimQ2 * (m - 1)

 if (v > 4){
   w <- 4 + (v - 4) * ((1 + (1 - 2/v) * (1/rm))^2)
 } else {
   w <- v * (1 + 1/dimQ2) * ((1 + 1/rm)^2)/2
 }

 statistic <- list(Dm=Dm,df1 = dimQ2,df2 = w,pvalue=1-pf(Dm, dimQ2, w))

 return(statistic)

}
