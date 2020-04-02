#' A function to pool model output from across multiple imputed datasets
#'
#' @usage pool.lrm(models,totalsamplesize)
#'
#' @param models Fitted models
#'
#' @param totalsamplesize samples size for datasets used
#'
#' @return pooled coefficients etc for models
#'
#
#' @examples
#' Mh1_sampleweight_lrm <- with(imputed_mcs2, lrm(recoded_mh_d1 ~ standardised_bw + standardised_gest + sex_combine + ethnic_combine + new_langcombine + highested + standardised_wealth, weights = weight_combine))
#' pool.lrm(Mh1_sampleweight_lrm,length(imputed_mcs2$data[,1]))
#' @export
pool.lrm <- function(models,totalsamplesize){

analysis_set = models$analyses
preds <- names(models$analyses[[1]]$coefficients)

est = data.frame()

for(i in 1:length(analysis_set)){
analysis = analysis_set[[i]]
est[1:length(analysis$coefficients),i] = as.vector(analysis$coefficients)
}

vars = data.frame()

for(i in 1:length(analysis_set)){
analysis = analysis_set[[i]]
vars[1:length(analysis$coefficients),i] = diag(vcov(analysis));

}

pooled = data.frame()
for(i in 1:length(est[,1])){
inf = pool.scalar(t(est[i,]),t(vars[i,]),n=totalsamplesize,k=nrow(est))
pooled[i,1] = round(as.vector(inf$qbar),9)
pooled[i,2] = round(as.vector(sqrt(inf$t)),2)
pooled[i,3] = round(as.vector(inf$qbar/sqrt(inf$t)),2)
pooled[i,4] = round(as.vector(inf$df),2)

if(inf$qbar >= 0){
pooled[i,5] = round(pt(as.vector(inf$qbar/sqrt(inf$t)),df=as.vector(inf$df),lower.tail=FALSE)*2,2)
}
else{
pooled[i,5] = round(pt(as.vector(inf$qbar/sqrt(inf$t)),df=as.vector(inf$df),lower.tail=TRUE)*2,2)
}
pooled[i,6] = round(exp(as.vector(inf$qbar)-(sqrt(as.vector(inf$t))*1.96)),2)
pooled[i,7] = round(exp(as.vector(inf$qbar)+(sqrt(as.vector(inf$t))*1.96)),2)
pooled[i,8] = as.vector(inf$ubar)
pooled[i,9] = as.vector(inf$b)

}

names(pooled) <- c("estimate","std.error","statistic","df","p-value","2.5%","97.5%","ubar","b")
row.names(pooled) <- preds

return(list("pooled" = pooled))
}
