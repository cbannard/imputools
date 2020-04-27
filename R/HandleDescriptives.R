#' A function to create a mean, standard deviation and CIs for sample weighted data
#'
#' @usage get_weighted_descriptives(x, y)
#'
#' @param x A vector of values to be summarised. No NAs.
#'
#' @param y A vector of weights for each of the values in x
#'
#' @return A numeric vector containing the weights mean, SD & upper and lower values of 95% interval
#'
#' @examples
#' get_weighted_descriptives(languageability,sampleweights)
#' @export
get_weighted_descriptives  <- function(variable,weights){
descstats=vector()
N = length(variable)
M = length(weights)
weightedvariable = variable*weights
totalcount = sum(weights)
av = sum(weightedvariable)/totalcount
SD = sqrt(sum(weights*(variable-av)^2)/((sum(weights)*(M-1))/M))
LOWER=av-(SD/sqrt(N))*1.96
UPPER=av+(SD/sqrt(N))*1.96

descstats[1] = av
descstats[2] = SD
descstats[3] = LOWER
descstats[4] = UPPER
#print(paste("MEAN: ",av))
#print(paste("SD: ",SD))
#print(paste("95% CI: ",LOWER," ",UPPER))
return(descstats)
}

#' A function to calculate mean, SD and CIs over imputed dataset descriptives
#'
#' @usage combine_imputed_descriptives(dataset)
#'
#' @param dataset data analyses are run on - each column should be a variable of interest and each row an imputed dataset
#'
#' @return  A data frame containing mean, SD and CIs for all imputed datasets
#'
#' @examples
#' combine_imputed_descriptives(dataset)
#' @export
combine_imputed_descriptives <- function(dat){
    combined = matrix(nrow=4,ncol=length(dat));
  combined[1,] = apply(as.matrix(dat), 2, mean)
  combined[2,] = apply(as.matrix(dat), 2, sd)
      CIs = apply(as.matrix(dat), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
      combined[3,] = CIs[1,]
      combined[4,] = CIs[2,]
     combined <- data.frame(combined)
      names(combined) <- names(dat)
return(combined)
}
