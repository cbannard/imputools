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


get_weighted_descriptives  <- function(variable,weights){

N = length(variable)
M = length(weights)
weightedvariable = variable*weights
totalcount = sum(weights)
av = sum(weightedvariable)/totalcount
SD = sqrt(sum(weights*(variable-av)^2)/((sum(weights)*(M-1))/M))
LOWER=av-(SD/sqrt(N))*1.96
UPPER=av+(SD/sqrt(N))*1.96

print(av)
print(SD)
print(LOWER)
print(UPPER)
}
