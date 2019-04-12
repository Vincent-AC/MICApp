#' Calculate geometric serie of concentrations
#'
#' This function allows you to calculate the drug concentration present in each
#' well of a line by inputing the concentration of the most concentrated well
#' the number of wells in the line and the dilution step
#' 
#' @param maxConc drug concentration in the most concentrated well
#' @param dilutionStep dilution step defaults to 2
#' @param numberOfConc number of wells, defaults to 10
#' @inheritParams calculateGrowthFraction
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

concSerie <- function(maxConc,dilutionStep=2,numberOfConc=10)
{
  serie <- NULL
  for (i in 1:numberOfConc)
  {
    serie[i] <- maxConc / dilutionStep^(numberOfConc-i)
  }
  
  revSerie <- rev(serie)
  return(revSerie)
  
}
