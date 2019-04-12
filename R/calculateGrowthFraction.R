#' Convert optical denisty into growth fraction
#'
#' This function calculates the % of growth observed in a well using the optical
#' densities measured in that well and in a growth control and a blank well
#'
#' @param wellOpticalDensity optical density measured in the well of interest
#' @param growthControlOpticalDensity optical density measured in the growth
#' control well
#' @param blankOpticalDensity optical density measured in the blank well
#' @param siginifDigits number of significants digits for the growth fraction. Defaults to 3
#' @keywords Optical density, growth fraction
#' @export
#' @examples
#' calculateGrowthFraction(0.2,0.5,0.03)
#' 0.362

calculateGrowthFraction <-
  function (wellOpticalDensity,
            growthControlOpticalDensity,
            blankOpticalDensity,
            siginifDigits=3)
  {
    growthFraction <-
      (wellOpticalDensity - blankOpticalDensity) / (growthControlOpticalDensity -
                                                      blankOpticalDensity)
    return(signif(growthFraction,siginifDigits))
  }
