#' Add MIC values to Growth results
#'
#' This function allows you to convert a table containing the OD values from one
#' named list element to a table containing %growth. The output is a named list
#' of length 1
#'
#' @param namedGrowthList named list element from the output of convertODDataToGrowth()
#' @inheritParams calculateGrowthFraction
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

addMICToGrowth <-
  function(namedGrowthList,
           growthThreshold = 0.1,
           growthControlCol = 11,
           blankCol = 12,
           maxConc,
           dilutionStep)
  {
    #separate the data part from the name part of the list input
    growthData <- namedGrowthList[[1]]
    
    binaryData <- convertGrowthToBinary(namedGrowthList,
                                        growthThreshold = 0.1)[[1]]
    
    
    growthDataWithMICResults <-
      cbind(growthData,
            apply(binaryData[,-c(growthControlCol, blankCol)], 1, function(x)
              max(which(x == F))),
            maxConc,
            dilutionStep)
    
    names(growthDataWithMICResults)[(blankCol + 1):ncol(growthDataWithMICResults)] <-
      c("MICCol", "maxConc", "dilutionStep")
    
    growthDataWithMICResults[, "MIC"] <-
      apply(growthDataWithMICResults, 1, function(x)
        concSerie(x$maxConc,
                  x$dilutionStep,
                  length(x) - 5)[x$MICCol])
    
    
    # apply(growthDataWithMICResults,1,concSerie(x$maxConc,
    #           x$dilutionStep,
    #           ncol(x[, -c(growthControlCol, blankCol)]))[x$MICCol])
    
    return(growthDataWithMICResults)
  }
