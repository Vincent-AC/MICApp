#' Convert OD tables to % growth tables
#'
#' This function allows you to convert a list of tables containing the OD values from one
#' named list element to a table containing %growth. The output is a named list
#' 
#' @param namedODdata named list element from the output of extractODTecanSheet()
#' @param growthControlCol number of the column containing the growth control. Default is 11
#' @param blankCol number of the column containing the broth control. Default is 12
#' @param siginifDigits number of significants digits for the growth fraction. Defaults to 3
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

convertODDataToGrowth <-
  function(namedODList,
           growthControlCol=11,
           blankCol=12,
           siginifDigits=3)
  {
    namedGrowthList <- list()
    #separate the data part from the name part of the list input
    for (i in 1:length(namedODList))
    {
      currentODtable <- namedODList[i]
      ODData <- currentODtable[[1]]
      
      growthData <- as.data.frame(t(sapply(
        1:nrow(ODData),
        FUN = function(x)
          calculateGrowthFraction(ODData[x,],
                                  ODData[x, growthControlCol],
                                  ODData[x, blankCol],
                                  siginifDigits)
      )))
      
      colnames(growthData) <- colnames(ODData)
      rownames(growthData) <- rownames(ODData)
      
      namedGrowthList[[i]] <- as.data.frame(growthData)
      
    }
    
    names(namedGrowthList) <- names(namedODList)
    return(namedGrowthList)
    
  }