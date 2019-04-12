#' Convert % growth tables to growth/no growth tables
#'
#' This function allows you to convert a list of tables containing %growth from one
#' named list element to a list of tables containing growth/no growth info. The output is a named list
#' 
#' @param namedGrowthList named list element from the output of convertODDataToGrowth()
#' @param growthThreshold threshold above which we consider that growth is achived. Defaults to 0.1 (10%)
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

convertGrowthToBinary <-
  function(namedGrowthList,
           growthThreshold=0.1)
  {
    namedBinaryList <- list()
    #separate the data part from the name part of the list input
    for (i in 1:length(namedGrowthList))
    {
      currentGrowthtable <- namedGrowthList[i]
      growthData <- currentGrowthtable[[1]]
      growthThreshold <- as.numeric(growthThreshold[[1]])
      
      binaryData <- as.data.frame(growthData > growthThreshold)
      
      colnames(binaryData) <- colnames(growthData)
      rownames(binaryData) <- rownames(growthData)
      
      namedBinaryList[[i]] <- as.data.frame(binaryData)
      
    }
    
    names(namedBinaryList) <- names(namedGrowthList)
    return(namedBinaryList)
    
  }
