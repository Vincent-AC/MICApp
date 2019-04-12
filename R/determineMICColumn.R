#' Convert growth/no growth tables to MIC column tables
#'
#' This function allows you to convert a list of tables containing containing growth/no growth info from one
#' named list element to a list of tables containing the index of columns conatining the MIC concentration.
#' The output is a named list
#' 
#' @param namedBinaryList named list element from the output of convertGrowthToBinary()
#' @param colGrowthCtl number of the column containing the growth control. Default is 11
#' @param colBrothCtl number of the column containing the broth control. Default is 12
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

determineMICcolumn <- function(namedBinaryList,
                               colGrowthCtl=11,
                               colBrothCtl=12)
{
  namedMICList <- list()
  #separate the data part from the name part of the list input
  for (i in 1:length(namedBinaryList))
  {
    currentBinarytable <- namedBinaryList[i]
    binaryData <- currentBinarytable[[1]]
    maximumColumn <- function(x)
    {     boolean <- which(x == F) 
    if (length(boolean)==0) var <- -1
    else var <- max(boolean)
    }
    MICData <- as.data.frame(apply(binaryData[,-c(colGrowthCtl, colBrothCtl)], 1,maximumColumn
    ))
    
    colnames(MICData) <- "MIC column"
    rownames(MICData) <- rownames(binaryData)
    
    namedMICList[[i]] <- as.data.frame(MICData)
    
  }
  
  names(namedMICList) <- names(namedBinaryList)
  return(namedMICList)
}