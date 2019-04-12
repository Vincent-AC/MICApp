#' Convert plate plans and MIC columns index to MIC values
#'
#' This function allows you to convert a list of tables containing the index of columns conatining
#' the MIC concentration and a list of plate plans to a list of tables containing MIC values
#' The output is a named list
#'
#' @param MICColumns named list of data frames containing the columns index of MIC
#' @param platePlans named list of data frames containing plate plans
#' @param plateOptions named list of data frames containing plate options
#' @keywords Tecan output, OD, growth percentage
#' @export
#' @examples

createMICTables <- function(MICColumns,platePlans,plateOptions)
{
  MICResultTables <- list()
  for (j in 1:length(MICColumns))
  {
    selectedMICColumn <- MICColumns[j]
    MICColumnData <- selectedMICColumn[[1]]
    selectedPlatePlan <- platePlans[j]
    platePlanData <- selectedPlatePlan[[1]]
    selectedPlateOptions <- plateOptions[j]
    plateOptionsData <- selectedPlateOptions[[1]]
    selectedMICResult <- plateOptionsData[,c("Species","Strain","ATB")]
    for (i in 1:nrow(MICColumnData))
    {
      if(MICColumnData[i,"MIC column"]==-1)
      {
        selectedMICResult[i,"MIC"] <- paste0(">",platePlanData[i,1])
      }
      else
      {
        selectedMICResult[i,"MIC"] <- platePlanData[i,MICColumnData[i,"MIC column"]]
      }
    }
    rownames(selectedMICResult)[1:nrow(plateOptionsData)] <- LETTERS[1:nrow(plateOptionsData)]
    MICResultTables[[j]] <- selectedMICResult
  }
  names(MICResultTables) <- names(platePlans)
  return(MICResultTables)
}