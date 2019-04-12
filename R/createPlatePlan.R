#' Updates an already existing plate plan
#'
#' This function allows you to update an existing plate plan using the user's input
#' Useful to recalculate concentrations especially

#'
#' @param inputTable data.frame containing the plate options
#' @param colGrowthCtl number of the column containing the growth control. Default is 11
#' @param colBrothCtl number of the column containing the broth control. Default is 12
#' @param nlines number of lines to read. Default is 8
#' @keywords Plate plan
#' @export
#' @examples

createPlatePlan <- function(inputTable,
                            colGrowthCtl=11,colBrothCtl=12,
                            nlines=8)
{
  i <- 1
  platePlan <- data.frame()
  inputTable<-inputTable[1:nlines,]
  maxConc <- unlist(inputTable["maxConc"])
  dilStep <- unlist(inputTable["dilStep"])
  numberOfConc <- unlist(inputTable["numberOfConc"])
  for (i in 1:length(maxConc))
  {
    currentline <- concSerie(maxConc[[i]],dilStep[[i]],numberOfConc[[i]])
    platePlan <- rbind(platePlan,currentline)
  }
  platePlan[,colGrowthCtl] <- 0
  platePlan[,colBrothCtl] <- "Broth Ctl"
  row.names(platePlan) <- LETTERS[1:nlines]
  colnames(platePlan)[1:12] <- 1:12
  platePlan[,"Strain"] <- inputTable["Strain"]
  platePlan[,"ATB"] <- inputTable["ATB"]
  platePlan[,"maxConc"] <- inputTable["maxConc"]
  platePlan[,"dilStep"] <- inputTable["dilStep"]
  platePlan[,"numberOfConc"] <- inputTable["numberOfConc"]

  platePlan <- platePlan[,c(as.character(1:12))]
  return(platePlan)
}