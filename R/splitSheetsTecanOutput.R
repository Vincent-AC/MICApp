#' Split the Tecan excel output into different objects
#'
#' This function allows you to split the multiple sheets of the Tecana excel
#' output into different objects and returns a list of objects containing the
#' sheet name and the excel data
#'
#' @param tecanDataFilename path to the excel file
#' @keywords Tecan output, split, sheets
#' @export
#' @import readxl
#' @examples
#' splitSheetsTecanOutput("MIC-12-10-17.xlsx")
splitSheetsTecanOutput <-
  function(tecanDataFilename)
  {
    library(readxl)
    sheetNames <- excel_sheets(tecanDataFilename)
    
    sheetDataList <- list()
    for (i in 1:length(sheetNames))
    {
      sheetDataList[[i]] <- as.data.frame(read_excel(
        tecanDataFilename,
        col_names = F,
        sheet = sheetNames[i]
      ))
      names(sheetDataList)[[i]] <- sheetNames[i]
      ###Added to remove empty excel sheets that are often added by TECAN at the end of file
      if (ncol(sheetDataList[[i]]) + nrow(sheetDataList[[i]]) == 0)
        sheetDataList[[i]] <- NULL
    }
    return(sheetDataList)
  }