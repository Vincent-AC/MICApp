#' Extracts the OD values from Tecan Sheet data
#'
#' This function allows you to extract the OD values from one list element
#' containing the sheet data and the sheet name as produced by
#' splitSheetsTecanOutput()
#' It returns a list of data.frame() containing OD values read from each
#' excel sheet read with splitSheetsTecanOutput()

#'
#' @param tecanSheet named list element from the output of splitSheetsTecanOutput()
#' @param ODDataStartCol number of the first column of the OD data. Default is 1
#' @param ODDataEndCol number of the last column of the OD data. Default is 13
#' @keywords Tecan output, OD, extraction
#' @export
#' @examples

extractODTecanSheet <-
  function(tecanSheet,
           ODDataStartCol = 1,
           ODDataEndCol = 13)
  {
    namedODList <- NULL
    for (i in 1:length(tecanSheet))
    {
      #separate the data part from the name part of the list input
      tecanSheetData <- tecanSheet[[i]]
      
      #get the line number where the OD data starts
      ODDataStartRow <-
        as.numeric(row.names(tecanSheetData)[(which(tecanSheetData == "<>"))]) #<> is always signalling
      #the OD table
      
      # End Time: is always 4 rows after the last row of the table
      ODDataEndRow <-
        as.numeric(row.names(tecanSheetData)[(which(tecanSheetData == "End Time:"))]) -
        4
      
      extractedODData <- tecanSheetData[ODDataStartRow:ODDataEndRow,
                                        ODDataStartCol:ODDataEndCol]
      
      columnNames <-
        extractedODData[1, ] # First line as column names
      columnNames <- columnNames[-1]
      
      rowNames <- extractedODData[, 1] # First column as row names
      rowNames <- rowNames[-1]
      
      extractedODData <-
        extractedODData[-1, -1] #Remove first column and row as
      #they are row and column names respectively
      
      extractedODData <-
        as.data.frame(sapply(extractedODData, function(x)
          as.numeric(as.character(x)))) #Convert all data to numeric
      
      colnames(extractedODData) <- columnNames
      rownames(extractedODData) <- rowNames
      
      namedODList[[i]] <- extractedODData
      
    }
    names(namedODList) <- names(tecanSheet)
    return(namedODList)
    
  }