% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitSheetsTecanOutput.R
\name{splitSheetsTecanOutput}
\alias{splitSheetsTecanOutput}
\title{Split the Tecan excel output into different objects}
\usage{
splitSheetsTecanOutput(tecanDataFilename)
}
\arguments{
\item{tecanDataFilename}{path to the excel file}
}
\description{
This function allows you to split the multiple sheets of the Tecana excel
output into different objects and returns a list of objects containing the
sheet name and the excel data
}
\examples{
splitSheetsTecanOutput("MIC-12-10-17.xlsx")
}
\keyword{Tecan}
\keyword{output,}
\keyword{sheets}
\keyword{split,}
