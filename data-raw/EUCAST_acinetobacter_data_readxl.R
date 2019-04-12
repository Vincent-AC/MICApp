library(readxl)
library(dplyr)
library(usethis)

eucast.breakpoints.acinetobacter <- read_xls("EUCAST_v_7.1_Breakpoint_Tables.xls", 8)

#Keep only the first 3 columns
eucast.breakpoints.acinetobacter <- eucast.breakpoints.acinetobacter[,1:3]

#Change column names
colnames(eucast.breakpoints.acinetobacter) <-
  c("ATB", "S.le", "R.gt")

#Remove NA lines
eucast.breakpoints.acinetobacter <- filter(eucast.breakpoints.acinetobacter,!is.na(R.gt))
#Remove NA lines
eucast.breakpoints.acinetobacter <- filter(eucast.breakpoints.acinetobacter,!is.na(ATB))

#Remove lines without MIC value
eucast.breakpoints.acinetobacter <- filter(eucast.breakpoints.acinetobacter,R.gt != "-")
eucast.breakpoints.acinetobacter <- filter(eucast.breakpoints.acinetobacter,R.gt != "NA")
eucast.breakpoints.acinetobacter <- filter(eucast.breakpoints.acinetobacter,R.gt != "IE")

#Notes are transformed to a number in the ATB name, remove it
eucast.breakpoints.acinetobacter <- mutate(eucast.breakpoints.acinetobacter,
                      ATB = gsub("[[:digit:]]","",eucast.breakpoints.acinetobacter$ATB))
eucast.breakpoints.acinetobacter <- mutate(eucast.breakpoints.acinetobacter,
                                         S.le=as.numeric(S.le),
                                         R.gt=as.numeric(R.gt))

usethis::use_data(eucast.breakpoints.acinetobacter,overwrite=T)
