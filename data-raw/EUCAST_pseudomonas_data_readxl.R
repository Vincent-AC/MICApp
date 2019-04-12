library(readxl)
library(dplyr)
library(usethis)

eucast.breakpoints.pseudomonas <- read_xls("EUCAST_v_7.1_Breakpoint_Tables.xls", 6)

#Keep only the first 3 columns
eucast.breakpoints.pseudomonas <- eucast.breakpoints.pseudomonas[,1:3]

#Change column names
colnames(eucast.breakpoints.pseudomonas) <-
  c("ATB", "S.le", "R.gt")

#Remove NA lines
eucast.breakpoints.pseudomonas <- filter(eucast.breakpoints.pseudomonas,!is.na(R.gt))
#Remove NA lines
eucast.breakpoints.pseudomonas <- filter(eucast.breakpoints.pseudomonas,!is.na(ATB))

#Remove lines without MIC value
eucast.breakpoints.pseudomonas <- filter(eucast.breakpoints.pseudomonas,R.gt != "-")
eucast.breakpoints.pseudomonas <- filter(eucast.breakpoints.pseudomonas,R.gt != "NA")
eucast.breakpoints.pseudomonas <- filter(eucast.breakpoints.pseudomonas,R.gt != "IE")

#Notes are transformed to a number in the ATB name, remove it
eucast.breakpoints.pseudomonas <- mutate(eucast.breakpoints.pseudomonas,
                                           ATB = gsub("[[:digit:]]","",eucast.breakpoints.pseudomonas$ATB))

#Two ATB names include ", P. aeruginosa" string remove it because it isn't necessary
eucast.breakpoints.pseudomonas <- mutate(eucast.breakpoints.pseudomonas,
                                         ATB = gsub(", P. aeruginosa","",eucast.breakpoints.pseudomonas$ATB))

eucast.breakpoints.pseudomonas <- mutate(eucast.breakpoints.pseudomonas,
                                                S.le=as.numeric(S.le),
                                                R.gt=as.numeric(R.gt))

usethis::use_data(eucast.breakpoints.pseudomonas,overwrite=T)
