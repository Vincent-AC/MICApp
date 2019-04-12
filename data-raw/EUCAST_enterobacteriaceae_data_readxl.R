library(readxl)
library(dplyr)
library(usethis)

eucast.breakpoints.enterobacteriaceae <- read_xls("EUCAST_v_7.1_Breakpoint_Tables.xls", 5)

#Keep only the first 3 columns
eucast.breakpoints.enterobacteriaceae <- eucast.breakpoints.enterobacteriaceae[,1:3]

#Change column names
colnames(eucast.breakpoints.enterobacteriaceae) <-
  c("ATB", "S.le", "R.gt")

#Remove NA lines
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,!is.na(R.gt))
#Remove NA lines
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,!is.na(ATB))

#Remove lines without MIC value
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,R.gt != "-")
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,R.gt != "NA")
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,R.gt != "IE")

#Notes are transformed to a number in the ATB name, remove it
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                         ATB = gsub("[[:digit:]]","",eucast.breakpoints.enterobacteriaceae$ATB))

#Temocillin has no breakpoint so remove :
eucast.breakpoints.enterobacteriaceae <- filter(eucast.breakpoints.enterobacteriaceae,
                                                ATB!="Temocillin")

#A lot of MIC values have notes attached to them, remove the note numbers
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("81","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("81","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("8,2","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("8,2","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("82","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("82","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("8,3","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("8,3","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("83","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("83","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("321,3","32",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("321,3","32",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("323","32",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("323","32",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("84","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("84","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("164","16",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("164","16",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("163","16",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("163","16",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("86","8",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("86","8",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("14","1",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("14","1",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("12","1",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("12","1",eucast.breakpoints.enterobacteriaceae$R.gt))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=gsub("22","2",eucast.breakpoints.enterobacteriaceae$S.le),
                                                R.gt=gsub("22","2",eucast.breakpoints.enterobacteriaceae$R.gt))

#Remove specificites in ATB name
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                ATB=gsub(" \\(uncomplicated UTI only\\)","",eucast.breakpoints.enterobacteriaceae$ATB))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                ATB=gsub(" iv","",eucast.breakpoints.enterobacteriaceae$ATB))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                ATB=gsub(",.*","",eucast.breakpoints.enterobacteriaceae$ATB))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                ATB=gsub("\nE\\. coli","",eucast.breakpoints.enterobacteriaceae$ATB))
eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                ATB=gsub(" \\(UTI only\\)","",eucast.breakpoints.enterobacteriaceae$ATB))


#Add specificity for Amox-clavulanic acid because it changes the cutoff
eucast.breakpoints.enterobacteriaceae[5,"ATB"] <- "Amoxicillin-clavulanic acid (uncomplicated UTI only)"


#Some ATB have 2 lines for 2 routes of administration but the same cutoff for the two
#Remove
eucast.breakpoints.enterobacteriaceae <- eucast.breakpoints.enterobacteriaceae[-c(45,25),]

eucast.breakpoints.enterobacteriaceae <- mutate(eucast.breakpoints.enterobacteriaceae,
                                                S.le=as.numeric(S.le),
                                                R.gt=as.numeric(R.gt))

usethis::use_data(eucast.breakpoints.enterobacteriaceae,overwrite=T)