# Script to load & process simple survey dataset on lecture theatre comfort
# Survey: https://www.isurvey.soton.ac.uk/admin/section_list.php?surveyID=17792
# Survey: https://www.isurvey.soton.ac.uk/17792

# clear the workspace
rm(list=ls())

# change working directory
setwd("~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/")

# input file name
file <- "SurveyID17792_13_10_2015"

# load the data as downloaded from isurvey into a data frame
# it's in week 2
classSurveyDF <- read.csv(paste0("Week 2/", file, ".csv"))

# convert the country of origin data to be non-disclosive
table(classSurveyDF$X8) # origin
table(classSurveyDF$X9)  # Where have you been living for the last 10 years

# Only China & UK >= 5
# car package -> recode
classSurveyDF$countryo <- "Other"
classSurveyDF$countryo[classSurveyDF$X8 == "China"] <- "China"
classSurveyDF$countryo[classSurveyDF$X8 == "United Kingdom"] <- "United Kingdom"
classSurveyDF$countryo[classSurveyDF$X8 == ""] <- NA
classSurveyDF$countryo <- as.factor(classSurveyDF$countryo)
# check
table(classSurveyDF$countryo)

# Only China & UK >= 5
# car package -> recode
classSurveyDF$countryr <- "Other"
classSurveyDF$countryr[classSurveyDF$X9 == "China"] <- "China"
classSurveyDF$countryr[classSurveyDF$X9 == "United Kingdom"] <- "United Kingdom"
classSurveyDF$countryr[classSurveyDF$X9 == ""] <- NA
classSurveyDF$countryr <- as.factor(classSurveyDF$countryr)
# check
table(classSurveyDF$countryr)

table(classSurveyDF$countryo, classSurveyDF$countryr) 
# notice that there is one individual who came form China but has ben in UK for last 10 years
# disclosive - we could identify that person if we knew something about the class.

# make safe
classSurveyDF$X8 <- NULL
classSurveyDF$X9 <- NULL

# save it to data
write.csv(classSurveyDF, file = paste0("Data/", file, "_thermal_wf.csv"))
