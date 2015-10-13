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
table(classSurveyDF$X8)
table(classSurveyDF$X9)
classSurveyDF$countryo_full <- classSurveyDF$X8 # origin
classSurveyDF$countryr_full <- classSurveyDF$X9 # Where have you been living for the last 10 years

# Only China & UK >= 5
# car package -> recode
classSurveyDF$countryo <- ifelse(grepl("China", 
              classSurveyDF$X8),"China", "Not China"); 
classSurveyDF$countryo <- ifelse(grepl("United Kingdom", 
              classSurveyDF$X8),"United Kingdom", "Other")
# set to NA if we don't know
classSurveyDF$previous_safe[classSurveyDF$Date.Finished == "Did not finish"] <- NA

if (classSurveyDF$X8[])
  
# make safe
classSurveyDF$previous_full <- NULL
classSurveyDF$What.was.your.first.degree...e.g..MEng.Civil.Engineering........ <- NULL

safeClassSurveyDF <- classSurveyDF[,-grep("X.|Previous.ID|.Order|YOUR|WHAT|FUTURE", colnames(classSurveyDF))]

# save it to week 3
write.csv(safeClassSurveyDF, file = paste0("Data/", file, "_wf.csv"))
