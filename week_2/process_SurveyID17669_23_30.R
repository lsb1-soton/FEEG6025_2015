# Script to load & process simple survey dataset
# Survey: https://www.isurvey.soton.ac.uk/admin/section_list.php?surveyID=17669
# Survey: https://www.isurvey.soton.ac.uk/17669

# clear the workspace
rm(list=ls())

# change working directory
setwd("~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/")

# input file name
file <- "SurveyID17669_23_30"

# load the data as downloaded from isurvey into a data frame
# it's in week 2
classSurveyDF <- read.csv(paste0("Week 2/", file, ".csv"))

# create a fictional age with replacement (so can have repeat values)
n <- length(classSurveyDF$Participant.ID)
classSurveyDF$age <- sample(seq(21, 35), size = n, replace = TRUE)

# check
hist(classSurveyDF$age, xlab = "Age", main = "Histogram of Age")

# convert the previous course data to be non-disclosive
classSurveyDF$previous_full <- classSurveyDF$What.was.your.first.degree...e.g..MEng.Civil.Engineering........
classSurveyDF$previous_safe <- "Other" # non-disclosive default
classSurveyDF$previous_safe[grep("ngine",classSurveyDF$previous_full)] <- "Engineer"
classSurveyDF$previous_safe[grep("ivil",classSurveyDF$previous_full)] <- "Civil Engineer"
# set to NA if we don't know
classSurveyDF$previous_safe[classSurveyDF$Date.Finished == "Did not finish"] <- NA

table(classSurveyDF$previous_safe)

# make safe
classSurveyDF$previous_full <- NULL
classSurveyDF$What.was.your.first.degree...e.g..MEng.Civil.Engineering........ <- NULL

safeClassSurveyDF <- classSurveyDF[,-grep("X.|Previous.ID|.Order|YOUR|WHAT|FUTURE", colnames(classSurveyDF))]

# save it to week 3
write.csv(safeClassSurveyDF, file = paste0("Data/", file, "_wf.csv"))
