# Meta ----
# Script to load & process simple survey dataset
# Survey admin: https://www.isurvey.soton.ac.uk/admin/section_list.php?surveyID=17669
# Survey: https://www.isurvey.soton.ac.uk/17669

# code by: b.anderson@soton.ak.uk (@dataknut) with help from lsb1@soton.ac.uk

### Housekeeping  ------------------------

# clear the workspace
rm(list=ls())

# where is the default working directory?
getwd()

# set location of data
# you will need to change this!
dpath <- "~/UoS One Drive/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Data"

# set location of results (if any)
# you will need to change this!
rpath <- "~/UoS One Drive/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Week 3"

setwd(dpath)

# input file name
# this is the one we downloaded & slightly processed
file <- "SurveyID17669_23_30_initial_wf"

### Functions ----

### Luke's finish time conversion
convertiSurveyFinishTime <- function(iSurveyFinishTime) {
  # lapply applies function(x) to every list item; this deals with character(0) results etc.
  as.POSIXct(unlist(lapply(iSurveyFinishTime,
                           function(x)
                             ifelse(
                               grepl(
                                 "[[:digit:]]{1,2}[a-z]{2} [A-Z][a-z]{2} [[:digit:]]{4}[[:blank:]][[:digit:]]{1,2}[:][[:digit:]]{2} am|pm",x
                               ),
                               paste(
                                 regmatches(x,regexpr("[[:digit:]]{4}",x)), # year
                                 regmatches(x,regexpr("[A-Z][a-z]{2}",x)), # month
                                 regmatches(x,regexpr(
                                   "[[:digit:]]{1,2}(?=[a-z]{2})",x,perl = TRUE
                                 )), # day
                                 regmatches(x,regexpr("[[:digit:]]{1,2}[:][[:digit:]]{2}",x)), # 12-hour
                                 regmatches(x,regexpr("am|pm",x)),
                                 sep = " "
                               ),
                               NA
                             ))), tz = "", "%Y %b %e %I:%M %p")
}

### Load & examine data  ------------------------

# load the data into a data frame
classSurveyDF <- read.csv(paste0(file, ".csv"))

# change the working directory once loaded in case we want to save out any results
setwd(rpath)

# variables?
names(classSurveyDF)
# very helpful column (variable) names!

# how many columns & rows?
dim(classSurveyDF)

# when did most people complete the survey?
table(classSurveyDF$Date.Finished)
# not very helpful!

# how does the survey record time taken?
table(classSurveyDF$Total.Time.Taken)
# very badly!

### Clean and process  ------------------------

# these variable names are a bit of a pain
# create a new data frame with cleaned up names
cleanClassSurvey <- data.frame(classSurveyDF$Participant.ID)
cleanClassSurvey$previous <- classSurveyDF$What.was.your.first.degree...e.g..MEng.Civil.Engineering........
cleanClassSurvey$enjoy <- classSurveyDF$On.a.scale.of.1.to.5..how.much.did.you.enjoy.your.degree.........
cleanClassSurvey$stats <- classSurveyDF$On.a.scale.of.1.to.5.how.would.you.rate.your.statistical.knowledge........
cleanClassSurvey$middle <- classSurveyDF$Which.of.these.gives.the.middle.value.of.a.distribution........
cleanClassSurvey$linear <- classSurveyDF$Have.you.ever.used.linear.regression........
cleanClassSurvey$tv <- classSurveyDF$How.often.do.you.leave.your.TV.on.standby.for.the.night........
cleanClassSurvey$cold <- classSurveyDF$How.often.do.you.put.more.clothes.on.when.you.feel.cold.rather.than.putting.the.heating.on.or.turning.it.up........
cleanClassSurvey$env <- classSurveyDF$I.am.environmentally.friendly.in.all.I.do
cleanClassSurvey$energy <- classSurveyDF$I.try.to.save.energy.all.the.time
cleanClassSurvey$climate <- classSurveyDF$Climate.change.is.irrelevant
cleanClassSurvey$trans <- classSurveyDF$I.try.to.walk.cycle.or.use.public.transport.rather.than.use.a.car
cleanClassSurvey$fly <- classSurveyDF$Flying.is.evil
cleanClassSurvey$future <- classSurveyDF$What.do.you.want.to.do.after.your.Masters.course........
cleanClassSurvey$improve <- classSurveyDF$In.what.ways.could.this.survey.be.improved........
cleanClassSurvey$age <- classSurveyDF$age
cleanClassSurvey$previous_safe <- classSurveyDF$previous_safe

# library(reshape) & rename would be a better way!

# add some new tidy derived variables

# create a nice duration by extracting & combining minutes & seconds (do this step by step to be clear)
classSurveyDF$duration_split <- strsplit(as.character(classSurveyDF$Total.Time.Taken), " ")
# minutes will be the first item in the split list
classSurveyDF$duration_mins <- sapply(classSurveyDF$duration_split,"[[", 1)
# seconds is the 3rd
classSurveyDF$duration_secst <- sapply(classSurveyDF$duration_split,"[[", 3)
# create a duration in seconds - this will introduce NA where no finish time
cleanClassSurvey$duration_secs <- as.numeric(classSurveyDF$duration_mins)*60 + 
  as.numeric(classSurveyDF$duration_secs)

# create a nice start time using Luke's function
cleanClassSurvey$startTime <- convertiSurveyFinishTime(classSurveyDF$Date.Started)

# create a flag if the survey was finished
cleanClassSurvey$finished[classSurveyDF$Date.Finished != "Did not finish"] <- "Finished"
cleanClassSurvey$finished[classSurveyDF$Date.Finished == "Did not finish"] <- "Unfinished"

# convert to factor so we can do tables etc
cleanClassSurvey$finished <- as.factor(cleanClassSurvey$finished)

# create a nice finish time using Luke's function
cleanClassSurvey$finishTime <- convertiSurveyFinishTime(classSurveyDF$Date.Finished)

# create a flag if the respondent gave feedback on the survey
cleanClassSurvey$feedback[cleanClassSurvey$improve != ""] <- "Feedback"
cleanClassSurvey$feedback[cleanClassSurvey$improve == ""] <- "No feedback"

# convert to factor so we can do tables etc
cleanClassSurvey$feedback <- as.factor(cleanClassSurvey$feedback)

# check variable names
names(cleanClassSurvey)

# remove the 'old' dataset to avoid confusion
classSurveyDF <- NULL

### Analyse cleanClassSurvey ------------------------
# Once you have got the code to work to here you should find that R Studio has a new 'clean'
# dataset called cleanClassSurvey

# Work through the following questions using this dataset
# For help in R type ?something at the > prompt and R will load the help page. e.g.
# > ?plot will open the help page on plot()
# you might find the pdfs in the "R Resources" folder on blackboard useful
# you might also find http://www.statmethods.net/index.html useful
# or try googling "R how to something"

## Lab Q1: How many enjoyed their previous degree? ----
# Hint: use plot(x) with the cleanClassSurvey$previous_safe variable 
# for extra points add labels to the x and y axes - go to help for plot() to see how

# compare enjoyment vs did they finish the survey?
# Hint: use table(x, y) with the 'previous' variable and the 'finished' variable

# turn that into a bar chart
# Hint: use table(x,y) and put the results into a variable

# Hint: now make the barplot(x) using the variable you just created
# of course you could have put the table() function inside the barplot function :-)

## Lab Q2:  Who knows stats by course type? ----
# We could use a table() and then a barplot but let's try something else
# Hint: use mosaicplot(x ~ y) with some fancy colours (you will need 5!) (why will you need 5?)

## Lab Q3: Did the respondents who gave feedback spend longer on the survey? ----

# first check mean duration - what would happen if we didn't tell R what to do about the NAs?
# Hint: use mean() and compare the results with summary()

# now for the comparison...
# Hint: use tapply() to get the mean of duration by yes/no feedback and tell it what to do about NAs!

# what would have happened if we had not told R to ignore the NA?
# what would you conclude from the result?

# make a boxplot to compare duration by feedback/no feedback
# Hint: use boxplot(x ~ y)

# what would you conclude from the boxplot?

## Lab Q4: Do older students spend longer completing surveys? ----
# Hint: use boxplot(x ~ y) to compare duration across age, 
# You could also add a label to the x & y axes

# anything strange about this one?

## Testing a correlelogram (if time - not a good example) ----
#install.packages("corrgram") # if needed
library(corrgram)

# convert factors to numerics to fool corrgram

# create corrgram

# STOP HERE!
print("XXXXXXXXXXX")
print("You were supposed to stop before here!")

## Confidence Intervals ----

# calculating a 95% CI for duration
m <- mean(cleanClassSurvey$duration_secs, na.rm = TRUE)
s <- sd(cleanClassSurvey$duration_secs, na.rm = TRUE)
n <- length(cleanClassSurvey$duration_secs[cleanClassSurvey$finished == "Finished"]) # what would happen if we didn't exclude the NAs?
error <- qnorm(0.975)*(s/sqrt(n))

duration_secsCIu <- m + error
duration_secsCIl <- m - error

print(paste0("Duration (secs) lower 95% CI: ", duration_secsCIl))
print(paste0("Duration (secs) mean: ", m))
print(paste0("Duration (secs) upper 95% CI: ", duration_secsCIu))

## Calculating 95% CI for % Civil Engineers
non_e <- length(cleanClassSurvey$previous_safe[cleanClassSurvey$previous_safe == "Civil Engineer" 
                                               & !is.na(cleanClassSurvey$previous_safe)]) # make sure ignore NA
valid <-  length(cleanClassSurvey$previous_safe[!is.na(cleanClassSurvey$previous_safe)]) # count the valid cases

p <- non_e/valid

error <- qnorm(0.975) * sqrt((p*(1-p))/valid)

non_eCIu <- p + error
non_eCIl <- p - error

print(paste0("non_eCIu lower 95% CI: ", non_eCIl))
print(paste0("non_e proportion: ", p))
print(paste0("non_eCIl lower 95% CI: ", non_eCIu))


# just for fun - use a t test to compare duration for yes/no feedback
t.test(cleanClassSurvey$duration_secs ~ cleanClassSurvey$feedback)


