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
# you will need to change this to whever you put the data (.csv fiule) you just downloaded
# use the results of getwd() above to see the format to use for the PC you are using
dpath <- "~/UoS One Drive/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Data"

# change the working directory/folder so R can find the data easily
setwd(dpath)

# input file name
# make sure this is the name of the data file (without the .csv suffix)
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

# load the data into a data frame using the 'file' variable you set above
# note that we add the '.csv' part here for reasons that will become clear when we start using other data types
classSurveyDF <- read.csv(paste0(file, ".csv"))

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

### Clean and process the data ------------------------

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

# Work through the following questions using this dataset and the commands you used in swirl on Tuesday
# For help in R type ?something at the > prompt and R will load the help page. e.g.
# > ?plot will open the help page on plot()
# you might find the pdfs in the "R Resources" folder on blackboard useful
# you might also find http://www.statmethods.net/index.html useful
# or try googling "R how to something"

## Lab Q1: How many enjoyed their previous degree? ----
# Hint: use plot(x) and add labels to the x and y axes using xlab = "" and ylab = ""
# R uses , to separate options...
plot(cleanClassSurvey$enjoy, xlab = "Enjoyment of previous course", ylab = "N")

# compare enjoyment vs which course done previously?
# Hint: use table(x, y) and the 'previous_safe' variable which I have already recoded into 3 options
# to prevent possible identification
table(cleanClassSurvey$enjoy, cleanClassSurvey$previous_safe)

# turn that into a bar chart
# Hint: use table(x,y) and put the results into a variable
# you can give the variable any name you like. Originally I used 'count' but just to be clear
# I will change this to something more meaningful and then will have to edit the barplot code too
tableEnjoyPrevious <- table(cleanClassSurvey$enjoy, cleanClassSurvey$previous_safe)

# Hint: now make the barplot(x) and use the 'beside' option to put the bars next to each other
# try adding a legend too
# change 'counts' to 'tableEnjoyPrevious' (see above)
barplot(tableEnjoyPrevious, 
        main="Previous course enjoyment by course type",
        xlab = "Course type",
        ylab = "N",
        legend = rownames(tableEnjoyPrevious), beside=TRUE )
# try running that afgain without the 'beside = TRUE' and see what happens - you should get a stacked bar chart instead
# with the bars on top of each other. Usually you would want to do this with % not counts so you'd have to create a different
# table first

# of course you could have put the table() function inside the barplot function :-)
# like this:
barplot(table(cleanClassSurvey$enjoy, cleanClassSurvey$previous_safe), 
        main="Previous course enjoyment by course type",
        xlab = "Course type",
        ylab = "N",
        legend = rownames(table(cleanClassSurvey$enjoy, cleanClassSurvey$previous_safe)), beside=TRUE )

## Lab Q2:  Who knows stats by previous course type? ----
# We could use a table() and then a barplot but let's try something else
# Hint: use mosaicplot(x ~ y) with some fancy colours (you will need 5!) (why will you need 5?)
# the following form may not work in old versions of R
mosaicplot(previous_safe ~ stats, 
           data = "cleanClassSurvey",
           main = "Previous reported stats experience by course type",
           xlab = "Previous course",
           ylab = "Stats experience",
           color = c("tan1", "tan2", "sienna1", "brown1", "firebrick1"))

# if so try this instead (it specifies the variables in a different way):
mosaicplot(cleanClassSurvey$previous_safe ~ cleanClassSurvey$stats, 
           main = "Previous reported stats experience by course type",
           xlab = "Previous course",
           ylab = "Stats experience",
           color = c("tan1", "tan2", "sienna1", "brown1", "firebrick1"))

# mosaicplot doesn't allow legends. If you want a legend use the vcd package's mosiac command 
# (see http://www.statmethods.net/advgraphs/mosaic.html)

# to see which colors you could use you might want to use try:
# demo(colors) - it will step through a large number of examples!

## Lab Q3: Did the respondents who gave feedback spend longer on the survey? ----

# first check mean duration - what would happen if we didn't tell R what to do about the NAs?
# Hint: use mean() and compare the results with summary()
mean(cleanClassSurvey$duration_secs)
mean(cleanClassSurvey$duration_secs, na.rm = TRUE)
summary(cleanClassSurvey$duration_secs)

# now for the comparison...
# Hint: use tapply() to get the mean of duration by yes/no feedback and tell it what to do about NAs!
tapply(cleanClassSurvey$duration_secs, cleanClassSurvey$feedback, na.rm = TRUE, mean)
# what would have happened if we had not told R to ignore the NA?
# what would you conclude from the result?

# if you want to compare the quartiles etc (as boxplot will do):
summary(cleanClassSurvey$duration_secs[
  !is.na(cleanClassSurvey$duration_secs) & 
    cleanClassSurvey$feedback == "Feedback"]) # where feedback given, checking for NA
summary(cleanClassSurvey$duration_secs[
  !is.na(cleanClassSurvey$duration_secs) & 
    cleanClassSurvey$feedback == "No feedback"]) # where no feedback given, checking for NA

# or much cleaner (but doesn't give the mean):
tapply(cleanClassSurvey$duration_secs, cleanClassSurvey$feedback, na.rm = TRUE, quantile)

# make a boxplot to compare duration by feedback/no feedback
# Hint: use boxplot(x ~ y)
boxplot(duration_secs ~ feedback,
        data = cleanClassSurvey,
        ylab = "Duration (s)")
# what would you conclude from the boxplot?

# just for fun - use a t test to compare duration for yes/no feedback
t.test(cleanClassSurvey$duration_secs ~ cleanClassSurvey$feedback)

## Lab Q4: Do older students spend longer completing surveys? ----
# Hint: use boxplot(x ~ y) to compare duration across age, 
# You could also add a label to the x & y axes
boxplot(duration_secs ~ age, 
        data = cleanClassSurvey,
        xlab = "Age", ylab = "Duration of survey")
# anything strange about this one?

## Testing a correlelogram (if time - not a good example) ----
#install.packages("corrgram") # if needed
library(corrgram)

# convert factors to numerics to fool corrgram
cleanClassSurvey$enjoy.n <- as.numeric(cleanClassSurvey$enjoy)
cleanClassSurvey$tv.n <- as.numeric(cleanClassSurvey$tv)
cleanClassSurvey$cold.n <- as.numeric(cleanClassSurvey$cold)
cleanClassSurvey$energy.n <- as.numeric(cleanClassSurvey$energy)

# create corrgram
cvars <- c("age", "duration_secs", "enjoy.n", "tv.n", "cold.n", "energy.n")
corrgram(cleanClassSurvey[,cvars], order=TRUE, lower.panel=panel.pts,
         upper.panel=panel.pie,
         main="Correlations (sorted)") 

# there, isn't that pretty. What does it show?
# it shows the correlation between a set of variables - the pie charts show the
# sice of the correlaiton. The variables are listed on the diagonal. The scatterplots
# are just scatter plots of the pairs of variables...

# There is a quite strong correlation between the scores for 'cold' (put on more clothes when cold) and 'energy' (try to save it)


# STOP HERE!
print("XXXXXXXXXXX")
print("You were supposed to stop before here!")

## Confidence Intervals ----

# Calculating a 95% CI for mean duration
# first calculate the mean, checking for NA (as some people did not finish)
m <- mean(cleanClassSurvey$duration_secs, na.rm = TRUE)
# now calculate the standard deviation
s <- sd(cleanClassSurvey$duration_secs, na.rm = TRUE)
# count how many valid cases using length() there are making sure we exclude NAs
n <- length(cleanClassSurvey$duration_secs[!is.na(cleanClassSurvey$finished)]) 
# what would happen if we didn't exclude the NAs?

# calculate the 95% error using the formula and R's qnorm function to give us a precise answer
# remember qnorm(0.975) ~= 1.96 
error <- qnorm(0.975)*(s/sqrt(n))

# calculate the upper and lower 95% CI by adding/subtracting the error to/from the mean
duration_secsCIu <- m + error
duration_secsCIl <- m - error

# tell the user the answers
print(paste0("Duration (secs) lower 95% CI: ", duration_secsCIl))
print(paste0("Duration (secs) mean: ", m))
print(paste0("Duration (secs) upper 95% CI: ", duration_secsCIu))

# now redraw the old boxplot for duration and draw on the 95% CIs
boxplot(cleanClassSurvey$duration_secs, main = "Box plot of duration")
abline(h = m, col = "red") # draw the mean
text(1, m , "Mean", col = "red") # add label
abline(h=duration_secsCIu, col = "blue") # draw the upper 95% CI
text(1, duration_secsCIu , "Upper 95%", col = "blue") # add label
abline(h=duration_secsCIl, col = "green") # draw the lower 95% CI
text(1, duration_secsCIl , "Lower 95%", col = "green") # add label

## Calculating 95% CI for % Civil Engineers
# how many civil engineers are there?
# use length() to calculate the 'length' of the column (vector) after
# we've selected the Civil Engineers
n_civil <- length(cleanClassSurvey$previous_safe[
  cleanClassSurvey$previous_safe == "Civil Engineer" 
  & !is.na(cleanClassSurvey$previous_safe)]) # make sure ignore NA
# how many valid cases are there?
n_valid <-  length(cleanClassSurvey$previous_safe[
  !is.na(cleanClassSurvey$previous_safe)]) # make sure ignore NA

# what proportion of validcases are engineers?
p <- n_civil/n_valid

# calculate the 95% error using the formula and R's qnorm function
error <- qnorm(0.975) * sqrt((p*(1-p))/n_valid)

# calculate the 95% CI
non_eCIu <- p + error
non_eCIl <- p - error

# tell the use the answer
print(paste0("non_eCIu lower 95% CI: ", non_eCIl))
print(paste0("non_e proportion: ", p))
print(paste0("non_eCIl lower 95% CI: ", non_eCIu))
