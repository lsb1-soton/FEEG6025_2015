# Meta ----
# Script to load & analyse DECC NEED data
# Data from: http://discover.ukdataservice.ac.uk/catalogue/?sn=7518
# See also: https://www.gov.uk/government/collections/national-energy-efficiency-data-need-framework

# code by: b.anderson@soton.ak.uk (@dataknut)

### Housekeeping  ------------------------

# clear the workspace
rm(list=ls())

# where is the default working directory?
getwd()

# set a variable with the data location
# You will need to change this
dpath <- "~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Data"
# set a variable for where we want the results to go
# You will need to change this too!
rpath <- "~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Week 3"

# input file name
# long form of data (pre-processed)
# notice that it has a .dta suffix - this is a STATA format data file
ifile <- "need_eul_may2014_merged_100pc.dta"

### Required packages ----
# We're going to load a STATA file so we need the 'foreign' package
#install.packages("foreign") # if needed
library(foreign)

### Load data  ------------------------

# change working directory to where the data is
setwd(dpath)

# load the data into a data frame
# this may take a few seconds - it is quite large!
needEulf2014DF <- read.dta(ifile)

# change the working directory once loaded in case we want to save out any results
setwd(rpath)

# how much data do we have?
dim(needEulf2014DF)

## Q0: Check out the data ----
# Q0.0: What are the variable names?
# Hint: use names()
names(needEulf2014DF)

# Q0.1: What kind of data do we have and what are the central tendencies?
# Hint: use summary()
summary(needEulf2014DF)

# Q0.1a: Do that again for gas & electricity consumption - do you notice anything strange?
# Why might that have happened?
# Hint: use summary() for each variable
summary(needEulf2014DF$Gcons)
summary(needEulf2014DF$Econs)

# Q0.2: How many properties do not have 'valid' elec readings per year?
# Hint: use table(x,y) and the EconsValid variable
table(needEulf2014DF$EconsValid, needEulf2014DF$year)
# What do the codes G, L, M & V mean?
# Hint: RTFM :-) -> http://bit.ly/1LazmvE -> "Excel workbook explaining variable codes"

# Q0.2a Draw a mosaicplot to visualise this
# Hint: You will need 4 colours as there are 4 'valid' codes
mosaicplot(year ~ EconsValid, 
           data = needEulf2014DF,
           main = "Valid codes by year (Electricity)",
           xlab = "Year",
           ylab = "N",
           color = c("tan1", "tan2", "sienna1", "green1"))

# Q0.3: How many properties do not have 'valid' gas readings per year? 
# How many do not have gas at all?
# Hint: use table(x,y) and the GconsValid variable
table(needEulf2014DF$GconsValid, needEulf2014DF$year)

# Q0.3a Draw a mosaicplot to visualise this
# no hint needed except you will need 5 colours this time to include O (off gas)
mosaicplot(year ~ GconsValid, 
           data = needEulf2014DF,
           main = "Valid codes by year (Gas)",
           xlab = "Year",
           ylab = "N",
           color = c("tan1", "tan2", "sienna1", "black", "green1"))


## Q1: Descriptive Analysis over time ----
# Q1.1: How does elec consumption vary over time?
# Hint: use a boxplot(x ~y) and don't forget to add axis labels!
boxplot(Econs ~ year,
        data = needEulf2014DF,
        xlab = "Year",
        ylab = "E consumption (kWh)")

# Q1.2: How does gas consumption vary over time?
boxplot(Gcons ~ year,
        data = needEulf2014DF,
        xlab = "Year",
        ylab = "G consumption (kWh)")

## Q2: Descriptive Analysis by UK region for 2012 ----
# Q2.1 is there any difference in domestic electricity consumption by UK region in 2012?
# Hint: use a boxplot() but select year == 2012 by sub-setting first & don't forget to label axes
cons2012 <- subset(needEulf2014DF, needEulf2014DF$year == 2012)
boxplot(cons2012$Econs ~ cons2012$ba_region,
        xlab = "Region",
        ylab = "E consumption (kWh, 2012)")

# Q2.1 is there any difference in domestic gas consumption by UK region in 2012?
# Hint: use a boxplot() but select year == 2012 by sub-setting first
boxplot(Gcons ~ ba_region,
        data = cons2012,
        xlab = "Region",
        ylab = "G consumption (kWh, 2012)")

## Q3: Descriptive Analysis by energy efficiency band in 2012 ----
# Q3.1 is there a difference in domestic electricity consumption by EE band in 2012?
# Hint: have a quick look at the EE_BAND first
table(cons2012$EE_BAND)

# now use boxplot (no hint, you should know how to do this!)
boxplot(Econs ~ EE_BAND,
        data = cons2012,
        xlab = "EE Band",
        ylab = "E consumption (kWh, 2012)")

# Q3.1 is there a difference in domestic electricity consumption by EE band in 2012?
# (no hint, you should know how to do this!)
boxplot(Gcons ~ EE_BAND,
        data = cons2012,
        xlab = "EE Band",
        ylab = "G consumption (kWh, 2012)")

# Q4: Is there an association between property age and wall structure using 2012 subset? ----
# Why use the 2012 subset? What would happen if we used all the data?
mosaicplot(PROP_AGE ~ WALL_CONS, 
           data = cons2012,
           main = "Dwelling age by wall type",
           xlab = "Dwelling age",
           ylab = "Wall type",
           color = c("green", "red"))

# Homework! Edit this file to save the graphs so you can use them in a report ----
# Hint: refer to the playWithDistributions.R code :-)