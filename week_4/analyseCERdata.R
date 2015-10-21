# Meta -----
# Analyse CER data
# code by: b.anderson@soton.ak.uk (@dataknut)

# Housekeeping ----

# clear the workspace
rm(list=ls())

# Working directory is:
getwd()

# change the working directory to where you put the data
# setwd("<where you put the data>")
# In my case:
setwd("~/Documents/Work/Data/CER Smart Metering Project/data/processed")

# Load data & describe it ----
# Load the data
# This may take some time - it's about 0.5 million observations
# even though it's only 10% of the actual October 2009 data.
# It's a csv file (I hope you unzipped it first!)
# Perhaps we should load it as a data table...?
residentialCER <- as.data.frame(
  read.csv("CER-halfhour-electricity-Census2022-Oct-2009-sample_res_10pc_wf_v1.csv"))

# what's here?
names(residentialCER)

# how many rows/columns?
dim(residentialCER)

# mean electricity consumption
summary(residentialCER$kwh)

# Plots - convert to data table first ----
# data table functions are quicker (could have created data table to start with...)
library(data.table)
residentialCER_DT <- as.data.table(residentialCER)
halfHourly <- residentialCER_DT[,mean(kwh),by=s_halfhour] # mean per half hour for all of October
plot(halfHourly) # familiar daily profile?

# now do by date and half hour
halfHourly_date <- residentialCER_DT[,mean(kwh),by=s_datetime]
plot(halfHourly_date)

# Time series analysis ----
# create a subset for the first household only
hh_1024 <- subset(residentialCER, residentialCER$ID == 1005)
# set a flag so we can select Oct only
hh_1024$oct[grep("oct",hh_1024$s_datetime)] <- 1

# select just October - you'll see why in a minute
hh_1024oct <- subset(hh_1024, hh_1024$oct == 1)

# need to check is sorted by datetime!
hh_1024oct_sorted <- hh_1024oct[order(hh_1024oct$s_datetime),] # only works if we remove the sept days!

plot(hh_1024oct_sorted$kwh)

# run acf with the first household only up to just over 48 hours (96 half hours)
acf(hh_1024oct_sorted$kwh, lag.max = 100)
# what do we conclude from that?
# what might happen if we excluded sleep time (00:00 - 06:00?)

# what kind of household is this?
table(hh_1024oct_sorted$ba_nadults)
# single person

table(hh_1024oct_sorted$ba_nchildren)
# no children

table(hh_1024oct_sorted$ba_empl)
# in work

