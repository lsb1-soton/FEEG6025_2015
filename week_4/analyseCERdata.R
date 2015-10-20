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
# load the data - this may take some time even though it's only 10% of the actual October 2009 data
# it's a csv file (I hope you unzipped it first!)
residentialCER <- as.data.frame(
  read.csv("CER-halfhour-electricity-Census2022-Oct-2009-sample_res_10pc_v1.csv"))

# what's here?
names(residentialCER)

# how many rows/columns?
dim(residentialCER)

# mean electricity consumption
summary(residentialCER$kwh)

# Time series analysis ----
# create a subset for the first household only
hh_1024 <- subset(residentialCER, residentialCER$ID == 1024)
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
