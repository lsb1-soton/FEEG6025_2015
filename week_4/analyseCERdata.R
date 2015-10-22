# Meta -----
# Analyse CER data
# code by: b.anderson@soton.ak.uk (@dataknut)

# Housekeeping ----

# install a couple of packages that we will need
# if you have them already, you can comment out these lines with a "#"
install.packages("data.table") 
library(data.table)
install.packages("zoo")
library(zoo)

# clear the workspace
rm(list=ls())

# Working directory is:
getwd()

# change the working directory to where you put the data
# setwd("<where you put the data>")
# In my case:
setwd("./data/CER")

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
##hh_1024$oct[grep("oct",hh_1024$s_datetime)] <- 1

#L: Or we can create a date vector using R's date conversion functions
hh_1024$l_datetime<-as.POSIXct(hh_1024$s_datetime,tz = "", "%d%b%Y %H:%M:%S")

# select just October - you'll see why in a minute
##hh_1024oct <- subset(hh_1024, hh_1024$oct == 1)
#L: we can select data from the dataframe by date range
date_start<-as.POSIXct("2009-10-01",tz="")
date_end<-as.POSIXct("2009-10-31",tz="")
hh_1024oct <- hh_1024[hh_1024$l_datetime %in% date_start:date_end, ]

# need to check is sorted by datetime (always increasing) and evenly spaced
# If so, the time series is regular and we can analyze it
##hh_1024oct_sorted <- hh_1024oct[order(hh_1024oct$s_datetime),] # only works if we remove the sept days!

# create zoo (time series) object
hh_1024oct_z=zoo(hh_1024oct)

# is it a regular time series? (using zoo function)
is.regular(hh_1024oct_z)

plot(hh_1024oct$kwh)

# run acf with the first household only up to just over 48 hours (96 half hours)
acf(hh_1024oct$kwh, lag.max = 100)
# what do we conclude from that?

# let's find the *partial autocorrelation function* (pacf)
# this is the effect of successive lags with the effect of previous lags removed
# It shows more clearly how the random variation depends on the previous lags
# see https://www.youtube.com/watch?v=R-oWTWdS1Jg
pacf(hh_1024oct$kwh, lag.max = 100)
# how many lags are significant?

# what might happen if we excluded sleep time (00:00 - 06:00?)
# hint: 
# hh_1024oct$my_hod<-(as.double(hh_1024oct$l_datetime)%%86400)/3600
# ...gives decimal hour of day
# then can use indexing to remove this data:
#(hh_1024oct$my_hod>0) & (hh_1024oct$my_hod<=6)

# what kind of household is this?
table(hh_1024oct$ba_nadults)
# how many adults?

table(hh_1024oct$ba_nchildren)
# how many children?

table(hh_1024oct$ba_empl)
# Employed?

