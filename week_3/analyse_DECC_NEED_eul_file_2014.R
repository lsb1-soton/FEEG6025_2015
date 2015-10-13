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
dpath <- "~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Data"
# set a variable for where we want the results to go
rpath <- "~/OneDriveBusiness/PG/Southampton/FEEG6025 Data Analysis & Experimental Methods for Engineers/Week 3"

# input file name
# long form of data (pre-processed)
ifile <- "need_eul_may2014_consumptionfile_long_100pc.dta"

### Required packages ----
# We're going to load a STATA file
library(foreign)
### Load data  ------------------------

# change working directory to where the data is
setwd(dpath)
# load the data into a data frame
# dom't ask me why we need to explicitly say it has "," as the seperater!
needEulf2014DF <- read.dta(ifile)

# change the working directory once loaded in case we want to save out any results
setwd(rpath)

# how much data do we have?
dim(needEulf2014DF)

## Q0: Check out the data ----
# Q0.1: What shape are the distributions of kWh for gas & electricity?

# Q0.2: Do you notice anything strange about them?

## Q1: Descriptive Analysis
# Q1.1: Plot mean and median kWh for elec & gas over time ----
# Q1.2: Plot mean and median kWh for elec & gas over time for different energy bands ----

## Q2: 
# Q2: Examine correlation of gas & electricity consumption for different kinds of households
