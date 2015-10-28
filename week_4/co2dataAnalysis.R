##################################
# CO2 and humidty analysis
# Purpose:
# to introduce loading data remotely
# to plot data

# Luke's sensor data is at:
# http://www.southampton.ac.uk/~lsb1/data/latestTRHweb.csv
# we can load it in to R without downloading it seperately like this:

##################################
# Load it & examine ----
co2data <- read.csv("http://www.southampton.ac.uk/~lsb1/data/latestTRHweb.csv")

# that's it!

# have a look at the first few rows of data
head(co2data)

##################################
# Convert to useful time ----
# L: we need to convert the timestamp into something that R can work with
# (currently R thinks that it is just a collection of bits of text)
# store the R time in a new field of the data frame
# "%a %b %d %X %Y" is a time format string - type ?strptime for details
# In this case:
# %a - day of the week in text
# %b - month of year in text
# %d - day of month as number
# %X - HH:MM:SS
# %Y - year as a number
co2data$RTime<-as.POSIXct(as.character(co2data$localTime),tz="","%a %b %d %X %Y")

##################################
# Plots !! ----
# plot temp
plot(co2data$RTime,co2data$temp_degC, type="l", col=1)
# now add the relative humidity to the same plot
lines(co2data$RTime,co2data$relHumid_pc, type="l", col=2)
# what happened to the second line?

# if we check the range for each variable we will see that...
summary(co2data)
#... they don't overlap, so the second line is off the top of the plot

# fix this by setting an appropriate ylim to include all the data
# you could be really clever here and work out which is the lowest/highest value for each
# variable 'on the fly' but we'll just use the results of summary()!
plot(co2data$RTime,co2data$temp_degC, type="l", col=1, 
     ylim = c(20,70),
     xlab="Date/time", 
     ylab="Rate")
     
# now add the relative humidity to the same plot
lines(co2data$RTime,co2data$relHumid_pc, type="l", col=2,
      xaxt="n",
      yaxt="n",xlab="",ylab="")

# add legend
legend("topleft",col=c(1:2),lty=1,
       legend=c("Temperature (degC)","Relative Humidity (%)"))

##################################
# Homework :-) ----
# Plot one of the lines on a different y axis scale on the same plot to enable better comparison
# Try smoothing the relative humidity using the filter() function to see if you
# can see a relationship with the CO2 levels
