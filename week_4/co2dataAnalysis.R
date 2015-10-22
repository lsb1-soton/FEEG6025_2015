# CO2 and humidty analysis
# Purpose:
# to introduce loading data remotely
# to plot data

# Luke's sensor data is at:
# http://www.southampton.ac.uk/~lsb1/data/latestTRHweb.csv
# we can load it in to R without downloading it seperately like this:

co2data <- read.csv("http://www.southampton.ac.uk/~lsb1/data/latestTRHweb.csv")

# that's it!

# have a look at the first few rows of data
head(co2data)

# plot temp
plot(co2data$temp_degC, type="l", col=1)
# now add the relative humidity to the same plot
lines(co2data$relHumid_pc, type="l", col=2)
# what happened to the second line?

# if we check the range for each variable we will see that...
summary(co2data)
#... they don't overlap, so the second line is off the top of the plot

# fix this by setting an appropriate ylim to include all the data
plot(co2data$temp_degC, type="l", col=1, 
     ylim = c(20,70),
     xlab="Time [s]", 
     ylab="Rate")
     
# now add the relative humidity to the same plot
lines(co2data$relHumid_pc, type="l", col=2,
      xaxt="n",
      yaxt="n",xlab="",ylab="")

# add legend
legend("topleft",col=c(1:2),lty=1,
       legend=c("CO2 level","Relative Humidity"))

# Homework :-) ----
# Plot one of the lines on a different y axis scale on the same plot to enable better comparison
# Try smoothing the relative humidity using the filter() function to see if you
# can see a relationship with the CO2 levels