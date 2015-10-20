#==============Plot data with trend===============#

# generate sequence of integers
t<-seq(250) # represents samples e.g. every second

# create a time series y made up of a linear trend 
# and a random component
y<-0.01*t # linear
e<-rnorm(length(y)) # normally distributed, s.d. of 1
y<-y+e # trend plus random noise with constant variance

# plot the time series
plot(t,y)

# add the sample mean (not very helpful)
lines(t,seq(mean(y), mean(y), length.out=length(y)),col="red")
# could have used 
# abline(h = mean(y), col="red")

#================Moving average=================#

# create filters
myfilt10=rep(1/10,10) # equal weighting
myfilt50=rep(1/50,50) # equal weighting

# create moving averages using the filter() function
# and the two filters
# this calculates the mean for moving groups of size defined by the filters
yfilt10=filter(y,myfilt10)
yfilt50=filter(y,myfilt50)

# plot them on the same axes
lines(t,yfilt10,col="blue")
lines(t,yfilt50,col="green")

#=======autocorrelation function================#

# of the random component
acf(e)
 
# of the time series - note how acf clearly shows signal is non-random
acf(y)
