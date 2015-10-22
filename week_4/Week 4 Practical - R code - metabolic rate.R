# code for week 4 lab using heart rate and accelerometer data
# Purpose: ESTIMATING ACTIVITY LEVEL
# code by @Stepha_Gauthier with additions from @dataknut

# Load the heart rate file for your group i.e. Group1_HRACC1Hz.csv
# for me: 
MET <- read.csv("~/Data/AllGroup_HRACC1Hz/Group7_HRACC1Hz.csv")

##################################
# ESTIMATING ACTIVITY LEVEL using Heart Rate ----

# Estimate the weight of the participant in kg and put it in a variable
WB <- "0"

# Estimate the age of the participant in years & put it in a new variable
PA <- "0"

# Refer to ISO 8996:2004 Annex C for these values by gender
x <- "0"
y <- "0"

# now calculate metabolic rate using these values
MET$METhr <- (x * MET$HR) - y

# calculate the mean checking for NA
# Why do we need to check for NA?
# Hint: look at MET$HR to see if there were missing observations in the raw data
mean(MET$METhr, na.rm=TRUE)

##################################
# ESTIMATING ACTIVITY LEVEL using Accelerometer ----

# Estimate the height of the participant in meters (not cm!!)
HB <- "0"

# Refer to Ralston, H. (1958). Energy-speed relation and optimal speed during level walking. European Journal of Applied Physiology,17, 277–283. doi:10.1007/BF00698754
#Ew <- 29 + 0.0053 * (va^2) with Ew is the energy expenditure in cal/min/kg, and va is the velocity in m/min
va <- MET$LA
#Convert to SI Unit with PW is the power in W, WB is the mass in kg, va is the velocity in m/s 
PW <- (2.02 + (1.33*(va^2))) * WB

# Refer toDu Bois formula in ISO 8996:2004 section 7.1.2
# this estimates suface area from weight & height
ADU <- 0.202 * (WB^0.425) * (HB^0.725)

# calculate activity level by dividing power (PW) by estimated surface area

MET$METacc <- PW / ADU
# again, check for NAs (Why?)
mean(MET$METacc, na.rm=T)

##################################
# PLOT both estimations on the same chart ----
# notice that we can let a command extend over 2+ lines
# as long as R recognises that the command is not yet complete

# set the size of the margins of our plot
# type ?par to see what these and other options do
par(mar=c(5,4,4,5)+.1)
# draw the first plot with points (type = "p")
plot(MET$METhr, type="p", col=1, 
     main="Group X - Activity Level", 
     ylab="Metabolic Rate from Heart Rate [W/m2]", 
     xlab="Time [s]", ylim=c(0,700))
par(new=TRUE) # tells R to add a new plot to the one that exists
# draw the second on top using a red (col = 2) line (type = "l")
plot(MET$METacc, type="l", col=2, xaxt="n",
     yaxt="n",xlab="", ylab="", ylim=c(0,700)) # leave ylab empty otherwise it would be drawn over the existing one
#add the axis on the rhs of the plot
axis(4)
# add the rhs axis label
mtext("Metabolic Rate from Acceleration [W/m2]",side=4,line=3)
legend("topleft",col=c(1:2),lty=1,
       legend=c("Metabolic Rate from Heart Rate","Metabolic Rate from Acceleration"))

##################################
# Moving average
# we want to smooth that spiky red line to see if there are any underlying trends
# or cycles

# first we create some 'windows' or 'filters' we will calculate the moving average in
myfilt10=rep(1/10,10) # windows of 10 observations
myfilt30=rep(1/30,30) # windows of 30 observations

# which do you think will have the greatest smoothing effect?

# now use the two filters to calculate two moving averages of METacc
yfilt10=filter(MET$METacc,myfilt10)
yfilt30=filter(MET$METacc,myfilt30)

# now add them to the plot from before using new colours
lines(yfilt10,col="blue")
lines(yfilt30,col="green")

# Were you correct about which filter would smooth the spikes the most?

# Excellent, now you know how to: ----
# calculate a new variable based on the ones you observed
# plot and smooth a time series
# draw multiple graphic elements on one plot
