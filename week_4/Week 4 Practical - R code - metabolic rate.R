
# ESTIMATING ACTIVITY LEVEL

#Load file i.e. Group1_HRACC1Hz.csv
MET <- read.csv("WRITE THE PATH OF YOUR FILE")

##################################
# ESTIMATING ACTIVITY LEVEL using Heart Rate

# Estimate the weight of the participant in kg
WB <- "0"

# Estimate the age of the participant in years
PA <- "0"

# Refer to ISO 8996:2004 Annex C 
x <- "0"
y <- "0"
MET$METhr <- (x * MET$HR) - y
mean(MET$METhr, , na.rm=T)

##################################
# ESTIMATING ACTIVITY LEVEL using Accelerometer

# Estimate the height of the participant in meter
HB <- "0"

# Refer to Ralston, H. (1958). Energy-speed relation and optimal speed during level walking. European Journal of Applied Physiology,17, 277–283. doi:10.1007/BF00698754
#Ew <- 29 + 0.0053 * (va^2) with Ew is the energy expenditure in cal/min/kg, and va is the velocity in m/min
va <- MET$LA
#Convert to SI Unit with PW is the power in W, WB is the mass in kg, va is the velocity in m/s 
PW <- (2.02 + (1.33*(va^2))) * WB

# Refer toDu Bois formula in ISO 8996:2004 section 7.1.2
ADU <- 0.202 * (WB^0.425) * (HB^0.725)

MET$METacc <- PW / ADU
mean(MET$METacc, na.rm=T)

##################################
# PLOT both estimations

par(mar=c(5,4,4,5)+.1)
plot(MET$METhr, type="p", col=1, main="Group 1 - Activity Level", ylab="Metabolic Rate from Heart Rate [W/m2]", xlab="Time [s]", ylim=c(0,700))
par(new=TRUE)
plot(MET$METacc, type="l", col=2, xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,700))
axis(4)
mtext("Metabolic Rate from Acceleration [W/m2]",side=4,line=3)
legend("topleft",col=c(1:2),lty=1,legend=c("Metabolic Rate from Heart Rate","Metabolic Rate from Acceleration"))

##################################
# Moving average

myfilt10=rep(1/10,10) 
myfilt30=rep(1/30,30) 

yfilt10=filter(MET$METacc,myfilt10)
yfilt30=filter(MET$METacc,myfilt30)

lines(yfilt10,col="blue")
lines(yfilt30,col="green")

