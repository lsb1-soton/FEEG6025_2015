
# Open your csv file
HR <- read.csv("WRITE THE PATH OF YOUR FILE")

######################################
# Review your file

nrow(HR) #number of rows
head(HR) #first 6 rows of your data frame
tail(HR) #last 6 rows of your data frame

######################################
# Review one of the variables of your file

max(HR$HRbpm) #maximun value of the variable HRbpm
min(HR$HRbpm) #minimum value of the variable HRbpm
max(HR$HRbpm)-min(HR$HRbpm) #range of the variable HRbpm
mean(HR$HRbpm) #mean of the variable HRbpm
median(HR$HRbpm) #median of the variable HRbpm
as.numeric(names(table(round(HR$HRbpm,0)))[which.max(table(round(HR$HRbpm,0)))]) #mode of the variable HRbpm
sd(HR$HRbpm) #standart deviation of the variable HRbpm (assuming that the variable HRbpm is a sample of the population)

######################################
# Plot one of the variables of your file

# Line graph
plot(HR$HRbpm, type="p", col=2, main="Heart Rate", ylab="Heart Rate [bpm]", xlab="Time [s]", ylim=c(50,200))

# Histogram
hist(HR$HRbpm, breaks=(max(HR$HRbpm)-min(HR$HRbpm))/2, main="Heart Rate",  xlab="Heart Rate [bpm]")
abline(v=mean(HR$HRbpm), col=2, lwd=2, lty=1)
abline(v=mean(HR$HRbpm)+sd(HR$HRbpm), col=2, lwd=1, lty=2)
abline(v=mean(HR$HRbpm)-sd(HR$HRbpm), col=2, lwd=1, lty=2)
