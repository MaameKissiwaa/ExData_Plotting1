
ExpAnaly <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Format date to Type Date
ExpAnaly$Date <- as.Date(ExpAnaly$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
ExpAnaly <- subset(ExpAnaly,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
ExpAnaly <- ExpAnaly[complete.cases(ExpAnaly),]

## Combine Date and Time column
dateTime <- paste(ExpAnaly$Date, ExpAnaly$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
ExpAnaly <- ExpAnaly[ ,!(names(ExpAnaly) %in% c("Date","Time"))]

## Add DateTime column
ExpAnaly <- cbind(dateTime, ExpAnaly)

## Format dateTime Column
ExpAnaly$dateTime <- as.POSIXct(dateTime)
## Create the histogram
hist(ExpAnaly$Global_active_power, main="Global Active Power", xlab = "GlobalActivePower(Kilowatts)", col="red")
## Save First Histogram  file and close my Pc
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
## Create Plot 2 ie a plot
plot(ExpAnaly$Global_active_power~ExpAnaly$dateTime, type="l", ylab="GlobalActivePower(Kilowatts)", xlab="")
##Save second plot and close
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()
###Create Plot3 quite complicated##
with(ExpAnaly, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
###Save complicated graph3###
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

###Combining all graph effects ie Plot 4 annotated ###

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(ExpAnaly, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
###Save annotated graph 4 of all previous graphs###
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
