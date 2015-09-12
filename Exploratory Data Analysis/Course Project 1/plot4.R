library(sqldf)

data <- read.csv2.sql(file = "household_power_consumption.txt", sql = "select * from file where Date='1/2/2007' or Date='2/2/2007'", header = TRUE)
data[[1]]<- strptime(paste(data[[1]], data[[2]]), format = "%d/%m/%Y %T")
data$Time <- NULL

png(filename = "plot4.png")
par(mfrow = c(2, 2))

plot(data$Global_active_power, xlab = "", ylab = "Global Active Power (Kilowatts)", type = "n", xaxt = "n")
lines(data$Global_active_power)
axis(1, at = c(1, length(data$Global_active_power)/2, length(data$Global_active_power)), labels = c("Thu", "Fri", "Sat"))

plot(data$Voltage, ylab = "Voltage", xlab = "datetime", type = "n", xaxt = "n")
lines(data$Voltage)
axis(1, at = c(1, length(data$Voltage)/2, length(data$Voltage)), labels = c("Thu", "Fri", "Sat"))

plot(data$Sub_metering_1,xlab = "", ylab = "Energy sub-metering", type = "n", xaxt = "n")
lines(data$Sub_metering_1, col = "black")
lines(data$Sub_metering_2, col = "red")
lines(data$Sub_metering_3, col = "blue")
lines(data$Global_active_power)
axis(1, at = c(1, length(data$Sub_metering_1)/2, length(data$Sub_metering_1)), labels = c("Thu", "Fri", "Sat"))
legend("topright", lty = "solid", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")

plot(data$Global_reactive_power, xlab = "datetime", ylab = "Global reactive Power (Kilowatts)", type = "n", xaxt = "n")
lines(data$Global_reactive_power)
axis(1, at = c(1, length(data$Global_active_power)/2, length(data$Global_active_power)), labels = c("Thu", "Fri", "Sat"))

dev.off()
