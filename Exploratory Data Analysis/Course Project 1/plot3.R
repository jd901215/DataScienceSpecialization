library(sqldf)

data <- read.csv2.sql(file = "household_power_consumption.txt", sql = "select * from file where Date='1/2/2007' or Date='2/2/2007'", header = TRUE)
data[[1]]<- strptime(paste(data[[1]], data[[2]]), format = "%d/%m/%Y %T")
data$Time <- NULL
par(mfrow = c(2, 2))

png(filename = "plot3.png")


plot(data$Sub_metering_1, xlab = "", ylab = "Energy sub-metering", type = "n", xaxt = "n")
lines(data$Sub_metering_1, col = "black")
lines(data$Sub_metering_2, col = "red")
lines(data$Sub_metering_3, col = "blue")
lines(data$Global_active_power)
axis(1, at = c(1, length(data$Sub_metering_1)/2, length(data$Sub_metering_1)), labels = c("Thu", "Fri", "Sat"))
legend("topright", lty = "solid", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()
