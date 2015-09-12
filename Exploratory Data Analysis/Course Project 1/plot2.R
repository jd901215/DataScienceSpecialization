library(sqldf)

data <- read.csv2.sql(file = "household_power_consumption.txt", sql = "select * from file where Date='1/2/2007' or Date='2/2/2007'", header = TRUE)
data[[1]]<- strptime(paste(data[[1]], data[[2]]), format = "%d/%m/%Y %T")
data$Time <- NULL
png(filename = "plot2.png")
plot(data$Global_active_power, xlab = "" ,ylab = "Global Active Power (Kilowatts)", type = "n", xaxt = "n")
lines(data$Global_active_power) 
axis(1, at = c(1, length(data$Global_active_power)/2, length(data$Global_active_power)), labels = c("Thu", "Fri", "Sat"))
dev.off()
