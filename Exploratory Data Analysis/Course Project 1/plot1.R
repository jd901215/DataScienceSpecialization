library(sqldf)

data <- read.csv2.sql(file = "household_power_consumption.txt", sql = "select * from file where Date='1/2/2007' or Date='2/2/2007'", header = TRUE)
data[[1]]<- strptime(paste(data[[1]], data[[2]]), format = "%d/%m/%Y %T")
data$Time <- NULL
png(filename = "plot1.png")
hist(data$Global_active_power, breaks = 12, xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
dev.off()
