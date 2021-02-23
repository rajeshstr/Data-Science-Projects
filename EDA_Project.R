data <- read.table('household_power_consumption.txt',sep=';',header=TRUE)
head(data)
nrow(data)
ncol(data)

data_v1 <- data[data[,'Date'] == '1/2/2007' | data[,'Date'] == '2/2/2007',]
nrow(data_v1)
ncol(data_v1)
head(data_v1)

# Plot 1:
data_v1$Global_active_power <- as.numeric(data_v1$Global_active_power)
hist(data_v1$Global_active_power,xlab='Global Active Power (kilowatts)',ylab = 'Frequency',main='Global Active Power',col='Red')

# Plot 2:
class(data_v1$Date)
data_v1$Date <- as.Date(data_v1$Date,format = '%d/%m/%Y')
class(data_v1$Time)
data_v1$Time <- strptime(data_v1$Time,format = '%H:%M:%S')

data_v1[data_v1[,'Date'] == '2007-02-01','Time'] <- format(data_v1[data_v1[,'Date'] == '2007-02-01','Time'],'2007-02-01 %H:%M:%S')
data_v1[data_v1[,'Date'] == '2007-02-02','Time'] <- format(data_v1[data_v1[,'Date'] == '2007-02-02','Time'],'2007-02-02 %H:%M:%S')

plot(data_v1$Time,data_v1$Global_active_power,type='l',xlab='Day of Week',ylab='Global Active Power (kilowatts)')

# Plot 3:
plot(data_v1$Time,data_v1$Sub_metering_1,type='n',xlab='Day of Week',ylab='Energy Sub Metering')
lines(data_v1$Time,as.numeric(data_v1$Sub_metering_1),col='Black')
lines(data_v1$Time,as.numeric(data_v1$Sub_metering_2),col='Red')
lines(data_v1$Time,as.numeric(data_v1$Sub_metering_3),col='Blue')
legend('topright',legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),col=c('Black','Red','Blue'),lty=1)


par(mfrow=c(2,2),mar=c(4,4,2,2))
with(data_v1){
  plot(data_v1$Time,data_v1$Global_active_power,type='l',xlab='',ylab='Global Active Power')
  plot(data_v1$Time,as.numeric(data_v1$Voltage),type='l',xlab='datetime',ylab='Voltage')
  plot(data_v1$Time,data_v1$Sub_metering_1,type='n',xlab='',ylab='Energy Sub Metering')
  lines(data_v1$Time,as.numeric(data_v1$Sub_metering_1),col='Black')
  lines(data_v1$Time,as.numeric(data_v1$Sub_metering_2),col='Red')
  lines(data_v1$Time,as.numeric(data_v1$Sub_metering_3),col='Blue')
  plot(data_v1$Time,as.numeric(data_v1$Global_reactive_power),type='l',xlab='datetime',ylab='Voltage')
  
}