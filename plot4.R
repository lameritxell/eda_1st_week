url_eda<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(url_eda, destfile = "~/cli/EDA/w1_project")
un_eda<-unzip("w1_project", exdir = "/home/didac/cli/EDA")

un_eda
eda<-read.table(un_eda, stringsAsFactors = TRUE)

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(reshape2)
library(grid)
library(gridExtra)
library(lattice)

eda_w<-separate(eda, col=1, c("Date", "Time", "Global_active_power", "Global_reactive_power","Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), sep = ";")
eda_w<-eda_w[2:2075260,] #all but 1st line
date<-eda_w[,1] #choose date var
a<-as.Date(date, "%d/%m/%Y") #transf to date format
head(a)
b<-grep("2007-02-01", a) #66637 til 68076 day 1
c<-grep("2007-02-02", a) #68077 til 69516 day 2
selected<- a[66637:69516] #selected rows

df_a<-eda_w[66637:69516,] #df selected rows
df_a_date<-cbind(df_a, selected) #date correct format (var selected)
names(df_a_date)

times<-c(df_a_date$Time)
dt<-paste(selected, times)
dt_format<-strptime(dt, "%Y-%m-%d %H:%M:%S")
new<-cbind(df_a_date, dt_format)


#plot2
new$Global_active_power<-as.numeric(new$Global_active_power)
a<-head(new$dt_format,1)
b<-tail(new$dt_format, 1)
lims <- as.POSIXct(c(a,b), format = "%Y-%m-%d %H:%M")
lims
plot2<-ggplot(data = new, aes(x=dt_format, y=Global_active_power)) + geom_line(size=1)+
                scale_x_datetime(labels = date_format("%a"), date_breaks = ("1 day"), limits = lims) +
                ylab("Global Active Power (kilowatts)") + xlab(NULL)

#plot3 to long format
long_new<-melt(new, id = c(1:6, 10, 11), measure.vars = c(7, 8, 9))
head(long_new)
long_new$value<-as.numeric(long_new$value)
plot3<-ggplot(data=long_new, aes(x=dt_format, y=value, col=variable)) + geom_line() + 
  scale_x_datetime(labels = date_format("%a"), date_breaks = ("1 day"), limits = lims) + 
  ylab("Energy submetering") + xlab(NULL) + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right", legend.title = element_blank()
  )

#plot4

new$Voltage<-as.numeric(new$Voltage)
plot_voltage<-ggplot(data = new, aes(x=dt_format, y=Voltage)) + geom_line(size=1)+
  scale_x_datetime(labels = date_format("%a"), date_breaks = ("1 day"), limits = lims) + xlab("datetime")
new$Global_reactive_power<-as.numeric(new$Global_reactive_power)
plot_reactive<-ggplot(data = new, aes(x=dt_format, y=Global_reactive_power)) + geom_line(size=1)+
  scale_x_datetime(labels = date_format("%a"), date_breaks = ("1 day"), limits = lims) + xlab("datetime")


upleft<-plot2
upright<-plot_voltage
downright<-plot3
downleft<-plot_reactive


plot4<-grid.arrange(upleft, upright, downright, downleft, ncol=2)

dev.copy(png,'plot4')
dev.off()
