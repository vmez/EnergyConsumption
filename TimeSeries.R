#Load libraries-----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(forecast)
library(reshape2)
####################################################################################
setwd("C:/Users/Violeta/Documents/Ubiqum/DeepAnalytics/Energy")

dataR <- read.csv("household_power_consumption.txt", sep = ";", na.strings = "?") 

household_power_consumption$DateTime<-paste(household_power_consumption$Date, household_power_consumption$Time)
household_power_consumption$DateTime<-strptime(household_power_consumption$DateTime,"%d/%m/%Y %H:%M:%S")
household_power_consumption$DateTime<-as.POSIXct(household_power_consumption$DateTime, tz="")

dataR$DateTime <- paste(dataR$Date,dataR$Time)
dataR$DateTime <- strptime(dataR$DateTime,"%d/%m/%Y %H:%M:%S")
dataR$DateTime <- as.POSIXct(dataR$DateTime,tz="")
dataR<-na.omit(dataR)

names(dataR)[7]<-'kitchen'
names(dataR)[8]<-'laundry'
names(dataR)[9]<-'whac'
dataR$Global_active_power<- dataR$Global_active_power*(1000/60)
names(dataR)[3]<-'GlobalEnergy'

dataR<-mutate(dataR,TotalSubmeters=kitchen+laundry+whac)
dataR<-mutate(dataR, MissingEnergy=dataR$GlobalEnergy-dataR$TotalSubmeters)
View(dataR)

#erasing NA
dataR<-na.omit(dataR)
str(dataR)

###################################################################################
###-TIME SERIES TUTORIAL- 2007,2008,2009 to make predictions#######################
TS2<-as.data.frame(dataR)
selectyears<-TS2[which(year(TS2$DateTime)==c(2007,2008,2009)),]

###-WINTER PREDICTIONS - 2007,2008,2009, YEAR/MONTH
winters<-selectyears[which(month(selectyears$DateTime)==c(12,1,2)),]
head(unique(winters))

winters<-mutate(winters, 
                month_year=paste(year(DateTime),
                                 formatC(month(DateTime), width = 2, flag="0")))

winterGE<-data.frame(winters$GlobalEnergy, winters$month_year)

head(winterGE)

winterGE<-group_by(winterGE, winters.month_year) %>% summarize(mean= mean(winters.GlobalEnergy))

head(winterGE)

ggplot(winterGE)+
  geom_line(aes(x=1:nrow(winterGE), y=winterGE$mean))+
  labs(x="Winter Months Across Three Years", y="Mean Global Energy Consumption")

WinterGETS<-ts(winterGE$mean, frequency = 3, start = c(1))
plot(WinterGETS)
Decomposed <- decompose(WinterGETS)
plot(Decomposed)

predictionWinter<-HoltWinters(WinterGETS, beta=FALSE, gamma = FALSE)
plot(predictionWinter)

################-SUMMER DAY/HOUR#######################
summer_dh<-selectyears%>% filter(quarter(DateTime)==3)%>%
  group_by(day(DateTime), hour(DateTime))%>%
  summarise_all(funs(mean))

head(unique(summer_dh))

tsummer_dh<-ts(summer_dh$GlobalEnergy, frequency = 24, start = c(1))
plot(tsummer_dh)

Decompose_summer<-decompose(tsummer_dh)
plot(Decompose_summer)

prediction<-HoltWinters(winterGE, beta=FALSE, gamma = FALSE)
plot(winterGE)


###############-WORKWEEK AUTUMNS 2007.2008,2009########
Autumns<-selectyears[which(month(selectyears$DateTime)==c(9,10,11)),]

Autumns<-Autumns[which(wday(Autumns$DateTime)==c("2", "3", "4", "5")),]

Autumns_work<-mutate(Autumns, week_day=paste(year(DateTime), 
                                             formatC(wday(DateTime), width = 2, flag="0")))

Autumns_workG<-group_by(Autumns_work, week_day)%>%summarize_all(funs(mean))
head(unique(Autumns_workG))

##-timeseries
tsAutumns_workG<-ts(Autumns_workG$GlobalEnergy, frequency=4)
plot(tsAutumns_workG)

##-decompose
DtdAutumns_workG<-decompose(tsAutumns_workG)
plot(DtdAutumns_workG)

##-HoltWinters from timeseries
predAG<-HoltWinters(tsAutumns_workG, beta= FALSE, gamma = TRUE, alpha = .5)
plot(predAG); grid()

##-Forecast from HoltWinters
fore <- forecast(predAG, h = 8 )
plot(fore)

##-using STL
stl.AG<-stl(tsAutumns_workG, s.window=5)
plot(stl.AG)
pred.AGsrl<-plot(forecast(stl.AG, h=8))

############-per equipment day in a week
september<-data2008[which(month(data2008$DateTime)==9),]
septemberW<-mutate(september, weekg=paste(hour(DateTime), formatC(day(DateTime))))
septemberW<-(group_by(septemberW, weekg)%>%
               summarise_all(funs(mean)))

#septemberW$weekg<-as.factor(septemberW$weekg)
HWuse<-ggplot(septemberW)+
  #geom_line(aes(weekg, kitchen), color='orange', group=1)+
  #geom_line(aes(weekg, laundry), color='purple',group=1)+
  geom_line(aes(weekg, whac), color='green4',group=1)+
  labs(x="Hourly Electrical Consumption in a Week", y="WHAC")
HWuse

###########################
septemberW<-september[which(wday(september$DateTime)==1),]
septemberW<-mutate(september, H=paste(hour(DateTime)))
septemberW<-group_by(septemberW, H)%>%summarize_all(funs(mean))
septemberW$H<-as.numeric(septemberW$H)
HWuse<-ggplot(septemberW)+
  geom_line(aes(H, kitchen), color='orange', group=1)+
  geom_line(aes(H, laundry), color='purple',group=1)+
  geom_line(aes(H, whac), color='green4',group=1)+
  labs(x="Sunday September 2009", y="")
HWuse

