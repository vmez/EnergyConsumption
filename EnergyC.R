#Loading Packages----------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(forecast)


########Load Data ###########################

#Clean and Order ###########################

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


################-make sure setting limits accurately if want to specify
#ylim(0,round(max(c(week2010$kitchen, week2010$laundry, week2010$whac))) + 1)

############################################# day consumption, per month, in 2007
data2007_7<- data2007 %>% group_by(month(DateTime), day(DateTime), hour(DateTime)) %>% 
  summarise(kitchen = sum(kitchen),
            whac = sum(whac),
            laundry = sum(laundry), 
            TotalSubmeters=sum(TotalSubmeters),
            MissingEnergy=sum(MissingEnergy))

names(data2007_7)[1]<-'GroupByMonth'
names(data2007_7)[2]<-'GroupByDay'
names(data2007_7)[3]<-'GroupByHour'

############################################
theme(plot.background = element_rect(fill = "cornsilk.2"))

###################################################mean submeters and mean missing energy
View(month_agg)
c1=FUN=mean(month_agg$TotalSubmeters)
c2=FUN=mean(month_agg8$TotalSubmeters)
c3=FUN=mean(month_agg9$TotalSubmeters)
c4=FUN=mean(month_agg10$TotalSubmeters)

c1.1=FUN=mean(month_agg$MissingEnergy)
c1.2=FUN=mean(month_agg8$MissingEnergy)
c1.3=FUN=mean(month_agg9$MissingEnergy)
c1.4=FUN=mean(month_agg10$MissingEnergy)


######average Global Energy*12 to make it in a year
z1=FUN=mean(month_agg$GlobalEnergy)
z2=FUN=mean(month_agg8$GlobalEnergy)
z3=FUN=mean(month_agg9$GlobalEnergy)
z4=FUN=mean(month_agg10$GlobalEnergy)

z1
z2
z3
z4

##############################this step not needed when na.strings in code ## as.numeric to convert to numerical value
str(dataG)
dataG$Global_active_power<-as.numeric(as.character(dataG$Global_active_power))
dataG$Sub_metering_1<-as.numeric(as.character(dataG$kitchen))
dataG$Sub_metering_2<-as.numeric(as.character(dataG$laundry))
dataG$Global_reactive_power<-as.numeric(as.character(dataG$Global_reactive_power))
dataG$Global_intensity<-as.numeric(as.character(dataG$Global_intensity))
str(dataG)

#######################################global active power overall in kWh per year
sum((dataR$GlobalEnergy)/47)*12/(1000)
sum((dataR$kitchen+dataR$laundry+dataR$whac)/47)*12/(1000)


#######################################################-data.frame years
data2007<-dataR[which(year(dataR$DateTime)==c(2007)),]
data2008<-dataR[which(year(dataR$DateTime)==c(2008)),]
data2009<-dataR[which(year(dataR$DateTime)==c(2009)),]
data2010<-dataR[which(year(dataR$DateTime)==c(2010)),]
#######################################################


#######################################################create data.frame for year 2007
data2007<-dataR[which(year(dataR$DateTime)==c(2007)),]
data2007<-mutate(data2007, month_tag=as.numeric(format(data2007$DateTime, "%m")))
View(data2007)
month_agg7<-aggregate(data2007, by=list(data2007$month_tag), FUN = mean)
names(month_agg7)[1]<-'Month2007'
month_agg$Month2007<-as.factor(month_agg$Month2007)
View(month_agg7)
Year2007<-ggplot()+geom_line(data=month_agg7, aes(Month2007, TotalSubmeters), color="black")+
  geom_line(data=month_agg7, aes(Month2007, MissingEnergy), color='red')+
  geom_line(data=month_agg7, aes(Month2007, GlobalEnergy), color='blue')+
  labs( y = "", x = "2007")+ #title="Difference in Energy Recordings for year 2007",
  ylim(0,40)
Year2007

#################################################### create data.frame for year 2008
data2008<-dataR[which(year(dataR$DateTime)==c(2008)),]
data2008<-mutate(data2008, month_tag=as.numeric(format(data2008$DateTime, "%m")))
View(data2008)
month_agg8<-aggregate(data2008, by=list(data2008$month_tag), FUN = mean)
names(month_agg8)[1]<-'Month2008'
month_agg$Month2008<-as.factor(month_agg$Month2008)
View(month_agg8)
Year2008<-ggplot()+geom_line(data=month_agg8, aes(Month2008, TotalSubmeters), color="black")+
  geom_line(data=month_agg8, aes(Month2008, MissingEnergy), color='red')+
  geom_line(data=month_agg8, aes(Month2008, GlobalEnergy), color='blue')+
  labs( y = "", x = "2008")+
  # labs(title="Difference in Energy Recordings for year 2008", ylab("Consumption in Submeters"), xlab("Month"))+
  ylim(0,40)
Year2008

###################################################create data.frame for year 2009
data2009<-dataR[which(year(dataR$DateTime)==c(2009)),]
data2009<-mutate(data2009, month_tag=as.numeric(format(data2009$DateTime, "%m")))
View(data2009)
month_agg9<-aggregate(data2009, by=list(data2009$month_tag), FUN = mean)
names(month_agg9)[1]<-'Month2009'
month_agg$Month2009<-as.factor(month_agg$Month2009)
View(month_agg9)
Year2009<-ggplot()+geom_line(data=month_agg9, aes(Month2009, TotalSubmeters), color="black")+
  geom_line(data=month_agg9, aes(Month2009, MissingEnergy), color='red')+
  geom_line(data=month_agg9, aes(Month2009, GlobalEnergy), color='blue')+
  labs( y = "", x = "2009")+
  #labs(title="Difference in Energy Recordings for year 2009", ylab("Consumption in Submeters"), xlab("Month"))+
  ylim(0,40)
Year2009

###################################################create a data.frame for year2010
data2010<-dataR[which(year(dataR$DateTime)==c(2010)),]
data2010<-mutate(data2010, month_tag=as.numeric(format(data2010$DateTime, "%m")))
View(data2010)
month_agg10<-aggregate(data2010, by=list(data2010$month_tag), FUN = mean)
names(month_agg10)[1]<-'Month2010'
month_agg$Month2010<-as.factor(month_agg$Month2010)
View(month_agg10)
Year2010<-ggplot()+geom_line(data=month_agg10, aes(Month2010,TotalSubmeters), color ="black")+
  geom_line(data=month_agg10, aes(Month2010, MissingEnergy), color='red')+
  geom_line(data=month_agg10, aes(Month2010, GlobalEnergy), color='blue')+
  labs( y = "",x = "2010")+
  scale_colour_manual(name='',values=c('TotalSubmeters'='black','MissingEnergy'='red', 'GlobalEnergy'='blue'))+
  #scale_color_discrete(name = "LABELS", labels = c("Total Submeters", "MissingEnergy","GlobalEnergy"))+
  #labs(title="Difference in Energy Recordings for year 2010", ylab("Consumption in Submeters"), xlab("Month"))+
  ylim(0,40)
Year2010

##################-GRID ARRANGE-#######################
grid.arrange(Year2007, Year2008, Year2009, Year2010, ncol=4, 
             top = "DIFFERENCE OF AVERAGE ENERGY CONSUMPTION", 
             left='Consumption in wH', 
             bottom='Average Consumption per Year')

#####################################3PLOTTING 2007 AV. CONSUMPTION IN WATS PER MONTH IN 2007 EACH SUBMETER
g7<-ggplot()+geom_line(aes(month_agg7$Month2007, month_agg7$kitchen), color="orange", group=1)+
  geom_line(aes(month_agg7$Month2007, month_agg7$laundry), color="purple", group=1)+
  geom_line(aes(month_agg7$Month2007, month_agg7$whac), color="green4", group=1)+
  ylim(0,15)+
  labs(y="", x="2007")
#labs(title="Averange Submeter Consumption per Month in 2007", x="Month", y="Consumption in wats per hour")
g7
#PLOTTING 2008 AV. MINUTE CONSUMPTION IN WATS PER MONTH IN 2008 EACH SUBMETER
g8<-ggplot()+geom_line(aes(month_agg8$Month2008, month_agg8$kitchen), color="orange", group=1)+
  geom_line(aes(month_agg8$Month2008, month_agg8$laundry), color="purple", group=1)+
  geom_line(aes(month_agg8$Month2008, month_agg8$whac), color="green4", group=1)+
  ylim(0,15)+
  labs(y="", x="2008")
#labs(title="Averange Submeter Consumption per Month in 2008", x="Month", y="Consumption in wats per hour")
g8
#PLOTTING 2009 AV. MINUTE CONSUMPTION IN WATS PER MONTH IN 2009 EACH SUBMETER
g9<-ggplot()+geom_line(aes(month_agg9$Month2009, month_agg9$kitchen), color="orange", group=1)+
  geom_line(aes(month_agg9$Month2009, month_agg9$laundry), color="purple", group=1)+
  geom_line(aes(month_agg9$Month2009, month_agg9$whac), color="green4", group=1)+
  ylim(0,15)+
  labs(y="", x="2009")
#labs(title="Averange Submeter Consumption per Month in 2009", x="Month", y="Consumption in wats per hour")
g9  
#PLOTTING 2010 AV. MINUTE CONSUMPTION IN WATS PER MONTH IN 2010 EACH SUBMETER
g10<-ggplot()+geom_line(aes(month_agg10$Month2010, month_agg10$kitchen), color="orange", group=1)+
  geom_line(aes(month_agg10$Month2010, month_agg10$laundry), color="purple", group=1)+
  geom_line(aes(month_agg10$Month2010, month_agg10$whac), color="green4", group=1)+
  ylim(0,15)+
  labs(y="", x="2010")
#labs(title="Averange Submeter Consumption per Month in 2010", x="Month", y="Consumption in wats per hour")
g10
##############################
grid.arrange(g7, g8, g9, g10, ncol=4, 
             top = "COMPARING CONSUMPTION OF SUBMETERS", 
             left='Consumption in wH', 
             bottom='Average Consumption per Month')


###############################################- WEEK AVERAGE-##################################################
########-WEEKS IN 2007
data2007<-dataR[which(year(dataR$DateTime)==c(2007)),]
View(data2007)
data2007 <- mutate(data2007, week_day = format(wday(data2007$DateTime)))
week2007<-aggregate(data2007, by=list(data2007$week_day), FUN=mean)
names(week2007)[1]<-'WeekDay'
View(week2007)

gweek2007<-ggplot()+geom_line(aes(week2007$WeekDay,week2007$kitchen), color='orange', group=1)+
  geom_line(aes(week2007$WeekDay,week2007$laundry), color='purple', group=1)+
  geom_line(aes(week2007$WeekDay,week2007$whac), color='green4', group=1)+
  labs(y="", x="2007")+
  ylim(0,10)
gweek2007

######- WEEKS IN 2008
data2008<-dataR[which(year(dataR$DateTime)==c(2008)),]
View(data2008)
data2008 <- mutate(data2008, week_day = format(wday(data2008$DateTime)))
week2008<-aggregate(data2008, by=list(data2008$week_day), FUN=mean)
names(week2008)[1]<-'WeekDay'
View(week2008)

gweek2008<-ggplot()+geom_line(aes(week2008$WeekDay,week2008$kitchen), color='orange', group=1)+
  geom_line(aes(week2008$WeekDay,week2008$laundry), color='purple', group=1)+
  geom_line(aes(week2008$WeekDay,week2008$whac), color='green4', group=1)+
  labs(y="", x="2008")+
  ylim(0,10)
gweek2008

######- WEEKS IN 2009
data2009<-dataR[which(year(dataR$DateTime)==c(2009)),]
data2009 <- mutate(data2009, week_day = format(wday(data2009$DateTime)))
week2009<-aggregate(data2009, by=list(data2009$week_day), FUN=mean)
names(week2009)[1]<-'WeekDay'
View(week2009)

gweek2009<-ggplot()+geom_line(aes(week2009$WeekDay,week2009$kitchen), color='orange', group=1)+
  geom_line(aes(week2009$WeekDay,week2009$laundry), color='purple', group=1)+
  geom_line(aes(week2009$WeekDay,week2009$whac), color='green4', group=1)+
  labs(y="", x="2009")+
  ylim(0,10)
gweek2009

######- WEEKS IN 2010
data2010<-dataR[which(year(dataR$DateTime)==c(2010)),]
data2010 <- mutate(data2010, week_day = format(wday(data2010$DateTime)))
week2010<-aggregate(data2010, by=list(data2010$week_day), FUN=mean)
names(week2010)[1]<-'WeekDay'
View(week2010)

gweek2010<-ggplot()+geom_line(aes(week2010$WeekDay,week2010$kitchen), color='orange', group=1)+
  geom_line(aes(week2010$WeekDay,week2010$laundry), color='purple', group=1)+
  geom_line(aes(week2010$WeekDay,week2010$whac), color='green4', group=1)+
  labs(y="", x="2010")+
  ylim(0,10)
gweek2010
######################-ALL WEEKS######
grid.arrange(gweek2007, gweek2008, gweek2009, gweek2010, ncol=4, 
             top = "COMPARING CONSUMPTION OF SUBMETERS", 
             left='Consumption in wH', 
             bottom='Average Weekly Consumption')


#########-JANUARY 2007
January2007<-data2007[which(month(data2007$DateTime)==c(1)),]
unique(month(January2007$DateTime))
January2007<-mutate(January2007, hour(DateTime))
Hour2007<-aggregate(January2007, by=list(January2007$`hour(DateTime)`), FUN=mean)
names(Hour2007)[1]<-'Hour'
ghour2007<-ggplot()+geom_line(aes(Hour2007$Hour,Hour2007$kitchen), color='orange', group=1)+
  geom_line(aes(Hour2007$Hour, Hour2007$laundry), color='purple', group=1)+
  geom_line(aes(Hour2007$Hour, Hour2007$whac), color='green4', group=1)+
  labs(y="", x='2007')+
  ylim(0,20)
ghour2007

#########-JANUARY 2008
January2008<-data2008[which(month(data2008$DateTime)==c(1)),]
January2008<-mutate(January2008, hour(DateTime))
Hour2008<-aggregate(January2008, by=list(January2008$`hour(DateTime)`), FUN=mean)
names(Hour2008)[1]<-'Hour'
ghour2008<-ggplot()+geom_line(aes(Hour2008$Hour,Hour2008$kitchen), color='orange', group=1)+
  geom_line(aes(Hour2008$Hour, Hour2008$laundry), color='purple', group=1)+
  geom_line(aes(Hour2008$Hour, Hour2008$whac), color='green4', group=1)+
  labs(y="", x='2008')+
  ylim(0,20)
ghour2008
#########-JANUARY 2009
January2009<-data2009[which(month(data2009$DateTime)==c(1)),]
January2009<-mutate(January2009, hour(DateTime))
Hour2009<-aggregate(January2009, by=list(January2009$`hour(DateTime)`), FUN=mean)
names(Hour2009)[1]<-'Hour'
ghour2009<-ggplot()+geom_line(aes(Hour2009$Hour,Hour2009$kitchen), color='orange', group=1)+
  geom_line(aes(Hour2009$Hour, Hour2009$laundry), color='purple', group=1)+
  geom_line(aes(Hour2009$Hour, Hour2009$whac), color='green4', group=1)+
  labs(y="", x='2009')+
  ylim(0,20)
ghour2009
########-JANUARY 2010
January2010<-data2010[which(month(data2010$DateTime)==c(1)),]
January2010<-mutate(January2010, hour(DateTime))
Hour2010<-aggregate(January2010, by=list(January2010$`hour(DateTime)`), FUN=mean)
names(Hour2010)[1]<-'Hour'
ghour2010<-ggplot()+geom_line(aes(Hour2010$Hour,Hour2010$kitchen), color='orange', group=1)+
  geom_line(aes(Hour2010$Hour, Hour2010$laundry), color='purple', group=1)+
  geom_line(aes(Hour2010$Hour, Hour2010$whac), color='green4', group=1)+
  labs(y="", x='2010')+
  ylim(0,20)
ghour2010
#####################-ALL JANUARY############
grid.arrange(ghour2007, ghour2008, ghour2009, ghour2010, ncol=4, 
             top = "COMPARING CONSUMPTION OF SUBMETERS", 
             left='Consumption in wH', 
             bottom='Average Consumption in January')

######################################################################################-WINTER SEASONS
####winter2007
winter2007<-data2007[which(month(data2007$DateTime)==c(12,1,2)),]
Hwinter2007<-mutate(winter2007, hour(DateTime))
Hwinter2007<-aggregate(Hwinter2007, by=list(Hwinter2007$`hour(DateTime)`), FUN=mean)
names(Hwinter2007)[1]<-'Hour'
gHwinter2007<-ggplot()+geom_line(aes(Hwinter2007$Hour, Hwinter2007$kitchen), color='orange', group=1)+
  geom_line(aes(Hwinter2007$Hour, Hwinter2007$laundry), color='purple', group=1)+
  geom_line(aes(Hwinter2007$Hour, Hwinter2007$whac), color='green4', group=1)+
  geom_line(aes(Hwinter2007$Hour, Hwinter2007$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2007 (Total Consumption 611.52WH)")+
  ylim(0,50)
gHwinter2007

###winter2008
winter2008<-data2008[which(month(data2008$DateTime)==c(12,1,2)),]
Hwinter2008<-mutate(winter2008, hour(DateTime))
Hwinter2008<-aggregate(Hwinter2008, by=list(Hwinter2008$`hour(DateTime)`), FUN=mean)
names(Hwinter2008)[1]<-'Hour'
gHwinter2008<-ggplot()+geom_line(aes(Hwinter2008$Hour, Hwinter2008$kitchen), color='orange', group=1)+
  geom_line(aes(Hwinter2008$Hour, Hwinter2008$laundry), color='purple', group=1)+
  geom_line(aes(Hwinter2008$Hour, Hwinter2008$whac), color='green4', group=1)+
  geom_line(aes(Hwinter2008$Hour, Hwinter2008$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2008 (Total Consumption 523.05WH)")+
  ylim(0,50)
gHwinter2008

###winter2009
winter2009<-data2009[which(month(data2009$DateTime)==c(12,1,2)),]
Hwinter2009<-mutate(winter2009, hour(DateTime))
Hwinter2009<-aggregate(Hwinter2009, by=list(Hwinter2009$`hour(DateTime)`), FUN=mean)
names(Hwinter2009)[1]<-'Hour'
gHwinter2009<-ggplot()+geom_line(aes(Hwinter2009$Hour, Hwinter2009$kitchen), color='orange', group=1)+
  geom_line(aes(Hwinter2009$Hour, Hwinter2009$laundry), color='purple', group=1)+
  geom_line(aes(Hwinter2009$Hour, Hwinter2009$whac), color='green4', group=1)+
  geom_line(aes(Hwinter2009$Hour, Hwinter2009$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x='2009 (Total Consumption 537.20WH)')+
  ylim(0,50)
gHwinter2009

###winter2010
winter2010<-data2010[which(month(data2010$DateTime)==c(12,1,2)),]
Hwinter2010<-mutate(winter2010, hour(DateTime))
Hwinter2010<-aggregate(Hwinter2010, by=list(Hwinter2010$`hour(DateTime)`), FUN=mean)
names(Hwinter2010)[1]<-'Hour'
gHwinter2010<-ggplot()+geom_line(aes(Hwinter2010$Hour, Hwinter2010$kitchen), color='orange', group=1)+
  geom_line(aes(Hwinter2010$Hour, Hwinter2010$laundry), color='purple', group=1)+
  geom_line(aes(Hwinter2010$Hour, Hwinter2010$whac), color='green4', group=1)+
  geom_line(aes(Hwinter2010$Hour, Hwinter2010$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x='2010 (Total Consumption 561.51WH)')+
  ylim(0,50)
gHwinter2010

#####ALL WINTERS
Winters<-grid.arrange(gHwinter2007, gHwinter2008, gHwinter2009, gHwinter2010, ncol=4,
                      top = "WINTER")
#left='Consumption in wH',
#bottom='Average Consumption in Winter')

######################################################################################-SPRING SEASONS
###spring2007
spring2007<-data2007[which(month(data2007$DateTime)==c(3,4,5)),]
Hspring2007<-mutate(spring2007, hour(DateTime))
Hspring2007<-aggregate(Hspring2007, by=list(Hspring2007$`hour(DateTime)`), FUN=mean)
names(Hspring2007)[1]<-'Hour'
gHspring2007<-ggplot()+geom_line(aes(Hspring2007$Hour, Hspring2007$kitchen), color='orange', group=1)+
  geom_line(aes(Hspring2007$Hour, Hspring2007$laundry), color='purple', group=1)+
  geom_line(aes(Hspring2007$Hour, Hspring2007$whac), color='green4', group=1)+
  geom_line(aes(Hspring2007$Hour, Hspring2007$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2007 (Total Consumption 428.02WH)")+
  ylim(0,50)
gHspring2007

###spring2008
spring2008<-data2008[which(month(data2008$DateTime)==c(3,4,5)),]
Hspring2008<-mutate(spring2008, hour(DateTime))
Hspring2008<-aggregate(Hspring2008, by=list(Hspring2008$`hour(DateTime)`), FUN=mean)
names(Hspring2008)[1]<-'Hour'
gHspring2008<-ggplot()+geom_line(aes(Hspring2008$Hour, Hspring2008$kitchen), color='orange', group=1)+
  geom_line(aes(Hspring2008$Hour, Hspring2008$laundry), color='purple', group=1)+
  geom_line(aes(Hspring2008$Hour, Hspring2008$whac), color='green4', group=1)+
  geom_line(aes(Hspring2008$Hour, Hspring2008$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2008 (Total Consumption 451.62WH)")+
  ylim(0,50)
gHspring2008

###spring2009
spring2009<-data2009[which(month(data2009$DateTime)==c(3,4,5)),]
Hspring2009<-mutate(spring2009, hour(DateTime))
Hspring2009<-aggregate(Hspring2009, by=list(Hspring2009$`hour(DateTime)`), FUN=mean)
names(Hspring2009)[1]<-'Hour'
gHspring2009<-ggplot()+geom_line(aes(Hspring2009$Hour, Hspring2009$kitchen), color='orange', group=1)+
  geom_line(aes(Hspring2009$Hour, Hspring2009$laundry), color='purple', group=1)+
  geom_line(aes(Hspring2009$Hour, Hspring2009$whac), color='green4', group=1)+
  geom_line(aes(Hspring2009$Hour, Hspring2009$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2009 (Total Consumption 450.32WH)")+
  ylim(0,50)
gHspring2009

###spring2010
spring2010<-data2010[which(month(data2010$DateTime)==c(3,4,5)),]
Hspring2010<-mutate(spring2010, hour(DateTime))
Hspring2010<-aggregate(Hspring2010, by=list(Hspring2010$`hour(DateTime)`), FUN=mean)
names(Hspring2010)[1]<-'Hour'
gHspring2010<-ggplot()+geom_line(aes(Hspring2010$Hour, Hspring2010$kitchen), color='orange', group=1)+
  geom_line(aes(Hspring2010$Hour, Hspring2010$laundry), color='purple', group=1)+
  geom_line(aes(Hspring2010$Hour, Hspring2010$whac), color='green4', group=1)+
  geom_line(aes(Hspring2010$Hour, Hspring2010$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2010 (Total Consumption 433.4WH)")+
  ylim(0,50)
gHspring2010
###########-ALL SPRING
Springs<-grid.arrange(gHspring2007, gHspring2008, gHspring2009, gHspring2010, ncol=4,
                      top = "SPRING") 
#left='Consumption in wH', 
#bottom='Average Consumption in Spring')

######################################################################################-SUMMER SEASONS
###SUMMER 2007
summer2007<-data2007[which(month(data2007$DateTime)==c(6,7,8)),]
Hsummer2007<-mutate(summer2007, hour(DateTime))
Hsummer2007<-aggregate(Hsummer2007, by=list(Hsummer2007$`hour(DateTime)`), FUN=mean)
names(Hsummer2007)[1]<-'Hour'
gHsummer2007<-ggplot()+geom_line(aes(Hsummer2007$Hour, Hsummer2007$kitchen), color='orange', group=1)+
  geom_line(aes(Hsummer2007$Hour, Hsummer2007$laundry), color='purple', group=1)+
  geom_line(aes(Hsummer2007$Hour, Hsummer2007$whac), color='green4', group=1)+
  geom_line(aes(Hsummer2007$Hour, Hsummer2007$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2007 (Total Consumption 300.85WH)")+
  ylim(0,50)
gHsummer2007

###SUMMER 2008
summer2008<-data2008[which(month(data2008$DateTime)==c(6,7,8)),]
Hsummer2008<-mutate(summer2008, hour(DateTime))
Hsummer2008<-aggregate(Hsummer2008, by=list(Hsummer2008$`hour(DateTime)`), FUN=mean)
names(Hsummer2008)[1]<-'Hour'
gHsummer2008<-ggplot()+geom_line(aes(Hsummer2008$Hour, Hsummer2008$kitchen), color='orange', group=1)+
  geom_line(aes(Hsummer2008$Hour, Hsummer2008$laundry), color='purple', group=1)+
  geom_line(aes(Hsummer2008$Hour, Hsummer2008$whac), color='green4', group=1)+
  geom_line(aes(Hsummer2008$Hour, Hsummer2008$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2008 (Total Consumption 274.02WH)")+
  ylim(0,50)
gHsummer2008

###SUMMER 2009
summer2009<-data2009[which(month(data2009$DateTime)==c(6,7,8)),]
Hsummer2009<-mutate(summer2009, hour(DateTime))
Hsummer2009<-aggregate(Hsummer2009, by=list(Hsummer2009$`hour(DateTime)`), FUN=mean)
names(Hsummer2009)[1]<-'Hour'
gHsummer2009<-ggplot()+geom_line(aes(Hsummer2009$Hour, Hsummer2009$kitchen), color='orange', group=1)+
  geom_line(aes(Hsummer2009$Hour, Hsummer2009$laundry), color='purple', group=1)+
  geom_line(aes(Hsummer2009$Hour, Hsummer2009$whac), color='green4', group=1)+
  geom_line(aes(Hsummer2009$Hour, Hsummer2009$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2009 (Total Consumption 281.12)")+
  ylim(0,50)
gHsummer2009

###SUMMER 2010
summer2010<-data2010[which(month(data2010$DateTime)==c(6,7,8)),]
Hsummer2010<-mutate(summer2010, hour(DateTime))
Hsummer2010<-aggregate(Hsummer2010, by=list(Hsummer2010$`hour(DateTime)`), FUN=mean)
names(Hsummer2010)[1]<-'Hour'
gHsummer2010<-ggplot()+geom_line(aes(Hsummer2010$Hour, Hsummer2010$kitchen), color='orange', group=1)+
  geom_line(aes(Hsummer2010$Hour, Hsummer2010$laundry), color='purple', group=1)+
  geom_line(aes(Hsummer2010$Hour, Hsummer2010$whac), color='green4', group=1)+
  geom_line(aes(Hsummer2010$Hour, Hsummer2010$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2010 (Total Consumption 307.42WH)")+
  ylim(0,50)
gHsummer2010

######-ALL SUMMER
Summers<-grid.arrange(gHsummer2007, gHsummer2008, gHsummer2009, gHsummer2010, ncol=4,
                      top = "SUMMER") 
#left='Consumption in wH', 
#bottom='Average Consumption in Summer')

######################################################################################-AUTUMN SEASONS
###AUTUMN 2007
autumn2007<-data2007[which(month(data2007$DateTime)==c(9,10,11)),]
Hautumn2007<-mutate(autumn2007, hour(DateTime))
Hautumn2007<-aggregate(Hautumn2007, by=list(Hautumn2007$`hour(DateTime)`), FUN=mean)
names(Hautumn2007)[1]<-'Hour'
gHautumn2007<-ggplot()+geom_line(aes(Hautumn2007$Hour, Hautumn2007$kitchen), color='orange', group=1)+
  geom_line(aes(Hautumn2007$Hour, Hautumn2007$laundry), color='purple', group=1)+
  geom_line(aes(Hautumn2007$Hour, Hautumn2007$whac), color='green4', group=1)+
  geom_line(aes(Hautumn2007$Hour, Hautumn2007$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2007 (Total Consumption is 449WH)")+
  ylim(0,50)
gHautumn2007

###AUTUMN 2008
autumn2008<-data2008[which(month(data2008$DateTime)==c(9,10,11)),]
Hautumn2008<-mutate(autumn2008, hour(DateTime))
Hautumn2008<-aggregate(Hautumn2008, by=list(Hautumn2008$`hour(DateTime)`), FUN=mean)
names(Hautumn2008)[1]<-'Hour'
gHautumn2008<-ggplot()+geom_line(aes(Hautumn2008$Hour, Hautumn2008$kitchen), color='orange', group=1)+
  geom_line(aes(Hautumn2008$Hour, Hautumn2008$laundry), color='purple', group=1)+
  geom_line(aes(Hautumn2008$Hour, Hautumn2008$whac), color='green4', group=1)+
  geom_line(aes(Hautumn2008$Hour, Hautumn2008$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2008 (Total Consumption is 468.04WH)")+
  ylim(0,50)
gHautumn2008

###AUTUMN 2009
autumn2009<-data2009[which(month(data2009$DateTime)==c(9,10,11)),]
Hautumn2009<-mutate(autumn2009, hour(DateTime))
Hautumn2009<-aggregate(Hautumn2009, by=list(Hautumn2009$`hour(DateTime)`), FUN=mean)
names(Hautumn2009)[1]<-'Hour'
gHautumn2009<-ggplot()+geom_line(aes(Hautumn2009$Hour, Hautumn2009$kitchen), color='orange', group=1)+
  geom_line(aes(Hautumn2009$Hour, Hautumn2009$laundry), color='purple', group=1)+
  geom_line(aes(Hautumn2009$Hour, Hautumn2009$whac), color='green4', group=1)+
  geom_line(aes(Hautumn2009$Hour, Hautumn2009$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2009 (Total Consumption 454.27WH)")+
  ylim(0,50)
gHautumn2009

###AUTUMN 2010
autumn2010<-data2010[which(month(data2010$DateTime)==c(9,10,11)),]
Hautumn2010<-mutate(autumn2010, hour(DateTime))
Hautumn2010<-aggregate(Hautumn2010, by=list(Hautumn2010$`hour(DateTime)`), FUN=mean)
names(Hautumn2010)[1]<-'Hour'
gHautumn2010<-ggplot()+geom_line(aes(Hautumn2010$Hour, Hautumn2010$kitchen), color='orange', group=1)+
  geom_line(aes(Hautumn2010$Hour, Hautumn2010$laundry), color='purple', group=1)+
  geom_line(aes(Hautumn2010$Hour, Hautumn2010$whac), color='green4', group=1)+
  geom_line(aes(Hautumn2010$Hour, Hautumn2010$GlobalEnergy), color='coral2', group=1)+
  labs(y="", x="2010 (Total Consumption 443.64WH)")+
  ylim(0,50)
gHautumn2010

####-ALL AUTUMN
Autumns<-grid.arrange(gHautumn2007, gHautumn2008, gHautumn2009, gHautumn2010, ncol=4,
                      top = "AUTUMN")
#left='Consumption in wH', 
# bottom='Average Consumption in Autumn')


##############-COMBINING SEASONS ACROSS YEARS
grid.arrange(Winters, Springs, Summers, Autumns, ncol=1, nrow=4,
             top="SEASONAL ENERGY CONSUMPTION", 
             left="wats per hour")

###################
Winters1<-grid.arrange(gHwinter2007, gHwinter2008, gHwinter2009, gHwinter2010, ncol=4,
                       bottom='Winter')
Springs1<-grid.arrange(gHspring2007, gHspring2008, gHspring2009, gHspring2010, ncol=4,
                       bottom='Spring')
Summers1<-grid.arrange(gHsummer2007, gHsummer2008, gHsummer2009, gHsummer2010, ncol=4,
                       bottom='Summer')
Autumns1<-grid.arrange(gHautumn2007, gHautumn2008, gHautumn2009, gHautumn2010, ncol=4,
                       bottom='Autumn')

Seasons<-grid.arrange(Winters1, Springs1, Summers1, Autumns1, ncol=2, nrow=2,
                      top="SUBMETER CONSUMPTION PER SEASON",
                      left="Consumption in wH",
                      bottom="Average consumption/hour")

##########################-
##########################- AGGREGATE DAY OF WEEK AND HOUR AND EXTRACT WEEK ON PLOT
autumn2008<-data2008[which(month(data2008$DateTime)==c(9,10,11)),]
avweek<-mutate(autumn2008, weekdays(DateTime))
weekend<-avweek[which(weekdays(avweek$DateTime)==c('Friday', 'Saturday', 'Sunday')),]
names(weekend)[13]<-'WeekDays'
Hweekend<-mutate(weekend, hour(DateTime))
Hweekend<-aggregate(Hweekend, by=list(Hweekend$WeekDays, Hweekend$`hour(DateTime)`), FUN=mean)
names(Hweekend)[1]<-'WeekDay'
names(Hweekend)[2]<-'Hour'
Autumn2008missing<-ggplot(Hweekend) +geom_line(aes(Hour, GlobalEnergy), color='coral2') + 
  geom_line(aes(Hour, whac), color='green4')+
  geom_line(aes(Hour, kitchen), color='orange')+
  geom_line(aes(Hour, laundry), color='purple')+
  labs(y="", x="S.Consumption 696WH")+
  ylim(0,60)+
  facet_grid(WeekDay ~ .)
Autumn2008missing

###################- WORKDAYS
WorkDays<-avweek[which(weekdays(avweek$DateTime)==c('Monday','Tuesday','Wednesday','Thursday')),]
names(WorkDays)[13]<-'WeekDays'
HWorkDays<-mutate(WorkDays, hour(DateTime))
HWorkDays<-aggregate(HWorkDays, by=list(HWorkDays$WeekDays, HWorkDays$`hour(DateTime)`), FUN=mean)
names(HWorkDays)[1]<-'WeekDay'
names(HWorkDays)[2]<-'Hour'
workAutumn2008missing<-ggplot(HWorkDays) + geom_line(aes(Hour, GlobalEnergy), color='coral2') + 
  geom_line(aes(Hour, whac), color='green4')+
  geom_line(aes(Hour, kitchen), color='orange')+
  geom_line(aes(Hour, laundry), color='purple')+
  labs(y="", x="S.Consumption 791WH")+
  ylim(0,60)+
  facet_grid(WeekDay ~.)
workAutumn2008missing
####
grid.arrange(workAutumn2008missing,Autumn2008missing, nrow=1, top='Autumn 2008, Submeter Division')

#################################
gAutumn2010<-grid.arrange(gWorkDays, gweekend, nrow=1, top='Autumn 2010')
grid.arrange(gWinter2007, gSpring2007, gSummer2007, gAutumn2007,
             gWinter2008, gSpring2008, gSummer2008, gAutumn2008, 
             gWinter2009, gSpring2009, gSummer2009, gAutumn2009,
             gWinter2010, gSpring2010, gSummer2010, gAutumn2010,
             nrow=4, ncol=4, 
             top='Seasonal Consumption')
Winters<-grid.arrange(gWinter2007, gWinter2008, gWinter2009, gWinter2010, nrow=1)
Springs<-grid.arrange(gSpring2007, gSpring2008, gSpring2009, gSpring2010, nrow=1)
Summers<-grid.arrange(gSummer2007, gSummer2008, gSummer2009, gSummer2010, nrow=1)
Autumns<-grid.arrange(gAutumn2007, gAutumn2008, gAutumn2009, gAutumn2010, nrow=1)
grid.arrange(Winters, Springs, Summers, Autumns, nrow=4, ncol=1)

#################
# Pie Chart 2007
a<-data.frame(dataR$DateTime, dataR$MissingEnergy, dataR$kitchen, dataR$laundry, dataR$whac)

a2007<-a[which(year(dataR$DateTime)==c(2007)),]
slices<-c(sum(a2007$dataR.MissingEnergy),sum(a2007$dataR.kitchen), sum(a2007$dataR.laundry), sum(a2007$dataR.whac))
lbls <- c("MissingEnergy", "Kitchen", "Laundry", "Whac")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Energy Consumption 2007", init.angle = 315)

a2008<-a[which(year(dataR$DateTime)==c(2008)),]
slices<-c(sum(a2008$dataR.MissingEnergy),sum(a2008$dataR.kitchen), sum(a2008$dataR.laundry), sum(a2008$dataR.whac))
lbls <- c("MissingEnergy", "Kitchen", "Laundry", "Whac")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Energy Consumption 2008", init.angle = 315)


a2009<-a[which(year(dataR$DateTime)==c(2009)),]
slices<-c(sum(a2009$dataR.MissingEnergy),sum(a2009$dataR.kitchen), sum(a2009$dataR.laundry), sum(a2009$dataR.whac))
lbls <- c("MissingEnergy", "Kitchen", "Laundry", "Whac")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Energy Consumption 2009",init.angle = 315)

a2010<-a[which(year(dataR$DateTime)==c(2010)),]
slices<-c(sum(a2010$dataR.MissingEnergy),sum(a2010$dataR.kitchen), sum(a2010$dataR.laundry), sum(a2010$dataR.whac))
lbls <- c("MissingEnergy", "Kitchen", "Laundry", "Whac")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Energy Consumption 2010",init.angle = 315)


#######################-COOL PIE-#################
MissingEnergy1<-sum(a2007$dataR.MissingEnergy)+sum(a2008$dataR.MissingEnergy)+sum(a2009$dataR.MissingEnergy)+sum(a2010$dataR.MissingEnergy)
Kitchen<-sum(a2007$dataR.kitchen)+sum(a2008$dataR.kitchen)+sum(a2009$dataR.kitchen)+sum(a2010$dataR.kitchen)
Laundry<-sum(a2007$dataR.laundry)+sum(a2008$dataR.laundry)+sum(a2009$dataR.laundry)+sum(a2010$dataR.laundry)
Whac<-sum(a2007$dataR.whac)+sum(a2008$dataR.whac)+sum(a2009$dataR.whac)+sum(a2010$dataR.whac)
# Create test data.
dat = data.frame(zones=c(MissingEnergy, Kitchen, Laundry, Whac), category=c("MissingEnergy", "Kitchen", "Laundry", "Whac"))
dat$pct <- round(dat$zones/sum(dat$zones)*100)

# Add addition columns, needed for drawing with geom_rect.
dat = dat[order(dat$pct), ]
dat$ymax = cumsum(dat$pct)
dat$ymin = c(0, head(dat$ymax, n=-1))
dat$lbls <- paste(dat$pct,"%",sep="")


# Make the plot
p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  geom_label(aes(label_value(dat$lbls)))+
  #geom_label(aes(label=lbls, x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)+
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "My Energy Consumption") +
  labs(title="")
p1


#########################-AUTUMNS WEEKWORK/END ACROSS YEARS
TS2<-as.data.frame(dataR)
selectyears<-TS2[which(year(TS2$DateTime)==c(2007,2008,2009)),]
autumn2008<-data2008[which(month(data2008$DateTime)==c(9,10,11)),]
avweek<-mutate(autumn2008, weekdays(DateTime))
weekend<-avweek[which(weekdays(avweek$DateTime)==c('Friday', 'Saturday', 'Sunday')),]
names(weekend)[13]<-'WeekDays'
Hweekend<-mutate(weekend, hour(DateTime))
Hweekend<-aggregate(Hweekend, by=list(Hweekend$WeekDays, Hweekend$`hour(DateTime)`), FUN=mean)
names(Hweekend)[1]<-'WeekDay'
names(Hweekend)[2]<-'Hour'
Autumn2008missing<-ggplot(Hweekend) +geom_line(aes(Hour, GlobalEnergy), color='coral2') + 
  geom_line(aes(Hour, whac), color='green4')+
  geom_line(aes(Hour, kitchen), color='orange')+
  geom_line(aes(Hour, laundry), color='purple')+
  labs(y="", x="S.Consumption 696WH")+
  ylim(0,60)+
  facet_grid(WeekDay ~ .)
Autumn2008missing

###################- WORKDAYS
WorkDays<-avweek[which(weekdays(avweek$DateTime)==c('Monday','Tuesday','Wednesday','Thursday')),]
names(WorkDays)[13]<-'WeekDays'
HWorkDays<-mutate(WorkDays, hour(DateTime))
HWorkDays<-aggregate(HWorkDays, by=list(HWorkDays$WeekDays, HWorkDays$`hour(DateTime)`), FUN=mean)
names(HWorkDays)[1]<-'WeekDay'
names(HWorkDays)[2]<-'Hour'
workAutumn2008missing<-ggplot(HWorkDays) + geom_line(aes(Hour, GlobalEnergy), color='coral2') + 
  geom_line(aes(Hour, whac), color='green4')+
  geom_line(aes(Hour, kitchen), color='orange')+
  geom_line(aes(Hour, laundry), color='purple')+
  labs(y="", x="S.Consumption 791WH")+
  ylim(0,60)+
  facet_grid(WeekDay ~.)
workAutumn2008missing