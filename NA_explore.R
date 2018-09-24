# ........EXPLORE NA. THREE TYPES.............#
#1, IF DON'T SPECIFY UTC, AS.POSIXCT CREATES 240NA ACCOUNTING FOR DAYLIGHT SAVINGS
#2, NA BECAUSE OF VACATION AND PEOPLE SWITCHED OFF COUNTER
#3, NA BECAUSE OF ERROR IN THE SUBMETER

# Loading Packages ....................................................
pacman:: p_load(dplyr, tidyr, lubridate, ggplot2, scales)

# Import data .........................................................
data_parse <- data.table::fread("Energy/household_power_consumption.txt", sep = ";", na.strings = '?')

# unite + convert to time format
data_parse <- data_parse %>% unite(DateTime, Date, Time, sep = " ", remove = F)
data_parse <- data_parse %>% mutate(Date = dmy(Date))
data_parse$DateTime <- parse_date_time(data_parse$DateTime, orders = "dmy HMS")
table(complete.cases(data_parse$DateTime))
  # parse_date_time accounts for UTC automatically

# how many NA ........................................................
table(complete.cases(data_parse)) # returns obs. containing NA
(25979 / nrow(data_parse)) * 100     # 1.25% obs (rows) missing data

table(is.na(data_parse))           #returns count of NA.
(181853/(nrow(data_parse)*9)) * 100   # 0.97% cells with missing data


# In UTC, how long the NA last?
data_parse_na <- data_parse %>% filter(!complete.cases(.))

count_min_na <- data_parse_na %>% group_by(Date) %>% 
  arrange(Date, .by_group = TRUE) %>% tally()
  # 82 obs with NA

table(count_min_na$n)
  # 21 obs with more than 7hrs of unmeasured energy

no_power_days <- count_min_na %>% filter(n >= 1440)
  # 9 full days with no power

# Plotting Duration of Unmeasured Consumption
ggplot(count_min_na, aes(n)) + geom_histogram(bins = 50, fill = "blue") +
  ggtitle("Duration of Unmeasured Consumption", subtitle = "47 Moths of Data") +
  xlab("Minutes per Day") + ylab("Times it Occurred") +
  theme_minimal()

# Calendar of Occurence..........................

source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
pacman::p_load(lattice, grid, chron)

png(filename = "Energy/UnregisteredEnergy.png", width = 480, height = 480)

calendarHeat(count_min_na$Date, count_min_na$n, 
             varname="Unmeasured Consumption per Minute", color="r2b")

dev.off()


# Cause of unmeasured consumption?
# Filter day of first minute NA
first <- data_parse %>% filter(Date == "2006-12-21")


#........ need to study missing values:


# Fill NA in Global with previous observation to understand behaviour ...................
power$Global_locf <- na.locf(power$Global)

# Plot for difference after complete NA.
d.locf <- power %>% select(Date, Global, Global_locf) %>% gather("type", "value", -Date)

# Visualize differences with density plot
ggplot(d.locf, aes(value, fill = type)) +
  geom_density(alpha= 0.35) + 
  ggtitle("Comparing Global Active Power Measures", subtitle = "Filling NA with last registry") +
  theme_minimal()

# Save density plot to Results folder
ggsave("Energy/LocfComparison.jpeg", width=6, height=4, dpi=600, units="in")

# na.locf for all
power <- power %>% do(na.locf(Glo))


