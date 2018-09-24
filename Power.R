# Set working directory and load data ...............................
#setwd("C:/Users/Violeta/Dropbox/Ubiqum/3_Deep.Analytics")
#power <- data.table::fread("Energy/household_power_consumption.txt", sep = ";", na.strings = '?')

# Libraries .........................................................
#pacman::p_load(dplyr, tidyr)

# Create Working Data .....................................................
#power <- power %>% 
#  unite(DateTime, Date, Time , sep = " ", remove = F) %>% 
#  rename(Global = Global_active_power, Reactive = Global_reactive_power, Intensity = Global_intensity, 
#         Kitchen = Sub_metering_1, Laundry = Sub_metering_2, WHAC = Sub_metering_3) %>% 
#  mutate(Global = Global/60, Kitchen = Kitchen/1000, Laundry = Laundry/1000, WHAC = WHAC/1000) %>%
#  mutate(TotalSub = Kitchen + Laundry + WHAC,
#         Unregistered = Global - TotalSub,
#         Efficiency = Global/(Voltage*Intensity))
 
#write.csv(power, "Energy/power_data.csv", row.names = F)
# ...........................................................................................
pacman::p_load(dplyr, tidyr, lubridate, zoo, forecast, ggplot2, htmlwidgets, dygraphs, xts, seasonal)
setwd("C:/Users/Violeta/Dropbox/Ubiqum/3_Deep.Analytics/Energy")
power <- data.table::fread("power_data.csv")

# Set Time Format
power$DateTime <- parse_date_time(power$DateTime, orders = "dmy HMS")
power <- power %>% mutate(Date = dmy(Date))

# Define Total Daily demand (kW) .......................................................
power$day <- floor_date(power$Date, "day")
power_day <- power %>% group_by(D = day) %>% summarise(GP = sum(Global, na.rm = T),
                                                       React = sum(Reactive, na.rm = T),
                                                       Kitchen = sum(Kitchen, na.rm = T),
                                                       Laundry = sum(Laundry, na.rm = T),
                                                       WHAC = sum(WHAC, na.rm = T),
                                                       T.Sub = sum(TotalSub, na.rm = T),
                                                       Unregistered = sum(Unregistered, na.rm = T),
                                                       Efficiency = sum(Efficiency, na.rm = T))

# what method to choose to plot the line of best fit? testing geom_line vs geom_smooth and its methods
p.auto <- ggplot(power_day, aes(D, GP)) + geom_line() + geom_smooth(method = "auto") + xlab("") + theme_minimal()
p.loess <- ggplot(power_day, aes(D, GP)) + geom_line() + geom_smooth(method = "loess") + xlab("") + theme_minimal()
gridExtra::grid.arrange(p.auto, p.loess, top = "Fitting line of best fit: gam vs loess")
    # the GAM method fits the data better. 

# Plotting geom_smooth to fit a line of best fit (leave as 'auto' instead of typing 'gam')
ggplot(power_day) + 
  geom_smooth(aes(D, GP), method = "auto", color = "blue", alpha = 0.7) + 
  geom_smooth(aes(D, Kitchen), method = "auto",  color = "tomato", alpha = 0.7) +
  geom_smooth(aes(D, Laundry), method = "auto", color = "seagreen", alpha = 0.7) +
  geom_smooth(aes(D, WHAC), method = "auto", color = "black", alpha = 0.7) + 
  ggtitle("Daily Total Energy Consumption", subtitle = "Consumption over 47 months") + 
  ylab("Total Use kWh") + xlab("") + theme_minimal()


# Define Total Week demand(kW) ........................................................
power$weekly <- floor_date(power$Date, "week") 
power_week <- power %>% group_by(W = weekly) %>% summarise(GP = sum(Global, na.rm = T),
                                                           React = sum(Reactive, na.rm = T),
                                                           Kitchen = sum(Kitchen, na.rm = T),
                                                           Laundry = sum(Laundry, na.rm = T),
                                                           WHAC = sum(WHAC, na.rm = T),
                                                           T.Sub = sum(TotalSub, na.rm = T),
                                                           Unregistered = sum(Unregistered, na.rm = T),
                                                           Efficiency = sum(Efficiency, na.rm = T))

ggplot(power_week, aes(W, GP)) + geom_line(color = "tomato", lwd = 1) + 
  ggtitle("Weekly Total Energy Consumption", subtitle = "Consumption over 47 months") +
  ylab("Total Use kWh") + xlab("") + theme_minimal()

# Line of best fit to the data:
ggplot(power_week, aes(W, GP)) + geom_line() + geom_smooth(method = "auto") + xlab("") + theme_minimal()
      # returns loess method

# Define Total Monthly demand (kW) .....................................................
power$month <- floor_date(power$Date, "month") 
power_month <- power %>% group_by(M = month) %>% summarise(GP = sum(Global, na.rm = T),
                                                           React = sum(Reactive, na.rm = T),
                                                           Kitchen = sum(Kitchen, na.rm = T),
                                                           Laundry = sum(Laundry, na.rm = T),
                                                           WHAC = sum(WHAC, na.rm = T),
                                                           T.Sub = sum(TotalSub, na.rm = T),
                                                           Unregistered = sum(Unregistered, na.rm = T),
                                                           Efficiency = sum(Efficiency, na.rm = T))

ggplot(power_month) + 
  geom_line(aes(M, GP), color = "blue", lwd = 1) +
  geom_line(aes(M, T.Sub), color = "deeppink3", lwd = 1) +
  ggtitle("Daily Total Energy Consumption", subtitle = "Consumption over 47 months") + 
  ylab("Total Use kWh") + xlab("") + theme_minimal() 

#Line of best fit:
ggplot(power_month, aes(M,GP)) + geom_line() + geom_smooth(method = "auto") + theme_minimal() + xlab("")
    # returns loess method

# Subset, average Energy Cosumption 
month_av <- power %>% group_by(M = month) %>% summarise(GP = mean(Global, na.rm = T),
                                                        React = mean(Reactive, na.rm = T),
                                                        Kitchen = mean(Kitchen, na.rm = T),
                                                        Laundry = mean(Laundry, na.rm = T),
                                                        WHAC = mean(WHAC, na.rm = T),
                                                        T.Sub = mean(TotalSub, na.rm = T),
                                                        Unregistered = mean(Unregistered, na.rm = T),
                                                        Efficiency = mean(Efficiency, na.rm = T))

ggplot(month_av, aes(M, T.Sub)) + geom_area(fill = "blue", lwd = 1) +
  ggtitle("Monthly Average Energy Consumption", subtitle = "Consumption over 47 months") +
  ylab("Average Use Wh") + xlab("") + theme_minimal()

#Line of Best Fit:
ggplot(month_av, aes(M, GP)) + geom_line() + geom_smooth() + xlab("") + theme_minimal()
    # returns loess method


# Define Total Yearly demand (kW) .......................................................
power$year <- floor_date(power$Date, "year")
power_year <- power %>% group_by(Y = year) %>% summarise(GP = sum(Global, na.rm = T),
                                                         React = sum(Reactive, na.rm = T),
                                                         Kitchen = sum(Kitchen, na.rm = T),
                                                         Laundry = sum(Laundry, na.rm = T),
                                                         WHAC = sum(WHAC, na.rm = T),
                                                         T.Sub = sum(TotalSub, na.rm = T),
                                                         Unregistered = sum(Unregistered, na.rm = T),
                                                         Efficiency = sum(Efficiency, na.rm = T))

power_year <- power_year[-1,] #remove 2006 since not even a complete month

ggplot(power_year, aes(Y, GP)) + geom_line(color = "gold", lwd = 1) + 
  ggtitle("Yearly Total Energy Consumption", subtitle = "Consumption over 47 months") +
  ylab("Total Use kWh") + xlab("") + theme_minimal() + ylim(c(7500, 10000))


# Line of Best Fit:
ggplot(power_year, aes(Y, GP)) + geom_line() + geom_smooth() + xlab("") + theme_minimal()
    # returns loess method

#(gam method was detected by auto for only daily energy consumption. Loess has been detected for weekly, monthly, yearly patterns)


# Adding perspective to Consumption ......................................................
ggplot() + 
  geom_line(data = power_day, aes(D, GP), color = "orange") +
  geom_line(data = power_week, aes(W, GP), color = "tomato") +
  geom_line(data = power_month, aes(M, GP), color = "blue") +
  ggtitle("Total Energy Consumption", subtitle = "Daily, Weekly, Monthly") + 
  ylab("Total Use kWh") + xlab("") + theme_minimal()


# create time series object for the dygraph ................................................
dayly <- xts(power_day$GP, power_day$D)
weekly <- xts(power_week$GP, power_week$W)
monthly <- xts(power_month$GP, power_month$M)

data_ts <- cbind(Daily = dayly, Weekly = weekly, Montly = monthly)

(consumption <- dygraph(data_ts, main = "Total Energy Consimption", ylab = "Total kWh") %>% 
    dyRangeSelector() %>% dyRoller(rollPeriod = 48)) # roll period set by monthly obs


# Pie Chart for room comaprison:


# Time Series ............................................................................
day_ts <- ts(power_day$GP, frequency = 356, start = c(2007,1), end = c(2010,356))

week_ts <- ts(power_week$GP, frequency = 53, start = c(2007, 1), end = c(2010, 50))

month_ts <- ts(power_month$GP, frequency = 12, start = c(2007,1), end = c(2010, 11))

year_ts <- ts(power_year$GP, frequency = 1, start = c(2007), end = c(2010))

# Bars and moving averages ...............................................................
autoplot(month_ts) + theme_minimal()
ggseasonplot(month_ts, year.labels = T) + theme_minimal() # plotting seasons
ggsubseriesplot(month_ts) + theme_minimal() # seasonal changes over time
gglagplot(month_ts, do.lines = F) # Better correlation with lag12
ggAcf(month_ts) + theme_minimal() # r12 and r1 are best fit
ggtsdisplay(month_ts, plot.type = "histogram") 
    # in the monthly gathered data, r1 and r12 are highest peaks. And lowest at r7


# Defining Moving Average for Monthly Consumption
autoplot(month_ts, series = "GP") +
  autolayer(ma(month_ts),series = "6-MA") + theme_minimal()

# Decompose based on additive:
month_ts %>% decompose(type = "additive") %>% autoplot() + theme_minimal()

# Decompose with X11:
fit <- month_ts %>% seas(x11 = "")
autoplot(fit) + theme_minimal() + ggtitle("Monthy Time Series")

# Seasonally adjusted for improved predictions:
autoplot(month_ts, series = "GP") +
  autolayer(trendcycle(fit), series = "Trend") +
  autolayer(seasadj(fit), series = "Seasonally Adjusted") + theme_minimal() + 
  ggtitle("Seasonaly Adjusted Month Time Series")


# Decompose Seasonal Extraction in ARIMA Time Series:
month_ts  %>% decompose() %>% autoplot() +  theme_minimal() +
  ggtitle("Seasonal Exraction in ARIMA Decomposition, Time Series")

# STL Decomposition "Seasonal and Trend decomposition using Loess":
# The default fits a parabola to the points, odd number for t.window:
fit_stl <- month_ts %>% stl(t.window = 11, s.window = "periodic", robust = T)
autoplot(fit_stl) + theme_minimal() + ggtitle("STL Decomposition, Month Time Series")

# Naïve Forecast:
fit_stl %>% seasadj() %>% naive() %>% autoplot() + theme_minimal() + 
  ggtitle("Seasonally Adjusted Naïve Forecast", subtitle = "High Granurality for Monthly Time Series")

# Naïve Forecast, not adjusted:
fit_stl %>% forecast(method = "naive") %>% autoplot() + theme_minimal() + 
  ggtitle("Naïve Forecast, Not-Seasonally Adjusted", subtitle = "Monthly Time Series")

fit_day <- day_ts %>% stl(t.window = 360, s.window = "periodic", robust = T)
fit_day %>% forecast(method = "naive") %>% autoplot() + theme_minimal() +
  ggtitle("Forecasting Daily Consumption", subtitle = "STL + Random Walk")

# Detecting Anomaly ........................................................................
#install.packages("anomalize")
require(anomalize)
month_noNA <- na.omit(power_month) # have to omit NA to use Anomalize:

# We use STL which uses seasonal decomposition. The alternative methos is "twitter" which uses
# trend to remove the trend. (Results slightly differ).
# Method "gesd" detects outliers better than the alternative "iqr".

# Unregistered Areas:9 anomalies detected
month_noNA %>% 
  time_decompose(Unregistered, method = "stl") %>% 
  anomalize(remainder, method = "gesd") %>% 
  time_recompose() %>% 
  plot_anomaly_decomposition(alpha_dots = 1, size_circles = 6, color_yes = "red") +
  ggtitle("Energy Consumption in Unregistered Areas",
          subtitle = "9 Anomalies in the Remainder calculated with GESD across 47 months")

# T.Submeters: 5 anomalies detected
month_noNA %>% 
  time_decompose(T.Sub, method = "stl") %>% 
  anomalize(remainder, method = "gesd") %>% 
  time_recompose() %>% 
  plot_anomaly_decomposition(alpha_dots = 0.5, size_circles = 6, color_yes = "deeppink") +
  ggtitle("Energy Consumption for all Three Submeters",
          subtitle = "5 Anomalies in the Remainder calculated with GESD across 47 months")

# Entire House: 2 anomalies detected
month_noNA %>% 
  time_decompose(GP, method = "stl") %>% 
  anomalize(remainder, method = "gesd") %>% 
  time_recompose() %>% 
  plot_anomaly_decomposition(alpha_dots = 0.5, size_circles = 6, color_yes = "black") +
  ggtitle("Energy Consumption registered for the Entire Household",
          subtitle = "2 Anomalies in the Remainder calculated with GESD across 47 months")
