# EnergyConsumption

Measuring Energy Consumption in a Household.

I used data from the UCI repository: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption to analyze energy consumption of a household. The data contains 47 months of observations - of particular interest are the measurements gathered by submeters installed in three rooms: kitchen, laundry room, airconditioning and water heating.

The data process has been to:
- Check for quality of the measurements: There are no records for 1.25% of the observations, indicating that 0.97% of the cells have no information. It was important to visualize the duration, and the moment in time those missing observations occured to evaluate if those values should be replaced.
- Visualize several periods of time to understand consumption patterns - if any.
- Build predictive models for different time series.

* Next steps are to improve visualization using Shiny App.

** A lot of work has been put to understand the differences in computational time, and consequences of using a library vs another. Particularly, careful attention was put to understand the difference between strptime, as.POSIXct, and parse_date_time to set the class for a parsed column containing information of date and time of each observation.

*** The improvement of the code comes from revisiting strategies for efficient programming. 
