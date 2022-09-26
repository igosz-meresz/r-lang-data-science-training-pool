dir.create("~/BIWR/Chapter1/Code", recursive = TRUE)
dir.create("~/BIWR/Chapter1/Data")
dir.create("~/BIWR/Chapter1/Results")

# set the working directory
setwd("~/BIWR/Chapter1")
getwd()

# this package will allow us to interpolate between missing time series values
require(zoo)

# these packages provide functions for easy data wrangling
require(dplyr)
require(reshape2)

# this package provides automated forecasting of time series data
require(forecast)

# this package allows us to create publication-quality plots
require(ggplot2)

# this package allows creation of javascript widgets for use in webpages
require(htmlwidgets)

# this package uses htmlwidgets to make series widgets
require(dygraphs)

# download the zip file into the data folder
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/
household_power_consumption.zip", destfile =
                "Data/household_power_consumption.zip")

# unzip the data from zip file into the Data folder
unzip("Data/household_power_consumption.zip", exdir = "Data")

# read the data into R
# NAs are represented by blanks and ? in this data, so I need to change that
power <- read.table("Data/household_power_consumption.txt", sep = ";", header = T, na.strings = c("?",""), stringsAsFactors = FALSE)

### WRANGLING DATA
# str gives me the structure of the dataset

str(power)

# The Date and Time variables are read in as characters, I'll convert them to date and time classes, as well  as create a  new DateTime column
# Convert the character date into ISO date
power$Date <- as.Date(power$Date, format = "%d/%m/%Y")

# create a DateTime object
power$DateTime <- as.POSIXct(paste(power$Date, power$Time))

# obtain the Month and Year for each data point
power$Month <- format(power$Date, "%Y-%m")

# add the first to each Y-m combo and convert back to ISO Date
power$Month <- as.Date(paste0(power$Month, "-01"))

# verify the changes
str(power)

# get an overview of the variables
summary(power)

# We can see from the summary that there are about 26k missing values (NAs) in our primary variable,
# Global_active_power—about 1.25% of the 2 million records. We can quickly get a table and graph
# of missing data over time by setting a counting marker for missing values with ifelse, then using
# the dplyr package to group and summarize the data, and finally pull a ready-made R function from
# the web to create a calendar graph that shows the daily distribution of the missing values.

# use ifelse to count each minute that is NA
power$Missing <- ifelse(is.na(power$Global_active_power), 1, 0)

# use dplyr's group_by function to group the data by Date
power_group_day <- group_by(power, Date)

# use dplyr's summarize function to summarize by our NA indicator
# (where 1 = 1 minute with NA)
power_day_missing <- summarize(power_group_day, Count_Missing = sum(Missing))

# download the 'calendarHeat' function from revolutionanalytics.com
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

# plot the calendar graph to view the missing data pattern
calendarHeat(power_day_missing$Date, power_day_missing$Count_Missing, varname = "Missing Data", color = "w2b")

# You can view the actual values of missing data on each day by exploring the power_day_missing
# data frame.
# The 4-5 day spans of missing data near the end of the time series may be a little concerning since we
# want to perform automated forecasting. However, since we’re aggregating to months and forecasting
# into months with very few missing values, we should be ok. But to make automatic forecasting
# work, we need to fill in those missing values. If we convert each missing value to 0, we’ll definitely
# underestimate usage for those times.
# A reasonable approach here is to carry the last value forward, which we can do with the na.locf
# function in the zoo package. While other approaches are possible (and perhaps better, e.g., using
# some sort of mean or median value instead of the last value, or even a seasonal Kalman filter), we’ll
# proceed with this option to keep the example simple.

# Use zoo to perform interpolation for missing series values
power$Global_active_power_locf <- na.locf(power$Global_active_power)

# Compare the original and interpolated distributions by
# reshaping the two variables into long form for ggplot
power_long <- melt(power, id.vars = "DateTime", measure.vars = c("Global_active_power", "Global_active_power_locf"))

# create density plot
ggplot(power_long, aes(value, fill=variable, color=variable)) +
  geom_density(alpha = 0.75) +
  facet_wrap(~variable)

# save density plot to Results folder
# note that ggsave used 'dpi' to set image resolution
ggsave("Results/density_plot.png", width = 6, height = 4, dpi = 600, units = "in")

# The overall shape hasn’t changed, though we can see a small spike at about 1 kW in the lastobservation-
# carried-forward data. This should be fine for our purposes, as the overall pattern is
# essentially the same.
# Now that we have a complete time series, we can determine total monthly use (kWh). While we’re
# at it, we can calculate maximum demand for a given month (kW) over the period of record as an
# example of how to calculate multiple summaries at once. kWh measures use, i.e., how much energy
# is used, while kW measures demand. We’re interested in usage, because that’s how power companies
# charge us.

# use dplyr to group by month
power_group <- group_by(power, Month)

# use dplyr to get monthly max demand and total user results
power_monthly <- summarise(power_group,
                           Max_Demand_kW = max(Global_active_power_locf),
                           Total_Use_kWh = sum(Global_active_power_locf)/60)

# remove partial months from data frame
power_monthly <- power_monthly[2:47,]

# convert month to date
power_monthly$Month <- as.Date(paste0(power_monthly$Month, "-01"))

# look at the structure of the result
str(power_monthly)

### ANALYTICS
# explore the data

# Plotting your data is the single most important part of analytics, so this book spends a lot of space
# on graphical representation. Here, as we’re focused on power use over time, we’ll plot the monthly
# summary we’ve calculated above.

# create a plot of total use by month
ggplot(power_monthly, aes(Month, Total_Use_kWh)) +
  geom_line(col = "blue", lwd = 1)

# save the result as hi-res png to Results subfolder
ggsave("Results/total_use_plot.png", width = 6, height = 4, dpi = 600, units = "in")

# We can see clear patterns in the data—higher in the winter and lower in the summer.

### --- Run a forecasting model --- ###
# Now we want to forecast total use for the next six months. We’ll create the model with the automated
# forecast function from the forecast package, then plot the results and view the model itself.

# create a time series object of monthly total use
total_use_ts <- ts(power_monthly$Total_Use_kWh, start = c(2007, 1), frequency = 12)

# automatically obtain the forecast for the next 6 months
# using the forecast package's forecast function
# see ?forecast for more details
total_use_fc <- forecast(total_use_ts, h = 6)

# view the forecast model results
summary(total_use_fc)

# export a copy of the model results into a text file in the Results folder
sink("Results/Forecast_Model.txt")
summary(total_use_fc)
sink()

# view the forecast plot
plot(total_use_fc)

# save the base graphics plot to the Results folder
# note that resolution is called 'res' here
png("Results/total_use_forecast.png", width = 6, height = 4, res = 600, units = "in")
plot(total_use_fc)
dev.off()

## REPORTING
# With the forecast summary and plot we have the essential pieces for addressing the problem. Now
# we need to create a report for decision-making. In this case, we’ll again keep it simple and just
# produce an interactive browser app of the monthly trends and forecast results.

# CREATE INTERACTIVE HTML PLOT

# create a data frame with the original data
# and placeholders for the forecast details
use_df <- data.frame(Total_Use = power_monthly$Total_Use_kWh,
                     Forecast = NA, Upper_80 = NA,
                     Lower_80 = NA, Upper_95 = NA, Lower_95 = NA)

# create a data frame for the forecast details
# with placeholder column for the original data
use_fc <- data.frame(Total_Use = NA, Forecast = total_use_fc$mean,
                     Upper_80 = total_use_fc$upper[,1], Lower_80 = total_use_fc$lower[,1],
                     Upper_95 = total_use_fc$upper[,2], Lower_95 = total_use_fc$lower[,2])

# union the two data frames into one using rbind
use_ts_fc <- rbind(use_df, use_fc)

# create a time series of the data and forecast results
total_use_forecast <- ts(use_ts_fc, start = c(2007, 1), freq = 12)

# create the widget
energy_use_prediction_widget <- dygraph(total_use_forecast,
                                        main = "Predicted Monthly Electricity Use (kWh)",
                                        ylab = "Total kWh", width = 900, height = 500) %>%
  dySeries(c("Total_Use"), label = "Actual kWh Usage") %>%
  dyEvent(x = "2008-08-01", "Went on vacation", labelLoc = "top") %>%
  dyRangeSelector(dateWindow = c("2008-08-15", "2011-06-01")) %>%
  dyLegend(width = 800)

# display the widget in the Viewer window
# hit the Zoom button for a pop-out
energy_use_prediction_widget

# save the widget as a stand-alone HTML file to Results folder
saveWidget(energy_use_prediction_widget, "energy_use_prediction_widget.html")

# Opening the energy_use_prediction_widget.html from the Reports folder shows us the result. This
# stand-alone file can be sent to the decision-maker(s) who can explore the exact trend and forecast
# values with a mouse hover, while still seeing the overall pattern.

