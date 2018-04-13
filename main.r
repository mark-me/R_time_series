setwd("~/R scripts/R_time_series")
source("project.R")
open_project("R_time_series", "~/R scripts")

# Time-series packages ----
library(xts)
library(forecast)
library(tseries)

# Bike sharing data ----
url_bike_sharing <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
download_extract(url_bike_sharing, dir_input)
tbl_bike_sharing <- read_csv(paste0(dir_input, "/day.csv"))

tbl_bike_sharing$Date = as.Date(tbl_bike_sharing$dteday)

# Plot bike sharing data
ggplot(tbl_bike_sharing, aes(Date, cnt)) + 
  geom_line(col = col_graydon[1]) + 
  scale_x_date('Months') + 
  scale_y_continuous(labels = format_number) +
  ylab("Daily Bike Checkouts") +
  xlab("") + 
  theme_graydon("grid")

# Create time-season
ts_bike_sharing <- ts(tbl_bike_sharing$cnt)
ts_bike_sharing_new = tsclean(ts_bike_sharing)

test <- cbind(ts_bike_sharing, ts_bike_sharing_new)

test[,1] - test[,2]
