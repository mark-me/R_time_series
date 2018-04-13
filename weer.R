home_dir <- "~/R scripts"
#home_dir <- "~/Downloads/Dropbox/Werk/R\ Scripts/"
setwd(paste0(home_dir, "R_time_series/"))
source("project.R")
open_project("R_time_series", home_dir)

library(httr)

get_url <- "http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi?"
get_url <-paste(get_url,
                "stns=240",
                "vars=DDVEC:FHVEC:FG:FHX:FXX:TG:TN:TX:SQ:SP:Q:DR:RX:RXH:NG:UG",
                "byear=1960&bmonth=1&bday=1",
                "eyear=2018&emonth=12&eday=31",
                sep = "&")

cols_weather <- c("id_station", "date_day", "wind_direction_degrees", "wind_speed", "wind_speed_mean", "wind_speed_hour_max",
                  "wind_speed_max", "temp_mean", "temp_min", "temp_max", "hours_sunshine", "per_sunshine_potantial",
                  "duration_recipitation", "RX", "RXH", "cloud_cover_mean", "WTF", "humidity")

# Write weather data to disk 
file_weather <- GET(get_url, write_disk(paste0(dir_input, "/weather.csv"), overwrite = TRUE))

tbl_weather <- read.csv(paste0(dir_input, "/weather.csv"),
                        col.names = cols_weather,
                        comment.char = "#", 
                        header = FALSE)

names(tbl_weather) <- cols_weather

tbl_weather %<>%
  mutate(date_day = ymd(date_day)) %>% 
  mutate(wind_speed = wind_speed * .36,
         wind_speed_mean = wind_speed_mean * .36,
         wind_speed_hour_max = wind_speed_hour_max * .36,
         wind_speed_max = wind_speed_max * .36,
         temp_mean  = temp_mean / 10,
         temp_min   = temp_min / 10,
         temp_max   = temp_max / 10)

ggplot(tbl_weather, aes(date_day, temp_mean)) + 
  geom_line(col = col_graydon[1]) + 
  scale_x_date('Months') + 
  scale_y_continuous(labels = format_number) +
  ylab("Temperature") +
  xlab("") + 
  theme_graydon("grid")
  
library(forecast)
library(tseries)
library(xts)


date_first <- min(tbl_weather$date_day)
ts_temp_mean <- ts(tbl_weather$temp_mean, frequency = 365, start = c(year(date_first), month(date_first)))
ts_temp_mean <- tsclean(ts_temp_mean)

autoplot(ts_temp_mean) +
  geom_smooth() +
  theme_graydon("horizontal")

frequency(ts_temp_mean)

library(fpp2)
library(astsa)

plot.xts(ts_temp_mean) 

acf <- ggAcf(ts_temp_mean) +
  theme_graydon("horizontal")

pacf <- ggPacf(ts_temp_mean) +
  theme_graydon("horizontal")

grid.arrange(acf, pacf)

ts_temp_mean_subset <- window(ts_temp_mean, start = 2009, end = 2017)

ggseasonplot(ts_temp_mean_subset, polar = TRUE) +
  scale_color_graydon() +
  theme_graydon("grid")

gglagplot(ts_temp_mean_subset)

forecast <- sarima.for(ts_temp_mean, 
                       n.ahead = 1, 
                       p = 1, d = 0, q = 2, 
                       P = 2, D = 0, Q = 1, 
                       S = 365)
ndiffs(ts_temp_mean_subset)
