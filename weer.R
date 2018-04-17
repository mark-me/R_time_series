#home_dir <- "~/R scripts"
home_dir <- "~/Downloads/Dropbox/Werk/R\ Scripts/"
setwd(paste0(home_dir, "R_time_series/"))
source("project.R")
open_project("R_time_series", home_dir)
library(yaml)

library(httr)

# Get weather data ----
get_url <- "http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi?"
get_url <-paste(get_url,
                "stns=240",
                "vars=DDVEC:FHVEC:FG:FHX:FXX:TG:TN:TX:SQ:SP:Q:DR:RX:RXH:NG:UG",
                "byear=1951&bmonth=1&bday=1",
                "eyear=2018&emonth=12&eday=31",
                sep = "&")
# Column names
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
         temp_max   = temp_max / 10) %>% 
  group_by(month=floor_date(date_day, "month")) %>%
  summarise(temp_mean = mean(temp_mean))

# Plot mean temperature data ----
ggplot(tbl_weather, aes(month, temp_mean)) + 
  geom_area(alpha = 0.5, position = position_dodge(0.8), col = col_graydon[2], fill = col_graydon[2]) + 
  stat_smooth(color = col_graydon[1], fill = col_graydon[1], method = "loess")+
  #scale_x_date('Months', limits = c(as.Date("2013-01-01"), NA)) + 
  scale_y_continuous(labels = format_number) +
  ylab("Temperature") +
  xlab("") + 
  theme_graydon("grid")
  
library(forecast)
library(ggfortify)
#library(tseries)
#library(xts)

# Create time series ----
date_first <- min(tbl_weather$month)
ts_temp_mean <- ts(tbl_weather$temp_mean, frequency = 12, start = c(year(date_first), month(date_first)))
ts_temp_mean <- tsclean(ts_temp_mean)
frequency(ts_temp_mean)

autoplot(ts_temp_mean, ts.colour = col_graydon[3]) +
  #geom_smooth(method = "lm", col = col_graydon[2]) +
  theme_graydon("horizontal")

boxplot(ts_temp_mean~cycle(ts_temp_mean))

# Decomposition ----
decomp <- stl(ts_temp_mean, s.window = "periodic")
deseasonal_temp <- seasadj(decomp)
plot(decomp)

ts_temp_mean_log <- log(ts_temp_mean + 20)
ts_temp_mean_diff <- diff(ts_temp_mean)
plot(ts_temp_mean_diff)

# Augmented Dickey-Fuller stationarity test ----
adf.test(ts_temp_mean_diff, alternative = "stationary")

deseasonal_temp_diff = diff(deseasonal_temp, differences = 1)
plot(deseasonal_temp_diff)
adf.test(deseasonal_temp_diff, alternative = "stationary")

library(fpp2)
library(astsa)

par(mfrow=c(4,2))
sarima(ts_temp_mean_diff, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)

# Auto correlation function plot
acf <- ggAcf(ts_temp_mean_diff) +
  theme_graydon("horizontal")

# Partial autocorrelation function plot
pacf <- ggPacf(ts_temp_mean_diff) +
  theme_graydon("horizontal")

grid.arrange(acf, pacf)

# Forecasting ----
# Naive
fit_naive <- snaive(ts_temp_mean, h = 60)
autoplot(fit_naive, xlim = c(2010, NA)) 

checkresiduals(fit_naive)

BoxCox.lambda(ts_temp_mean)

ts_temp_mean %>% BoxCox(lambda = 1.45) %>% autoplot()

difflogtemp <- diff(log(ts_temp_mean + 20), lag = 1)
autoplot(difflogtemp)

# Auto ARIMA forecasting
fit_auto <- auto.arima(ts_temp_mean, seasonal = TRUE)
tsdisplay(residuals(fit_auto), 
          #lag.max = 15, 
          main = 'Seasonal Model Residuals')

fcast <- forecast(fit_auto, h=60)
autoplot(fcast)
  
# Manual ARIMA forecasting
(fit <- arima(ts_temp_mean, c(1, 0, 0), seasonal = list(order = c(2, 1, 0), period = 12)))

fcast <- forecast(fit, h=60)
autoplot(fcast)


# Determine seasonality ----
ts_temp_mean_subset <- window(ts_temp_mean, start = 2012, end = 2017)

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
