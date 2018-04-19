home_dir <- "~/R\ scripts"  # "~/Downloads/Dropbox/Werk/R\ Scripts/" "~/R scripts/"   
setwd(paste0(home_dir, "/R_time_series/"))

source("project.R")
source("load_companies.R")
source("roll_up_nace_tree.R")
source("forecast_functions.R")

# Configuration file
library(yaml)
config <- read_yaml("config.yml")

# Set up project
open_project("R_time_series", home_dir)

# Set analysis variables
var_date_from <- as.Date(config$date_start) # Start date of time series
var_date_to <- as.Date(config$date_end)     # End date of time series
months_forecast <- config$months_forecast   # Number of months in forecast

# Import branche hierarchy data ----
tbl_nace <- read.csv2(paste0(dir_input, "/branche_hierarchy.csv"), stringsAsFactors = FALSE)

# Import & transform company data ----
tbl_companies <- prep_companies(config$process_companies, var_date_from, var_date_to)

# Do nace roll-up ----
tbl_nace_qty <- tbl_nace %>% 
  left_join(tbl_companies, by = c("code" = "sbi_full")) %>% 
  group_by(code, code_parent, layer_no) %>% 
  summarise(qty = n_distinct(id_giant, na.rm = TRUE)) %>% 
  ungroup() 

lst_nace_recoding <- roll_up_nace_tree(tbl_nace_qty, 50000)

tbl_companies %<>% 
  left_join(lst_nace_recoding$tbl_dictionary, by = c("sbi_full" = "code"))

# Spread out active companies by month and group by SBI code ----
if(config$process_aggregate){
  tbl_companies_aggr <- aggregate_companies(tbl_companies, lst_nace_recoding$tbl_dictionary)
  saveRDS(tbl_companies_aggr, file = paste0(dir_input, "/companies_aggr.RDS"))
} else {
  tbl_companies_aggr <- read_rds(paste0(dir_input, "/companies_aggr.RDS"))
}

tbl_companies_aggr %<>%
  rename(code = code_new) %>% 
  left_join(tbl_nace, by = "code")

# Plot number of companies per NACE code by month ----
ggplot(tbl_companies_aggr, aes(x = month, y = qty_companies)) +
  geom_area(col = col_graydon[2], fill = col_graydon[2], alpha = .6) +
  facet_wrap(~ code) +
  scale_y_continuous(labels = format_number) +
  labs(x = "", y = "# Companies") +
  theme_graydon("grid")

# Number of companies per NACE code ----
tbl_companies_by_nace <- tbl_companies_aggr %>% 
  group_by(code, description) %>% 
  summarise(qty_companies = sum(qty_companies))

# Display number of companies for a single NACE by month ----
tbl_companies_aggr %>% 
  filter(code == config$code_nace) %>% 
ggplot(aes(x = month, y = qty_companies)) +
  geom_area(col = col_graydon[2], fill = col_graydon[2], alpha = .6) +
  facet_wrap(~ description) +
  scale_y_continuous(labels = format_number) +
  labs(x = "", y = "# Companies") +
  theme_graydon("grid")

# Timeseries exploration ----
library(forecast)
library(ggfortify)

# Get selected nace code
tbl_companies_code <- tbl_companies_aggr %>% filter(code == config$code_nace)
var_sector <- (tbl_nace %>% filter(code == config$code_nace))$description

ts_companies <- ts(tbl_companies_code$qty_companies, 
                   frequency = 12, 
                   start = c(year(var_date_from), month(var_date_from)) )


# Clean time series
ts_companies_clean <- tsclean(ts_companies)

p_ts <- autoplot(ts_companies,
                 main = var_sector,
                 ts.colour = col_graydon[1]) +
  theme_graydon("grid")

p_ts_clean <- autoplot(ts_companies_clean, 
                       main = paste(var_sector, "clean"),
                       ts.colour = col_graydon[1]) +
  scale_y_continuous(labels = format_number) +
  theme_graydon("grid")

grid.arrange(p_ts, p_ts_clean)

df_companies_clean <- fortify(ts_companies_clean) %>% mutate(set = "Cleaned")
df_companies <- fortify(ts_companies) %>% mutate(set = "Original")
df_companies <- rbind(df_companies, df_companies_clean)

ggplot(df_companies, aes(x = Index, y = Data)) + 
  geom_line(aes(col = set)) + 
  scale_color_graydon() +
  scale_y_continuous(labels = format_number) +
  labs(x = "", y = "# Compannies", col = "") +
  theme_graydon("grid")
  
rm(df_companies, df_companies_clean)

# Forecasting trends ----
# Create training set
ts_companies_train = subset(ts_companies_clean, end = length(ts_companies_clean) - months_forecast)

# Set up data frame for error evaluation
tbl_model_error <- data_frame(method = as.character(), MASE = as.integer())
library(rlist)

# Mean forecasting (mean of all observations)
fit_mean <- meanf(ts_companies_train, h = months_forecast)
result_fit <- evaluate_forecast(fit_mean, ts_companies_clean)
lst_plots <- list(result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Naive
fit_naive <- naive(ts_companies_train, h = months_forecast)
result_fit <- evaluate_forecast(fit_naive, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Simple exponential smoothing
fit_ses <- ses(ts_companies_train, h = months_forecast)
result_fit <- evaluate_forecast(fit_ses, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Holt’s linear trend
fit_holt <- holt(ts_companies_train, h = months_forecast)
result_fit <- evaluate_forecast(fit_holt, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Holt-Winters seasonal method - Multiplicative
fit_hw <- hw(ts_companies_train, h = months_forecast, seasonal = "multiplicative")
result_fit <- evaluate_forecast(fit_hw, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Holt-Winters seasonal method - Additive
fit_hw <- hw(ts_companies_train, h = months_forecast, seasonal = "additive")
result_fit <- evaluate_forecast(fit_hw, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Errors, Trend, and Seasonality (ETS) ----
model_ets <- ets(ts_companies_train)
fit_ets <- forecast(model_ets)
result_fit <- evaluate_forecast(fit_ets, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# ARIMA (automatic)
model_auto.arima <- auto.arima(ts_companies_train)
fit_auto.arima <- forecast(model_auto.arima, h = months_forecast)
result_fit <- evaluate_forecast(fit_auto.arima, ts_companies_clean)
lst_plots <- list.append(lst_plots, result_fit$p_forecast)
tbl_model_error <- rbind(tbl_model_error, result_fit$mase)

# Making stationary ----
# White noise?
Box.test(ts_companies_clean, lag = 12, type = "Ljung") # Nope

# Removing trends: differencing
ts_companies_diff <- diff(ts_companies_clean)
p_diff <- autoplot(ts_companies_diff, 
                   ts.colour = col_graydon[1], 
                   main = "Differenced") +
  theme_graydon("grid")

p_clean <- autoplot(ts_companies_clean, 
                    ts.colour = col_graydon[1], 
                    main = "Normal") +
  theme_graydon("grid")

grid.arrange(p_clean, p_diff)

# White noise?
Box.test(ts_companies_diff, lag = 12, type = "Ljung") # Nope

# Removing seasonality
ggAcf(ts_companies_clean)

ts_test <- ts_companies_diff + 1000

ts_companies_deseason <- diff(log(ts_test), lag = 12)

Box.test(ts_companies_diff, lag = 12, type = "Ljung") # Nope
Pacf(diff(ts_test, lag = 12))

p_deseason <- autoplot(ts_companies_deseason, 
                    ts.colour = col_graydon[1], 
                    main = "De-seasoned") +
  theme_graydon("grid")

grid.arrange(p_clean, p_deseason)

# Alternative all in one
decomp <- stl(ts_companies_clean, s.window = "periodic")
plot(decomp)

# Review MASE ----
tbl_model_error %>% 
  mutate(is_best = MASE == min(MASE)) %>% 
ggplot(aes(x = reorder(method, MASE), y = MASE)) +
  geom_col(aes(fill = is_best)) +
  geom_text(aes(label = round(MASE, 2)), 
            hjust = -.1) +
  scale_fill_graydon() +
  coord_flip() +
  labs(x = "") +
  guides(fill = FALSE) +
  theme_graydon("vertical")

# Plot all forecast methods ----
do.call("grid.arrange", c(lst_plots, ncol=4))

# Plot of the best performing forecast method ----
no_best <- (tbl_model_error %>% 
              mutate(row_no = row_number()) %>% 
              mutate(is_best = MASE == min(MASE)) %>% 
              filter(is_best))$row_no 

lst_plots[no_best]  

# Do best method forecast for longer period ----
fit_hw <- hw(ts_companies_train, h = 60, seasonal = "additive")
result_fit <- evaluate_forecast(fit_hw, ts_companies_clean)
result_fit$p_forecast
