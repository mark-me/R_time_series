home_dir <- "~/Downloads/Dropbox/Werk/R\ Scripts/"  # "~/Downloads/Dropbox/Werk/R\ Scripts/" "~/R scripts/"   
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

# Create time series
ts_companies <- ts(tbl_companies_code$qty_companies, 
                   frequency = 12, 
                   start = c(year(var_date_from), month(var_date_from)) )

# Clean time series
ts_companies_clean <- tsclean(ts_companies)

# Plot imported vs. cleaned time series
plot_time_series(list(ts_companies, ts_companies_clean),
                 c("Read", "Cleaned"))

# Forecasting ----
# Create training set
ts_companies_train = subset(ts_companies_clean, end = length(ts_companies_clean) - months_forecast)

# Create forecast models ----
fit_mean <- meanf(ts_companies_train, h = months_forecast)    # Mean forecasting (mean of all observations)
fit_naive <- naive(ts_companies_train, h = months_forecast)   # Naive
fit_snaive <- snaive(ts_companies_train, h = months_forecast) # Naive seasonal
fit_ses <- ses(ts_companies_train, h = months_forecast)       # Simple exponential smoothing
fit_holt <- holt(ts_companies_train, h = months_forecast)     # Holt’s linear trend
fit_hw_m <- hw(ts_companies_train, h = months_forecast, seasonal = "multiplicative")  # Holt-Winters multiplicative
fit_hw_a <- hw(ts_companies_train, h = months_forecast, seasonal = "additive")        # Holt-Winters additive
model_ets <- ets(ts_companies_train)                # Errors, Trend, and Seasonality (ETS) 
fit_ets <- forecast(model_ets, h = months_forecast)
model_auto.arima <- auto.arima(ts_companies_train)  # ARIMA (automatic)
fit_auto.arima <- forecast(model_auto.arima, h = months_forecast)

# Evaluate forecast models ----
lst_fitted <- list(fit_mean, fit_naive, fit_snaive, fit_holt, fit_hw_m, fit_hw_a, 
                   fit_ets, fit_ses, fit_auto.arima)
lst_evaluations <- lapply(lst_fitted, evaluate_forecast, ts_companies_clean) 

# Review MASE
tbl_forecast_mase <- do.call("rbind", sapply(lst_evaluations, "[", 2))
tbl_forecast_mase %>% 
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

# Plot all forecast methods
lst_forecast_plots <- sapply(lst_evaluations, "[", 1)
do.call("grid.arrange", c(lst_forecast_plots, ncol=3))

# Plot of the best performing forecast method
no_best <- (tbl_forecast_mase %>% 
              mutate(row_no = row_number()) %>% 
              mutate(is_best = MASE == min(MASE)) %>% 
              filter(is_best))$row_no 
lst_forecast_plots[no_best]  

# Do best method forecast for longer period ----
fit_hw_a <- hw(ts_companies_train, h = 60, seasonal = "additive")
result_fit <- evaluate_forecast(fit_hw_a, ts_companies_clean)
result_fit$p_forecast


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
