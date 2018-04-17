# Configuration file
library(yaml)
config <- read_yaml("config.yml")
home_dir <- config$home_dir

# Set up project
setwd(paste0(home_dir, "R_time_series/"))
source("project.R")
source("roll_up_nace_tree.R")
open_project("R_time_series", home_dir)

# Set analysis variables
var_date_from <- as.Date(config$date_start) # Start date of time series
var_date_to <- as.Date(config$date_end)   # End date of time series

# Import branche hierarchy data ----
tbl_nace <- read.csv2(paste0(dir_input, "/branche_hierarchy.csv"), stringsAsFactors = FALSE)

# Import & transform company data ----
if(!config$process_rds) {
  tbl_companies <- read_csv2(paste0(dir_input, "/WORK_QUERY_FOR_FILTER_FOR_BEDRIJVEN_N.csv"))
  saveRDS(tbl_companies, file = paste0(dir_input, "/companies.RDS"))
} else {
  tbl_companies <- read_rds(paste0(dir_input, "/companies.RDS"))
}
names(tbl_companies) <- c("temp", "id_giant", "code_legal", "legal_form", "date_start", "is_discont", 
                          "descr_discont", "code_discont", "date_discont", "postcode", "sbi_full", "sbi_2", 
                          "rating_calamity", "code_reason_calamity", "reason_calamity", "sbi_grouped")

tbl_companies %<>%
  mutate(date_start   = dmy(date_start, locale = Sys.setlocale("LC_TIME", "English")),
         date_discont = dmy(date_discont, locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(date_discont = floor_date(date_discont, "month")) %>% 
  mutate(is_discont   = is_discont == 1) %>% 
  mutate(date_start   = if_else(date_start <= var_date_from, 
                                var_date_from - months(1), 
                                floor_date(date_start, "month"))) %>% 
  filter(!is.na(date_start)) %>% 
  mutate(date_end = if_else(is.na(date_discont), var_date_to, date_discont )) %>% 
  filter(date_end >= var_date_from) %>%                   # Only dates ending dates after the start period of analysis 
  filter(!is.na(date_start) | date_start <= date_end) %>% # Only those that have ending dates after starting dates
  # Remove durations less than a month
  mutate(qty_months = round(as.period(date_end - date_start, unit = "months")/ months(1), 0)) %>% 
  filter(qty_months > 0) %>%
  mutate(sbi_full = ifelse(sbi_full == "0001", "01",
                           ifelse(sbi_full == "0002", "02",
                                  sbi_full)))

# Do nace roll-up ----
tbl_nace_qty <- tbl_nace %>% 
  left_join(tbl_companies, by = c("code" = "sbi_full")) %>% 
  group_by(code, code_parent, layer_no) %>% 
  summarise(qty = n_distinct(id_giant, na.rm = TRUE)) %>% 
  ungroup() 

lst_nace_recoding <- roll_up_nace_tree(tbl_nace_qty, 50000)

tbl_companies %<>% 
  left_join(lst_nace_recoding$tbl_dictionary, by = c("sbi_full" = "code"))

# Aggregate company data ----
vec_months <- data_frame(month = seq(var_date_from, var_date_to, "months"))

tbl_companies_agg <- tbl_companies %>% 
  group_by(code_new,
           month_end = floor_date(date_end, "month"),
           month_start = floor_date(date_start, "month")) %>%
  summarise(qty_companies = n())

library(sqldf)
tbl_companies_agg <- sqldf("select vec_months.month, code_new, sum(qty_companies) as qty_companies
                            from vec_months
                            left join tbl_companies_agg
                              on vec_months.month between tbl_companies_agg.month_start and tbl_companies_agg.month_end
                            group by vec_months.month, code_new")
  
rm(vec_months)

tbl_companies_agg %<>%
  rename(code = code_new) %>% 
  left_join(tbl_nace, by = "code")

# Plot starting and stopping ----
ggplot(tbl_companies_agg, aes(x = month, y = qty_companies)) +
  geom_area(col = col_graydon[2], fill = col_graydon[2], alpha = .6) +
  facet_wrap(~ code) +
  scale_y_continuous(labels = format_number) +
  labs(x = "", y = "# Companies") +
  theme_graydon("grid")

# Number of companies per nace code
tbl_companies_by_nace <- tbl_companies_agg %>% 
  group_by(code, description) %>% 
  summarise(qty_companies = sum(qty_companies))

# Display single nace
tbl_companies_agg %>% 
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
tbl_companies_code <- tbl_companies_agg %>% filter(code == config$code_nace)
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
  theme_graydon("grid")

grid.arrange(p_ts, p_ts_clean)

# Forecasting trends ----
# Create training set
ts_companies_train = subset(ts_companies_clean, end = length(ts_companies_clean) - 6)

# Set up data frame for error evaluation
tbl_model_error <- data_frame(method = as.character(), MASE = as.integer())

# Function for displaying forecast and evaluating model
evaluate_forecast <- function(forecast_model, ts_full){
  
  autoplot(forecast_model,
           main = forecast_model$method,
           ts.colour = col_graydon[1])
  
  #p_res <- checkresiduals(forecast_model)

  data.frame(method = forecast_model$method,
             MASE = accuracy(forecast_model, ts_full)["Test set", "MASE"])
}

# Mean forecasting (mean of all observations)
fit_mean <- meanf(ts_companies_train, h = 6)
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_mean, ts_companies_clean))

# Naive
fit_naive <- naive(ts_companies_train, h = 60)
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_naive, ts_companies_clean))

autoplot(fit_naive) +
  autolayer(fit_naive)

# Simple exponential smoothing
fit_ses <- ses(ts_companies_train, h = 60)
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_ses, ts_companies_clean))

# Holt’s linear trend
fit_holt <- holt(ts_companies_train, h = 60)
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_holt, ts_companies_clean))

# Holt-Winters seasonal method
fit_hw <- hw(ts_companies_train, h = 60, seasonal = "multiplicative")
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_hw, ts_companies_clean))

fit_hw <- hw(ts_companies_train, h = 60, seasonal = "additive")
tbl_model_error <- rbind(tbl_model_error, evaluate_forecast(fit_hw, ts_companies_clean))

# Errors, Trend, and Seasonality (ETS)
fit_ets <- forecast(ets(ts_companies_train))
autoplot(fit_ets)
