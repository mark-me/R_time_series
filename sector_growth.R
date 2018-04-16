home_dir <- "~/R scripts"
#home_dir <- "~/Downloads/Dropbox/Werk/R\ Scripts/"
setwd(paste0(home_dir, "R_time_series/"))
source("project.R")
source("roll_up_nace_tree.R")
open_project("R_time_series", home_dir)

var_date_from <- as.Date("1997-01-01") # Start date of time series
var_date_to <- as.Date("2017-10-01")   # End date of time series

# Import branche hierarchy data ----
tbl_nace <- read.csv2(paste0(dir_input, "/branche_hierarchy.csv"), stringsAsFactors = FALSE)

# Import & transform company data ----
tbl_companies <- read_csv2(paste0(dir_input, "/WORK_QUERY_FOR_FILTER_FOR_BEDRIJVEN_N.csv"))
names(tbl_companies) <- c("temp", "id_giant", "code_legal", "legal_form", "date_start", "is_discont", 
                          "descr_discont", "code_discont", "date_discont", "postcode", "sbi_full", "sbi_2", 
                          "rating_calamity", "code_reason_calamity", "reason_calamity", "sbi_grouped")

tbl_companies %<>%
  mutate(date_start   = dmy(date_start, locale = Sys.setlocale("LC_TIME", "English")),
         date_discont = dmy(date_discont, locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(date_discont = floor_date(date_discont, "month")) %>% 
  mutate(is_discont   = is_discont == 1) %>% 
  mutate(date_start   = if_else(date_start <= var_date_from, 
                                as.Date("1996-12-01"), 
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


# Aggregated for starting and stopping companies ----
# Starting companies
tbl_start <- tbl_companies %>% 
  mutate(set = "Starting") %>% 
  filter(date_start >= var_date_from) %>% 
  rename(date = date_start) %>% 
  group_by(code_new, date) %>% 
  summarise(qty_started = n()) %>% 
  ungroup() 

# Discontinued companies
tbl_discontinued <- tbl_companies %>%
  mutate(set = "Discontinued") %>% 
  filter(!is.na(date_discont)) %>%
  filter(date_discont < var_date_to) %>% 
  filter(is_discont) %>% 
  rename(date = date_discont) %>% 
  group_by(code_new, date) %>% 
  summarise(qty_discontinued = n()) %>% 
  ungroup()

# Combining started & discontinued ----
tbl_start_discont <- tbl_start %>% 
  tidyr::complete(date, nesting(code_new), 
                  fill = list(qty_started = 0)) %>% 
  left_join(tbl_discontinued, by = c("date", "code_new")) %>% 
  mutate(qty_discontinued = ifelse(is.na(qty_discontinued), 0, qty_discontinued)) %>% 
  rename(code = code_new) %>% 
  left_join(tbl_nace, by = "code")

# Plotting started and stopped companies -----
tbl_start_discont %>% 
  filter(!is.na(description) & code == "74") %>% 
ggplot(aes(x = date))  +
  geom_area(aes(y = qty_started), 
            alpha = .6, fill = col_graydon[1], col = col_graydon[1]) +
  geom_area(aes(y = qty_discontinued), 
            alpha = .6, fill = col_graydon[2], col = col_graydon[2]) +
  scale_y_continuous(labels = format_number) + 
  #scale_x_date(limits = c(as.Date("2008-01-01"), NA)) +
  facet_wrap(~description, ncol = 6) +
  theme_graydon("grid")
