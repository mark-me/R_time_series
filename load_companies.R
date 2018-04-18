# Load and transform company information ----
prep_companies <- function(do_processing, date_from, date_to){
  
  if(do_processing){
    # Load source data
    tbl_companies <- read_csv2(paste0(dir_input, "/WORK_QUERY_FOR_FILTER_FOR_BEDRIJVEN_N.csv"))
    names(tbl_companies) <- c("temp", "id_giant", "code_legal", "legal_form", "date_start", "is_discont", 
                              "descr_discont", "code_discont", "date_discont", "postcode", "sbi_full", "sbi_2", 
                              "rating_calamity", "code_reason_calamity", "reason_calamity", "sbi_grouped")
    
    # Transform source data in data frame tbl_companies
    tbl_companies %<>%
      mutate(date_start   = dmy(date_start, locale = Sys.setlocale("LC_TIME", "English")),
             date_discont = dmy(date_discont, locale = Sys.setlocale("LC_TIME", "English"))) %>% 
      mutate(date_discont = floor_date(date_discont, "month")) %>% 
      mutate(is_discont   = is_discont == 1) %>% 
      mutate(date_start   = if_else(date_start <= date_from, 
                                    date_from - months(1), 
                                    floor_date(date_start, "month"))) %>% 
      filter(!is.na(date_start)) %>% 
      mutate(date_end = if_else(is.na(date_discont), date_to, date_discont )) %>% 
      filter(date_end >= date_from) %>%                   # Only dates ending dates after the start period of analysis 
      filter(!is.na(date_start) | date_start <= date_end) %>% # Only those that have ending dates after starting dates
      # Remove durations less than a month
      mutate(qty_months = round(as.period(date_end - date_start, unit = "months")/ months(1), 0)) %>% 
      filter(qty_months > 0) %>%
      mutate(sbi_full = ifelse(sbi_full == "0001", "01",
                               ifelse(sbi_full == "0002", "02",
                                      sbi_full)))
    
    # Write dataframe 'tbl_companies' to processed file
    saveRDS(tbl_companies, file = paste0(dir_input, "/companies.RDS"))
    
  } else {
    # Load previously processed data in a data frame 'tbl_companies'
    tbl_companies <- read_rds(paste0(dir_input, "/companies.RDS"))
  }
  stopifnot(exists("tbl_companies"))  # If 'tbl_companies' doesn't exists, stop the script
  return(tbl_companies)
}

# Aggregate company data ----
aggregate_companies <- function(tbl_companies, tbl_dictionary){
  
  # Aggregate company data ----
  vec_months <- data_frame(month = seq(var_date_from, var_date_to, "months"))
  
  tbl_companies_aggr <- tbl_companies %>% 
    group_by(code_new,
             month_end = floor_date(date_end, "month"),
             month_start = floor_date(date_start, "month")) %>%
    summarise(qty_companies = n())
  
  library(sqldf)
  tbl_companies_aggr <- sqldf("select vec_months.month, code_new, sum(qty_companies) as qty_companies
                               from vec_months
                               left join tbl_companies_aggr
                               on vec_months.month between tbl_companies_aggr.month_start and tbl_companies_aggr.month_end
                               group by vec_months.month, code_new")
  
  rm(vec_months)

  return(tbl_companies_aggr)  
}