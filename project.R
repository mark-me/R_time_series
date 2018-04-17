# Installing and loading libraries
list_of_packages <- c("ggplot2", "dplyr", "magrittr", "purrr", "fst", "ggmap", "ggthemes", "reshape2", "scales",
                      "stringr", "RColorBrewer", "qgraph", "Hmisc", "factoextra", "cluster", "kimisc", "ggrepel", "class",
                      "lubridate", "tidyr", "broom", "funr", "htmltools", "outliers", "readr", "janitor", "ggmosaic",
                      "extrafont", "gridExtra")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
lapply(list_of_packages, require, character.only = TRUE)
rm(list_of_packages, new_packages)

dir_project <- NULL
dir_input <- NULL
dir_output_data <- NULL
dir_output_plots <- NULL

# Creates a subdirectory and/or sets the working directory of a project. ----
#'
#' @param project_name The name of the subdirectory for the project you use/create
#' @param dir_base The directory where you want to have the project created in. The default is the current working directory
#' @examples
#' open_project("My project")
#' open_project("My project" , "~/R Scripts")
open_project <- function(project_name, dir_base = NULL) {

  #this_file_location <- "~/R Scripts/project.r"
  name_project <- project_name
  dir_base <- ifelse(is.na(dir_base), getwd(), dir_base)
  
  # Project directory
  dir.create(file.path(dir_base, name_project), showWarnings = FALSE)
  setwd(file.path(dir_base, name_project))
  dir_project <<- paste0(dir_base, "/", name_project)
  
  # Data directory
  dir.create(file.path(dir_project, "Input"), showWarnings = FALSE)
  dir_input <<- paste0(dir_project, "/", "Input")
  
  # Output data directory
  dir.create(file.path(dir_project, "Output data"), showWarnings = FALSE)
  dir_output_data <<- paste0(dir_project, "/", "Output data")

  # Output plot directory
  dir.create(file.path(dir_project, "Output plots"), showWarnings = FALSE)
  dir_output_plots <<- paste0(dir_project, "/", "Output plots")
  
  # Presentation directory
  dir.create(file.path(dir_project, "Presentation"), showWarnings = FALSE)
  
  # Make a copy of the current file 
  #file.copy(this_file_location, dir_project)
  
  # Create a project file called 'main.r'
  if (!file.exists("main.r")) {
	fileConn<-file("main.r")
	cmd_open_project = paste0("open_project(\"",name_project, "\", \"", dir_base, "\")" )
	writeLines(c("source(\"project.r\")", cmd_open_project), fileConn)
	close(fileConn)
  }
}

# Copy a dataframe to the clipboard. ----
#'
#' @param df The data frame you want to put on the clipboard
#' @param row.names Indicates whether you want to copy the row names as well, default is FALSE.
#' @param col.names Indicates whether you want to copy the column names as well, default is TRUE.
df_to_clipboard <-
  function(df,
           row.names = FALSE,
           col.names = TRUE,
           ...) {
    write.table(
      df,
      "clipboard-16384",
      sep = "\t",
      row.names = row.names,
      col.names = col.names,
      ...
    )
  }

# Sets first letter of a string to capital -----
#' @param x The number you want to format as euros
#' @param largest_with_cents The largest number that should still be formatted using cents.
#' @return A string containing the euro formatted number
str_firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Formats a number into a euro string -----
#' @param x The number you want to format as euros
#' @param largest_with_cents The largest number that should still be formatted using cents.
#' @return A string containing the euro formatted number
format_euro_currency <- function(x, largest_with_cents = 10000) {
  x <- round(x, digits = 2)
  if (max(x, na.rm = TRUE) < largest_with_cents &
      !all(x == floor(x), na.rm = TRUE)) {
    nsmall <- 2L
  } else {
    x <- round(x, digits = 2)
    nsmall <- 0L
  }
  return(paste0(
    intToUtf8(8364),
    format(
      x,
      nsmall = nsmall,
      trim = TRUE,
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE,
      digits = 1L
    )
  ))
}

# Formats a number into a euro string -----
#' @param x The number you want to format as euros
#' @param display_as how you want to display the euro amount: by thousands "k", by millions "Mln" or "normal"
#' @param largest_with_cents The largest number that should still be formatted using cents.
#' @return A string containing the euro formatted number
format_euro_currency2 <- function(x, display_as = NA, no_decimals = NA) {
  
  display_as <- ifelse(is.na(display_as), "normal", display_as)
  
  # Change suffix depending (none, "k" or "Mln.") and divide
  print(display_as)
  suffix <- ifelse(display_as == "normal", "", display_as)
  if (display_as == "normal") {
    suffix <- ""
    no_decimals <- ifelse(is.na(no_decimals), 2, no_decimals)
  } 
  else if (display_as == "k") {
    suffix <- "k"
    x <- x / 10000
    no_decimals <- ifelse(is.na(no_decimals), 1, no_decimals)
  } 
  else if (display_as == "Mln") {
    suffix <- " Mln."
    no_decimals <- ifelse(is.na(no_decimals), 1, no_decimals)
    x <- x / 10 ^ 6
  }
  
  x <- round(x, digits = no_decimals)
  
  return(paste0( intToUtf8(8364),
                 format(x,
                        trim = TRUE,
                        big.mark = ".",
                        decimal.mark = ",",
                        scientific = FALSE,
                        nsmall = no_decimals
                 ),
                 suffix
  )
  )
}

#' Formats a number by adding . (thousands) and , (decimals) to the number ----
#'
#' @param x The number you want to format
#' @param num_digits The number of decimal places you want shown. Default is 0
#' @return A string containing the formatted number
format_number <- function(x, num_digits = 0) {
  formatC(
    x,
    format = "f",
    big.mark = '.',
    decimal = ',',
    digits = num_digits
  )
}

#' Formats a number to percentage by adding . (thousands), "," (decimals) and % to the number ----
#'
#' @param x The number you want to format
#' @param num_digits The number of decimal places you want shown. Default is 1
#' @return A string containing the formatted number
format_percent <- function(x, num_digits = 1) {
  
  x <- x * 100
  
  x <- formatC(
    x,
    format = "f",
    big.mark = '.',
    decimal = ',',
    digits = num_digits
  )
  
  x <- paste0(x, "%")
  return(x)
}

# Gets the last day of the month of a date in the format YYYY-MM-DD ----
#'
#' @param date you'll want to get
end_of_month <- function(date) {
  date <- as.POSIXct(date)
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

# Graydon color palette -----
col_graydon <- c("#00545D", "#EB6E08", "#858587", "#8BB0DE", "#186FA7", "#474646", "#BABFC1", "#000000", "#3AA2DF")

# Graydon theming ----
font_import(pattern = "Roboto", prompt = FALSE)
loadfonts(device="win")

theme_graydon <- function(type = c("grid", "horizontal", "blank")) {
  
  graydon_theme <- 
    theme_gdocs() + 
    theme(axis.title = element_text(face = "plain"),
          panel.grid.major = element_line(colour = "#BABFC1"),
          plot.background = element_blank(), 
          axis.line = element_line(colour = "#474646"),
          text = element_text(family = "Roboto Medium",
                              color = "#474646")
    )
  
  if (type == "horizontal") {
 
    graydon_theme <- graydon_theme +
      theme(panel.grid.major.x = element_blank(),
            axis.line.y = element_blank()
      )
    
  } else if (type == "blank") {
    
    graydon_theme <- theme_gdocs() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            rect = element_blank(),
            text = element_text(family = "Roboto Medium",
                                color = "#474646")
      )
    
  }
  return(graydon_theme)
}

scale_color_graydon <- function(){
  return(scale_color_manual(values = col_graydon))
}

scale_fill_graydon <- function(){
  return(scale_fill_manual(values = col_graydon))
}

# Downloading and extracting zip ----
download_extract <- function(url, dest_dir, zip_file = NA){
  
  # Create directory if nog exists
  if (!file.exists(dest_dir)) {
    dir.create(dest_dir)
  }
  
  if(is.na(zip_file)){
    file <- paste(dest_dir, basename(url), sep = '/')
  } else {
    file <- paste(dest_dir, zip_file, sep = '/')
  }
  
  # Download and unzip if zip file does not exist
  if (!file.exists(file)) {
    download.file(url, file)
    unzip(file, exdir = dest_dir)
  }
  
}
