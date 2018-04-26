# Function for displaying forecast and accuracy of a time series model ----
#'
#' @param forecast_model the fitted time series forecast model
#' @param ts_full complete time series
#' @return A list containing the plot with forecast points with full time series and a row with accuracy measures
evaluate_forecast <- function(forecast_model, ts_full){
  
  # Make data frames from the time series objects
  df_forecast <- fortify(forecast_model) %>% 
    select(Index, Data = `Point Forecast`, `Lo 80`, `Hi 80`, `Lo 95`, `Hi 95`) 
  df_full <- fortify(ts_full) 
  
  # Plot displaying the forecasted and full time series
  p_forecast <- ggplot(data = df_forecast, aes(x = Index)) + 
    geom_line(data = df_full, aes(y = Data), col = col_graydon[1]) + 
    geom_line(aes(y = Data), col = col_graydon[4]) + 
    geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`),
                alpha = .4, fill = col_graydon[4]) +
    geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`),
                alpha = .2, fill = col_graydon[4]) +
    scale_color_graydon() +
    scale_y_continuous(labels = format_number) +
    labs(title = forecast_model$method, x = "", y = "", col = "") +
    theme_graydon("grid")
  
  # Row of data for the method, and MASE of the test time series
  error_measure <- data.frame(value = accuracy(forecast_model, ts_full)["Test set",])
  error_measure %<>% 
    mutate(measure = row.names(error_measure)) %>% 
    spread(measure, value) %>% 
    mutate(method = forecast_model$method)

  # Returning a list with the plot and MASE
  list(p_forecast = p_forecast,
       error_measures = error_measure)
}

# Function for displaying forecast and evaluating accuracy of multiple time series models ----
#'
#' @param lst_fitted_models a list of fitted time series forecast models
#' @param ts_test the test set 
#' @return A list containing the plot with forecast points with full time series and a data frame with accuracy measures
evaluate_forecasts <- function(lst_fitted_models, 
                               ts_test, 
                               accuracy_measures = c("ACF1", "MAE", "MAPE", "MASE", "ME", "MPE", "RMSE", "Theil's U")){
  
  lst_evaluations <- lapply(lst_fitted_models, evaluate_forecast, ts_test)
  
  lst_plot_forecast <- lapply(lst_evaluations, "[", 1)
  
  tbl_accuracy <- do.call("rbind", sapply(lst_evaluations, "[", 2))
  
  tbl_accuracy %<>%
    gather(key = "measure", value = "value", -method) %>% 
    group_by(measure) %>% 
    filter(measure %in% accuracy_measures) %>% 
    mutate(is_best = value == min(value)) %>% 
    ungroup()
  
  p_accuracy <- ggplot(tbl_accuracy, 
                       aes(x = reorder(method, value), y = value)) +
    geom_col(aes(fill = is_best)) +
    geom_text(aes(label = round(value, 2)), 
              hjust = -.1) +
    facet_wrap(~measure, scales = "free", ncol = 2) +
    scale_fill_graydon() +
    coord_flip() +
    labs(x = "") +
    guides(fill = FALSE) +
    theme_graydon("vertical")
  
  return (list(lst_plot_forecast = lst_plot_forecast,
               tbl_accuracy = tbl_accuracy, 
               p_accuracy = p_accuracy))
}

# Plot for comparing time series ----
#'
#' @param lst_timeseries list containing all time series
#' @param vec_names vector containing the names that should be given to each time series
#' @return A ggplot
plot_time_series <- function(lst_timeseries, vec_names){

  # Create a combined data frame of all time series objects
  lst_df <- do.call("rbind", lapply(lst_timeseries, FUN = "fortify"))
  # Create column naming each time series
  vec_length <- sapply(lst_timeseries, "length")
  tseries_names <- c(rep(vec_names, vec_length))
  # Add column to the data frame
  df_series <- cbind(tseries_names, lst_df)
  
  p_plot <- ggplot(df_series, aes(x = Index, y = Data)) + 
    geom_line(aes(col = tseries_names)) + 
    scale_color_graydon() +
    scale_y_continuous(labels = format_number) +
    labs(x = "", y = "", col = "") +
    theme_graydon("grid")
  
  return(p_plot)  
}

# Decomposition of time series ----
#'
#' @param ts_data Time series data
#' @return A ggplot
decompose_ts <- function(ts_data) {
  df_stl <- ts_data %>% 
    stl(t.window=13, s.window="periodic", robust=TRUE)
  
  df_stl <- fortify(df_stl)
  
  df_stl %<>% 
    gather(key = "component", value = "value", -Index) %>% 
    mutate(component = paste0(toupper(str_sub(component, 1,1)) , 
                              str_sub(component, start = 2))) %>% 
    mutate(component = ordered(component, c("Data", "Trend", "Seasonal", "Remainder")))
  
  p_stl <- ggplot(df_stl, aes(x = Index, y = value, col = component)) +
    geom_line() +
    facet_wrap(~component, scales = "free", ncol = 1) +
    scale_y_continuous(labels = format_number) +
    scale_color_graydon() +
    labs(x = "", y = "") +
    guides(col = FALSE) +
    theme_graydon("grid")
  
  return(list(df_stl = df_stl,
              p_stl = p_stl))
}

