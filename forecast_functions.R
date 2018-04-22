# Function for displaying forecast and evaluating model ----
#'
#' @param forecast_model the fitted time series forecast model
#' @param ts_full complete time series
#' @return A list containing the plot with forecast points with full time series and a row with MASE
evaluate_forecast <- function(forecast_model, ts_full){
  
  # Make data frames from the time series objects
  df_forecast <- fortify(forecast_model) %>% 
    select(Index, Data = `Point Forecast`, `Lo 80`, `Hi 80`, `Lo 95`, `Hi 95`) 
  df_full <- fortify(ts_full) 
  
  # Plot displaying the forecasted and full time series
  p_forecast <- ggplot(data = df_forecast, aes(x = Index)) + 
    geom_line(data = df_full, aes(y = Data), col = col_graydon[1]) + 
    geom_line(aes(y = Data), col = col_graydon[2]) + 
    geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`),
                alpha = .4, fill = col_graydon[2]) +
    geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`),
                alpha = .2, fill = col_graydon[2]) +
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

evaluate_forecasts <- function(lst_fitted_models, ts_test){
  
  lst_evaluations <- lapply(lst_fitted_models, evaluate_forecast, ts_test)
  
  tbl_accuracy <- do.call("rbind", sapply(lst_evaluations, "[", 2))
  
  tbl_accuracy %<>%
    gather(key = "measure", value = "value", -method) %>% 
    group_by(measure) %>% 
    mutate(is_best = value == max(value)) %>% 
    ungroup()
}