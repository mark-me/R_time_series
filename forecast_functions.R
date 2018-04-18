# Function for displaying forecast and evaluating model ----
evaluate_forecast <- function(forecast_model, ts_full){
  
  df_forecast <- fortify(forecast_model) %>% 
    select(Index, Data = `Point Forecast`, `Lo 80`, `Hi 80`, `Lo 95`, `Hi 95`) 
  df_full <- fortify(ts_full) 
  
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
  
  mase <- data.frame(method = forecast_model$method,
                     MASE = accuracy(forecast_model, ts_full)["Test set", "MASE"])
  
  list(p_forecast = p_forecast,
       mase = mase)
}

# Comparing two time series ----
plot_time_series <- function(ts_a, name_a, ts_b, name_b){
  
  df_full <- rbind(fortify(ts_a) %>% mutate(set = name_a),
                   fortify(ts_b) %>% mutate(set = name_b))

  p_plot <- ggplot(df_full, aes(x = Index, y = Data)) + 
    geom_line(aes(col = set)) + 
    scale_color_graydon() +
    scale_y_continuous(labels = format_number) +
    labs(title = forecast_model$method, x = "", y = "", col = "") +
    theme_graydon("grid")
  
  return(p_plot)
}
