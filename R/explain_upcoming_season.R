explain_upcoming_season <- function(object, burn, n_obs, seasonality) {
  if (seasonality == 1 || n_obs <= seasonality) return("")
  
  period_name <- "period"
  period_step_name <- "observation"
  period_step_name_s <- ""
  if (seasonality == 7) {
    period_name <- "week"
    period_step_name <- "day"
    period_step_name_s <- "day's"
  }  
  if (seasonality == 12) {
    period_name <- "year"
    period_step_name <- "month"
    period_step_name_s <- "month's"
  }
  
  components <- object$state.contributions
  if (burn > 0) {
    components <- components[-(1:burn), , , drop = FALSE]
  }
  season <- components[, paste0("seasonal.", seasonality, ".1"), ]
  last_season <- season[, (n_obs - seasonality + 1):n_obs]
  
  next_lower <- mean(last_season[, 1] < 0) > 0.8
  next_lowest <- all(mean(last_season[, 1]) <= colMeans(last_season))
  
  next_higher <- mean(last_season[, 1] > 0) > 0.8
  next_highest <- all(mean(last_season[, 1]) >= colMeans(last_season))
  
  if (next_lower) {
    if (next_lowest) {
      explanation <- paste0(
        "Given it's historical seasonality, the series is expected to be at its lowest level next ",
        period_step_name,
        ", as a ", 
        period_name, 
        " ago."
      )
    } else {
      explanation <- paste0(
        "The series' seasonality is expected to pull down next ",
        period_step_name_s,
        " observation, similar to a ",
        period_name,
        " ago."
      )
    }
  } else if (next_higher) {
    if (next_highest) {
      explanation <- paste0(
        "Given it's historical seasonality, the series is expected to be at its highest level next ",
        period_step_name,
        ", as a ", 
        period_name, 
        " ago."
      )
    } else {
      explanation <- paste0(
        "The series' seasonality is expected to bring up next ",
        period_step_name_s,
        " observation, similar to a ",
        period_name,
        " ago."
      )
    }
  } else {
    explanation <- paste0(
      "Next ", 
      period_step_name, 
      ", the series' seasonality will be at its average height per ",
      period_name,
      "."
    )
  }
  
  return(explanation)
}
