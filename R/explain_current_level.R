explain_current_level <- function(object, burn, n_obs, seasonality) {
  period_name <- "period"
  if (seasonality == 7) period_name <- "week"
  if (seasonality == 12) period_name <- "year"
  
  components <- object$state.contributions
  if (burn > 0) {
    components <- components[-(1:burn), , , drop = FALSE]
  }
  
  trend <- components[, "trend", ]
  
  current_level <- trend[, n_obs, drop = TRUE]
  current_level_before <- trend[, n_obs - 1, drop = TRUE]
  current_level_before_season <- trend[, n_obs - seasonality, drop = TRUE]
  avg_level <- rowMeans(trend)
  period_level <- rowMeans(trend[, (n_obs - seasonality + 1):n_obs, drop = FALSE])
  
  if (mean(current_level > avg_level) > 0.8) {
    description_change_long <- "higher than"
  } else if (mean(current_level < avg_level) > 0.8) {
    description_change_long <- "lower than"
  } else {
    description_change_long <- "about as high as"
  }
  
  if (mean(current_level > current_level_before_season) > 0.8) {
    description_change_short <- "higher than"
  } else if (mean(current_level < current_level_before_season) > 0.8) {
    description_change_short <- "lower than"
  } else {
    description_change_short <- "about as high as"
  }
  
  
  explanation <- paste0(
    "The current level of the series is ", 
    description_change_long,
    " the average level observed in the history of the series"
  )
  
  if (seasonality > 1) {
    explanation <- paste0(
      explanation,
      ", and ",
      description_change_short,
      " a ", 
      period_name, 
      " ago."
    )
  } else {
    explanation <- paste0(explanation, ".")
  }
  
  if (mean(current_level > current_level_before) > 0.8) {
    explanation <- paste0(
      explanation, 
      " The current level is increasing which is likely continuing into the forecast."
    )
  } else if (mean(current_level < current_level_before) > 0.8) {
    explanation <- paste0(
      explanation, 
      " The current level is decreasing which is likely continuing into the forecast."
    )
  } else {
    explanation <- paste0(
      explanation,
      " The current level is neither clearly decreasing nor clearly increasing."
    )
  }
  
  return(explanation)
}
