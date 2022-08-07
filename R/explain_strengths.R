get_strengths <- function(object, burn, seasonality) {
  
  season_name <- paste0("seasonal.", seasonality, ".1")
  # as in `bsts::residuals.bsts()`
  components <- object$state.contributions
  if (burn > 0) {
    components <- components[-(1:burn), , , drop = FALSE]
  }
  
  # moving `component` dimension to the end to sum over it via `rowSums()`
  fitted <- rowSums(aperm(components, c(1, 3, 2)), dims = 2)
  trend <- aperm(components, c(1, 3, 2))[, , "trend"]
  season <- aperm(components, c(1, 3, 2))[, , season_name]
  residuals <- t(as.numeric(object$original.series) - t(fitted))
  detrended <- t(as.numeric(object$original.series) - t(trend))
  deseasoned <- t(as.numeric(object$original.series) - t(season))
  
  # similar idea to what is done in `forecast::tsoutliers()`
  season_strength <- median(
    1 - (rowMeans(x = residuals^2) - rowMeans(x = residuals)^2) /
      (rowMeans(x = detrended^2) - rowMeans(x = detrended)^2)
  )
  trend_strength <- median(
    1 - (rowMeans(x = residuals^2) - rowMeans(x = residuals)^2) /
      (rowMeans(x = deseasoned^2) - rowMeans(x = deseasoned)^2)
  )
  
  return(
    list(
      season = season_strength,
      trend = trend_strength
    )
  )
}

get_component_range <- function(object, burn, component_name) {
  components <- object$state.contributions
  if (burn > 0) {
    components <- components[-(1:burn), , , drop = FALSE]
  }
  
  range_size <- function(x) max(x) - min(x)
  
  median_trend_range <- abs(median(
    apply(
      X = components[, component_name, ], MARGIN = 1, FUN = range_size
    )
  ))
  
  return(median_trend_range)
}

get_avg_trend_range_per_period <- function(object, burn, n_obs, seasonality) {
  trend_range <- get_component_range(
    object = object, burn = burn, component_name = "trend"
  )
  
  avg_trend_range_per_period <- trend_range / n_obs * seasonality
  
  return(avg_trend_range_per_period)
}

get_period_name <- function(seasonality) {
  period_name <- "period"
  if (seasonality == 7) period_name <- "week"
  if (seasonality == 12) period_name <- "year"
  
  return(period_name)
}

explain_strengths <- function(object, burn, n_obs, seasonality) {
  
  period_name <- get_period_name(seasonality = seasonality)
  
  strengths <- get_strengths(
    object = object, burn = burn, seasonality = seasonality
  )
  
  relative_range <- get_component_range(
    object = object, 
    burn = burn, 
    component_name = paste0("seasonal.", seasonality, ".1")
  ) / get_avg_trend_range_per_period(
    object = object,
    burn = burn,
    n_obs = n_obs,
    seasonality = seasonality
  )
  
  if (relative_range > 2) {
    explanation_relative <- " Within a given %s, the seasonal variation will dominate the change caused by the trend."
  } else if (relative_range < 0.5) {
    explanation_relative <- " Within a given %s, the change due to the trend will dominate the seasonal variation."
  } else {
    explanation_relative <- " Within a given %s, neither trend nor seasonality dominates."
  }
  explanation_relative <- sprintf(explanation_relative, period_name)
  
  if (strengths$season < 0.6 && strengths$trend < 0.6) {
    explanation <- "The impact of neither trend nor season is large on the series and the model's forecast."
  } else if (strengths$season >= 0.6 && strengths$trend < 0.6) {
    explanation <- "The model estimates a strong seasonal component in the series which will be projected into the future."
  } else if (strengths$season < 0.6 && strengths$trend >= 0.6) {
    explanation <- "The model estimates a strong trend component in the series which will be projected into the future."
  } else if (strengths$season > 0.9 && strengths$trend > 0.9) {
    explanation <- paste0(
      "Trend and seasonality dominate other variation in the series. The forecast is largely driven by these two signals.",
      explanation_relative
    )
  } else if (strengths$season > 0.6 && strengths$trend > 0.6) {
    explanation <- paste0(
      "The model estimates that the series has both a clear trend and seasonality. The forecast will be represent these factors.",
      explanation_relative
    )
  }
  
  return(explanation)
}
