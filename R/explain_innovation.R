explain_innovation <- function(object, 
                               burn, 
                               sigma_obs = "sigma.obs",
                               sigma_level = "trend.level.sd",
                               sigma_season = "sigma.seasonal.12",
                               sigma_trend = "trend.slope.sd") {
  
  mcmc_index <- 1:object$niter
  if (burn > 0) mcmc_index <- mcmc_index[-(1:burn)]
  
  sigma_obs_size <- median(object[[sigma_obs]][mcmc_index])
  
  sigma_season_size <- 0
  if (!is.null(sigma_season)) {
    sigma_season_size <- median(object[[sigma_season]][mcmc_index])
  }
  
  sigma_trend_size <- 0
  if (!is.null(sigma_trend)) {
    sigma_trend_size <- median(object[[sigma_trend]][mcmc_index])
  }
  
  sigma_level_size <- 0
  sigma_level_total_size <- 0
  if (!is.null(sigma_level)) {
    sigma_level_size <- median(object[[sigma_level]][mcmc_index])
    
    if (!is.null(sigma_trend)) {
      sigma_level_total_size <- median(
        sqrt(
          object[[sigma_level]][mcmc_index]^2 +
            object[[sigma_trend]][mcmc_index]^2
        )
      )
    }
  }
  
  if (sigma_trend_size == 0) {
    explanation_level <- ""
  } else if (sigma_level_size > 1.5 * sigma_trend_size) {
    explanation_level <- " The level of the time series has historically fluctuated rather due to noise than due to changes in the trend."
  } else if (sigma_trend_size > 1.5 * sigma_level_size) {
    explanation_level <- " The level of the time series has historically fluctuated primarily due to changes in the trend."
  } else {
    explanation_level <- " The level of the time series fluctuated historically both due to noise and due to changes in the trend."
  }
  
  if (sigma_obs_size > 1.5 * sigma_level_total_size &&
      sigma_obs_size > 1.5 * sigma_season_size) {
    explanation <- "The forecast uncertainty is largely driven by observation noise, more so than by changes in level or seasonal effects."
  } else if (sigma_level_total_size > 1.5 * sigma_obs_size &&
             sigma_level_total_size > 1.5 * sigma_season_size) {
    explanation <- "The forecast uncertainty is largely driven by fluctuations in the level of the time series."
    explanation <- paste0(explanation, explanation_level)
  } else if (sigma_season_size > 1.5 * sigma_obs_size &&
             sigma_season_size > 1.5 * sigma_level_total_size) {
    explanation <- "The forecast uncertainty is largely driven by fluctuations of the seasonal effects."
  } else if (sigma_obs_size > 1.5 * sigma_season_size &&
             sigma_level_total_size > 1.5 * sigma_season_size) {
    explanation <- "Both observation noise and changes of the level of the series contribute to the forecast uncertainty."
    explanation <- paste0(explanation, explanation_level)
  } else {
    explanation <- ""
  }
  
  return(explanation)
}

get_sigma_obs_name <- function(components) {
  switch(
    components,
    SeasonalSemilocalLinearTrend = "sigma.obs",
    SeasonalLocalLevel = "sigma.obs",
    LocalLevel = "sigma.obs"
  )
}

get_sigma_level_name <- function(components) {
  switch(
    components,
    SeasonalSemilocalLinearTrend = "trend.level.sd",
    SeasonalLocalLevel = "sigma.level",
    LocalLevel = "sigma.level"
  )
}

get_sigma_trend_name <- function(components) {
  switch(
    components,
    SeasonalSemilocalLinearTrend = "trend.slope.sd",
    SeasonalLocalLevel = NULL,
    LocalLevel = NULL
  )
  
}

get_sigma_season_name <- function(components, seasonality) {
  switch(
    components,
    SeasonalSemilocalLinearTrend = paste0("sigma.seasonal.", seasonality),
    SeasonalLocalLevel = paste0("sigma.seasonal.", seasonality),
    LocalLevel = NULL
  )
}
