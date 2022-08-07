#' Explain anomalies found in the time series
#' 
#' @examples
#' library(bsts)
#' 
#' y <- log(AirPassengers)
#' y[length(y) - 50] <- y[length(y) - 50] + 2
#' plot(y)
#' 
#' ss <- AddSemilocalLinearTrend(list(), y)
#' ss <- AddSeasonal(ss, y, nseasons = 12)
#' model <- bsts(y, state.specification = ss, niter = 5000)
#' pred <- predict(model, horizon = 12, burn = 1000)
#' 
#' par(mfrow = c(1,2))
#' plot(model)
#' plot(pred)
#' 
#' plot(model, "residuals")
#' plot(model, "prediction.errors")
#' 
#' explain_anomalies(
#'   object = model,
#'   burn = 1000,
#'   n_obs = length(y),
#'   seasonality = 12,
#'   threshold = 3
#' )
#' 
explain_anomalies <- function(object, burn, n_obs, seasonality, threshold = 3) {
  
  # One can recompute the prediction errors as follows:
  # errors <- bsts::bsts.prediction.errors(
  #   bsts.object = object,
  #   cutpoints = NULL,
  #   burn = burn,
  #   standardize = TRUE
  # )
  
  result <- list(
    explanation = "No anomalous observations were detected in the history of the time series.",
    anomalies = integer()
  )
  
  period_name <- "period"
  period_step_name <- "observation"
  if (seasonality == 7) {
    period_name <- "week"
    period_step_name <- "day"
  }  
  if (seasonality == 12) {
    period_name <- "year"
    period_step_name <- "month"
  }
  
  mcmc_index <- 1:object$niter
  if (burn > 0) mcmc_index <- mcmc_index[-(1:burn)]
  
  errors <- object$one.step.prediction.errors[mcmc_index, ]
  
  errors_sd <- matrix(
    data = sqrt(rowMeans(errors^2) - rowMeans(errors)^2),
    nrow = nrow(errors), ncol = ncol(errors),
    byrow = FALSE
  )
  
  errors_std <- errors / errors_sd
  anomaly_candidates <- which(colMeans(abs(errors_std) > threshold) > 0.8)
  
  result$anomalies <- anomaly_candidates
  
  if (length(anomaly_candidates) == 0) {
    return(result)
  }
  
  obs_since_anomalies <- n_obs - anomaly_candidates
  obs_since_last_anomaly <- min(obs_since_anomalies)
  n_anomalies_not_previous <- length(obs_since_anomalies) - 1
  
  if (obs_since_last_anomaly == 0) {
    explanation <- "The most recent observation has been identified as an anomaly. That observation might disturb the forecast. It could also indicate an upcoming level shift to which the forecast should adjust, but hasn't yet given too little evidence."
    if (n_anomalies_not_previous > 0) {
      obs_since_last_anomaly_not_previous <-
        min(obs_since_anomalies[obs_since_anomalies != 0])
      
      if (obs_since_last_anomaly_not_previous > 2 * seasonality) {
        periods_since_last_anomaly_not_previous <- paste0(
          round(obs_since_last_anomaly_not_previous / seasonality, 1),
          " ", period_name, "s"
        )
      } else {
        obs_since_last_anomaly_not_previous <- paste0(
          obs_since_last_anomaly_not_previous,
          " ", period_step_name, "s"
        )
      }
      
      if (n_anomalies_not_previous == 1) {
        explanation <- paste0(
          explanation,
          " There has been one more anomaly, ",
          periods_since_last_anomaly_not_previous,
          " ago."
        )
      } else {
        explanation <- paste0(
          explanation,
          " There have been several other anomalies previously, the most recent one ",
          periods_since_last_anomaly_not_previous,
          " ago."
        )
      }
    }
    
    result$explanation <- explanation
    return(result)
  }
  
  # if the function did not yet return, then there is at least one anomaly, and
  # there is no anomaly at the most recent observation
  
  if (obs_since_last_anomaly > 2 * seasonality) {
    periods_since_last_anomaly <- paste0(
      round(obs_since_last_anomaly / seasonality, 1),
      " ", period_name, "s"
    )
  } else if (obs_since_last_anomaly == 0) {
    periods_since_last_anomaly <- paste0("previous ", period_step_name)
  } else {
    periods_since_last_anomaly <- paste0(
      obs_since_last_anomaly,
      " ", period_step_name, "s"
    )
  }
  
  if (length(anomaly_candidates) == 1) {
    explanation <- paste0(
      "Over the course of the series, there has been a single anomaly ",
      periods_since_last_anomaly,
      " ago."
    )
  } else {
    explanation <- paste0(
      "There have been several anomalies over the history of the series, the most recent one ",
      periods_since_last_anomaly,
      " ago."
    )
  }
  
  result$explanation <- explanation
  return(result)
}
