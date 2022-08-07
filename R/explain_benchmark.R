explain_benchmark <- function(gof_benchmark,
                              n_obs,
                              seasonality,
                              components_str,
                              uses_trend = FALSE,
                              uses_seasonality = FALSE) {
  e <- list(
    explanation = NA_character_,
    gof_snaive = gof_benchmark$snaive,
    gof_runrate = gof_benchmark$runrate,
    better_than_snaive = integer(),
    better_than_runrate = integer(),
    worse_than_snaive = integer(),
    worse_than_runrate = integer()
  )
  
  if (seasonality == 1) {
    e$explanation <- "As the series is not suspected to follow any seasonality, we do not compare the model against further benchmarks."
    e$gof_snaive <- NA
    e$gof_runrate <- NA
    
    return(e)
  }
  if (n_obs < 2 * seasonality) {
    e$explanation <- "As the series has less than two seasonal periods of observations, the model was not compared against the seasonal naive and runrate benchmarks."
    e$gof_snaive <- NA
    e$gof_runrate <- NA
    
    return(e)
  }
  
  if (seasonality == 12) {
    snaive_description <- "the value from a year ago"
    runrate_description <- "the current year's runrate"
  } else if (seasonality == 7) {
    snaive_description <- "the value from a week ago"
    runrate_description <- "the current week's runrate"
  } else {
    snaive_description <- "the value from a period ago"
    runrate_description <- "the current period's runrate"
  }
  
  if (e$gof_snaive < 0.05 && isTRUE(e$gof_runrate < 0.05)) {
    e$worse_than_runrate <- get_different_obs(
      gof_benchmark$share_of_runrate_errors, improvement = FALSE
    )
    e$worse_than_snaive <- get_different_obs(
      gof_benchmark$share_of_snaive_errors, improvement = FALSE
    )
    
    runrate_better <- e$gof_snaive > e$gof_runrate
    
    e$explanation <- paste0(
      "Historically, the model has not predicted the series at hand better than ",
      snaive_description, " or ", runrate_description, 
      " would have. The model might not be capturing ",
      c("the series' seasonality", "level shifts")[runrate_better + 1], 
      " properly, or it might not be sufficiently robust against anomalies.",
      " It's preferable to use ",
      c(snaive_description, "the runrate")[runrate_better + 1],
      " as forecast."
    )
  } else if (isTRUE(e$gof_runrate < 0.05)) {
    e$worse_than_runrate <- get_different_obs(
      gof_benchmark$share_of_runrate_errors, improvement = FALSE
    )
    
    e$explanation <- paste0(
      "Historically, the model has not predicted the series at hand better than ",
      runrate_description, 
      " would have."
    )
    
    if (components_str == "LocalLevel") {
      e$explanation <- paste0(
        e$explanation,
        " However, since the model is a Local-Level specification, this is not unexpected as it captures similar effects as the runrate."
      )
    } else {
      e$explanation <- paste0(
        e$explanation,
        " The model might not be capturing ",
        "level shifts properly,",
        c("", " extrapolating unnecessary seasonal effects,")[uses_seasonality + 1],
        " or it might not be sufficiently robust against anomalies.",
        " It's preferable to use the runrate as forecast."
      )
    }
  } else if (e$gof_snaive < 0.05) {
    e$worse_than_snaive <- get_different_obs(
      gof_benchmark$share_of_snaive_errors, improvement = FALSE
    )
    
    e$explanation <- paste0(
      "Historically, the model has not predicted the series at hand better than ",
      snaive_description, 
      " would have."
    )
    
    if (components_str == "Seasonal") {
      e$explanation <- paste0(
        e$explanation,
        " However, since the model is pure Seasonal specification, this is not unexpected it models effects similarly."
      )
    } else {
      e$explanation <- paste0(
        e$explanation,
        " The model might not be capturing ",
        "the series' seasonality", 
        " properly,", 
        c("", " extrapolating an unnecessary trend,")[uses_trend + 1],
        " or it might not be sufficiently robust against anomalies.",
        " It's preferable to use ",
        snaive_description,
        " as forecast."
      )
    }
  } else {
    e$explanation <- paste0(
      "Historically, the model has predicted the series at hand better than ",
      c(
        paste0("both ", runrate_description, ", and "), ""
      )[is.na(e$gof_runrate) + 1],
      snaive_description, "."
    )
    
    if (!isFALSE(e$gof_runrate > 0.25) && e$gof_snaive > 0.25) {
      e$explanation <- paste0(
        e$explanation,
        " It captures historical patterns that simple benchmarks do not."
      )
    }
    
    e$better_than_snaive <- get_different_obs(
      gof_benchmark$share_of_snaive_errors, improvement = TRUE
    )
    e$better_than_runrate <- get_different_obs(
      gof_benchmark$share_of_runrate_errors, improvement = TRUE
    )
  }
  
  return(e)
}

explain_goodness_of_fit <- function(gof) {
  explanation <- ""
  
  if (gof < 0.05) {
    explanation <- "The model exhibits worse predictive performance than a naive benchmark. It's forecast is not reliable."
  } else if (gof > 0.5) {
    explanation <- "The model exhibits better predictive performance than a naive benchmark, indicating that it is able to model signal in the series."
  } else {
    explanation <- "The model exhibits a somewhat better predictive performance than a naive benchmark. Still, there might be a sizeable share of variation in the series that the model can't explain."
  }
  
  return(explanation)
}

get_runrate <- function(y, seasonality, start_period) {
  if (start_period > seasonality) {
    stop("Please provide an integer `start_period` that is less than or equal to `seasonality`.")
  }
  if (start_period < 1) {
    stop("Please provide an integer `start_period` that is at least 1.")
  }
  start_period <- as.integer(start_period)
  
  n_obs <- length(y)
  if (n_obs <= seasonality) return(rep(NA, n_obs))
  
  n_periods <- ceiling(n_obs) / seasonality + 2
  period_idx <- rep(1:seasonality, times = n_periods)
  
  values <- rep(NA, length(period_idx))
  values[start_period:(n_obs + start_period - 1)] <- y
  
  malues <- matrix(values, ncol = n_periods, nrow = seasonality)
  malues_idx <- matrix(period_idx, ncol = n_periods, nrow = seasonality)
  malues_cummean <- apply(malues, 2, cumsum) / malues_idx
  
  malues_cummean_lag <- matrix(NA, nrow = nrow(malues), ncol = ncol(malues))
  malues_cummean_lag[2:seasonality, ] <- malues_cummean[1:(seasonality - 1), ]
  malues_cummean_lag[1, 2:ncol(malues_cummean_lag)] <-
    malues_cummean[seasonality, 1:(ncol(malues_cummean) - 1)]
  
  runrate <- as.numeric(malues_cummean_lag)
  runrate <- runrate[start_period:(n_obs + start_period - 1)]
  
  return(runrate)
}

get_benchmark_gof <- function(object,
                              burn,
                              seasonality,
                              n_obs,
                              start_period = NULL) {
  if (n_obs < 2 * seasonality) {
    return(list(snaive = NA, runrate = NA))
  }
  
  y <- as.numeric(object$original.series)
  
  bsts_errors <- bsts::bsts.prediction.errors(object, burn = burn)$in.sample
  bsts_errors <- colMeans(bsts_errors)
  availability_idx <- c(FALSE, rep(TRUE, length = n_obs - 1))
  
  snaive_errors <- c(rep(NA, seasonality), diff(y, lag = seasonality))
  availability_idx <- availability_idx & !is.na(snaive_errors)
  
  runrate_errors <- rep(NA, length(y))
  if (!is.null(start_period)) {
    runrate <- get_runrate(
      y = y,
      seasonality = seasonality,
      start_period = start_period
    )
    runrate_errors <- runrate - y
    
    availability_idx <- availability_idx & !is.na(runrate_errors)
  }
  
  gof_snaive <- 1 - sum(bsts_errors[availability_idx]^2) / 
    (var(snaive_errors[availability_idx]) * sum(availability_idx))
  
  gof_runrate <- 1 - sum(bsts_errors[availability_idx]^2) / 
    (var(runrate_errors[availability_idx]) * sum(availability_idx))
  
  share_of_snaive_errors <- round(abs(bsts_errors) / abs(snaive_errors), 2)
  share_of_runrate_errors <- round(abs(bsts_errors) / abs(runrate_errors), 2)
  
  return(
    list(
      snaive = gof_snaive,
      runrate = gof_runrate,
      share_of_snaive_errors = share_of_snaive_errors,
      share_of_runrate_errors = share_of_runrate_errors
    )
  )
}

get_different_obs <- function(share_of_errors, improvement = TRUE) {
  
  if (improvement) {
    error_order <- order(share_of_errors, na.last = TRUE, decreasing = FALSE)
    is_relevant <- which(share_of_errors < 1)
  } else {
    error_order <- order(share_of_errors, na.last = TRUE, decreasing = TRUE)
    is_relevant <- which(share_of_errors > 1)
  }
  
  error_order <- error_order[error_order %in% is_relevant]
  
  if (length(error_order) <= 5) {
    return(error_order)
  }
  
  return(error_order[1:5])
}
