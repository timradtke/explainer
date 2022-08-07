#' Explain a Fitted Structural Time Series Model
#' 
#' @param object The BSTS model fitted via `bsts::bsts()`
#' @param burn Integer; defines the number of MCMC iterations that will be
#'     discarded as burn-in. No samples will be discarded for burn-in if 
#'     `burn <= 0`. See also `bsts::SuggestBurn()`.
#' @param seasonality Integer defining the period length of the time series,
#'     can be used even if model is not seasonal.
#' @param start_period Integer indicating the period in which the series starts,
#'     for example, if the series has yearly seasonality and starts in March,
#'     then `start_period = 3` could be provided. If `start_period` is `NULL`,
#'     the runrate benchmark cannot be computed.
#' 
#' @return A list of strings that are explanations of components of the object
#' 
#' @export
#' @examples
#' library(bsts)
#' 
#' y <- log(AirPassengers)
#' ss <- bsts::AddSemilocalLinearTrend(list(), y)
#' ss <- bsts::AddSeasonal(ss, y, nseasons = 12)
#' model <- bsts::bsts(y, state.specification = ss, niter = 5000)
#' 
#' explanation <- explain(
#'   object = model,
#'   burn = 1000,
#'   seasonality = 12,
#'   start_period = 1
#' )
#' print(explanation)
#' 
#' names(explanation)
#' explanation$anomalies
#' explanation$benchmark
#' 
explain <- function(object, 
                    burn = bsts::SuggestBurn(.1, object),
                    seasonality = NULL,
                    start_period = NULL) {
  
  if (!inherits(object, "bsts")) {
    stop("The object must be of class `bsts`.")
  }
  
  if (object$family %in% c("logit", "poisson")) {
    stop("Residuals are not supported for Poisson or logit models.")
  }
  
  if (isTRUE(object$has.regression)) {
    stop("Model specifications with regression are currently not supported.")
  }
  
  if (is.null(burn)) {
    stop("Please provide an integer `burn` value.")
  }
  
  burn <- as.integer(burn)
  if (is.na(burn)) {
    stop("The provided `burn` could not be cast as integer. Please provide an integer-values `burn`.")
  }
  if (burn >= object$niter) {
    stop("The provided `burn` value would not leave any samples. Please provide a smaller `burn` value or use more iterations when fitting the model.")
  }
  
  if (!is.null(seasonality)) {
    seasonality <- assert_seasonality(seasonality)
  }
  
  if (!is.null(start_period)) {
    start_period <- as.integer(start_period)
    if (is.na(start_period)) {
      stop("The provided `start_period` could not be cast as integer. Please provide an integer-values `start_period`.")
    }
  }
  
  components <- sort(
    vapply(
      X = object$state.specification, 
      FUN.VALUE = NA_character_,
      FUN = function(x) attr(x, which = "class")[1]
    )
  )
  
  if (length(components) > 2) {
    stop("Model specifications with more than 2 components are currently not supported.")
  }
  
  known_components_seasonal <- "Seasonal"
  known_components_trend <- c("LocalLevel", "LocalLinearTrend", 
                              "SemilocalLinearTrend")
  
  components_str <- paste(components, collapse = "")
  
  if (grepl(pattern = "AutoAr", x = components_str)) {
    stop("The model specification uses an `AutoAr` component that currently can't be explained.")
  }
  if (!all(components %in% c(known_components_trend, 
                             known_components_seasonal))) {
    stop("The model specification uses a component that currently can't be explained.")
  }
  
  n_obs <- length(object$original.series)
  
  if (is.null(seasonality)) {
    seasonality <- get_seasonality(object = object)
  }
  
  gof <- bsts::summary.bsts(object = object, burn = burn)$relative.gof
  
  explanation <- list(
    components = NA_character_,
    model = NA_character_,
    length = NA_character_,
    variance_reduction = NA_character_,
    goodness_of_fit = NA_character_,
    benchmark = NA_character_,
    current_level = NA_character_,
    strengths = NA_character_,
    long_run_trend = NA_character_,
    upcoming_season = NA_character_,
    innovation = NA_character_,
    anomalies = NA_character_,
    family = NA_character_
  )
  
  class(explanation) <- "explanation"
  
  explanation$components <- components_str
  
  explanation$model <- explain_specification(components_str)
  
  explanation$length <- explain_length(n_obs = n_obs, seasonality = seasonality)
  
  explanation$variance_reduction <- explain_variance_reduction(
    object = object, burn = burn
  )
  
  explanation$goodness_of_fit <- explain_goodness_of_fit(gof)
  
  if (gof < 0.05 || n_obs < ceiling(seasonality) || n_obs < 10) {
    return(explanation)
  }
  
  gof_benchmark <- get_benchmark_gof(
    object = object,
    burn = burn,
    seasonality = seasonality,
    n_obs = n_obs,
    start_period = start_period
  )
  
  explanation$benchmark <- explain_benchmark(
    gof_benchmark = gof_benchmark,
    n_obs = n_obs,
    seasonality = seasonality,
    components_str = components_str,
    uses_trend = grepl(pattern = "Trend", x = components_str),
    uses_seasonality = grepl(pattern = "Season", x = components_str)
  )
  
  if (!is.na(explanation$benchmark$explanation)) {
    explanation$goodness_of_fit <- NA_character_
  }
  
  explanation$current_level <- explain_current_level(
    object = object,
    burn = burn,
    n_obs = n_obs,
    seasonality = seasonality
  )
  
  if (grepl(pattern = "Seasonal", x = components_str) &&
      grepl(pattern = "Trend", x = components_str)) {
    explanation$strengths <- explain_strengths(
      object = object,
      burn = burn,
      n_obs = n_obs,
      seasonality = seasonality
    )
  }
  
  if (grepl(pattern = "SemilocalLinearTrend", x = components_str)) {
    explanation$long_run_trend <- explain_long_run_trend(
      object = object,
      burn = burn
    )
  } else {
    explanation$long_run_trend <- 
      "The model does not project a long-run trend different from the current trend."
  }
  
  if (grepl(pattern = "Seasonal", x = components_str)) {
    explanation$upcoming_season <- explain_upcoming_season(
      object = object,
      burn = burn,
      n_obs = n_obs,
      seasonality = seasonality
    )
  } else {
    explanation$upcoming_season <- NA_character_
  }
  
  explanation$innovation <- explain_innovation(
    object = object,
    burn = burn,
    sigma_obs = get_sigma_obs_name(components = components_str),
    sigma_level = get_sigma_obs_name(components = components_str),
    sigma_trend = get_sigma_trend_name(components = components_str),
    sigma_season = get_sigma_season_name(
      components = components_str,
      seasonality = seasonality
    )
  )
  
  explanation$anomalies <- explain_anomalies(
    object = object,
    burn = burn,
    n_obs = n_obs,
    seasonality = seasonality,
    threshold = 3
  )
  
  explanation$family <- explain_family(
    object = object,
    anomalies = explanation$anomalies$anomalies
  )
  
  return(explanation)
}
