explain_length <- function(n_obs, seasonality) {
  explanation <- NA_character_
  
  if (seasonality == 1) {
    if (n_obs < 10) {
      explanation <- sprintf(
        "The provided series is short with %s observations. Predictions may not represent possible future paths well.",
        n_obs
      )
    }
  } else {
    if (n_obs < ceiling(seasonality / 2)) {
      explanation <- sprintf(
        "The provided series is short (%s observations) and does not provide much information from which to estimate trend and seasonality.",
        n_obs
      )
    } else if (n_obs < 2 * seasonality + 2) {
      explanation <- sprintf(
        "While the series has %s observations, this is less than two seasonal periods and might not suffice to estimate trend and seasonality reliably.",
        n_obs
      )
    } else if (n_obs < 4 * seasonality) {
      explanation <- sprintf(
        "The provided series is somewhat short with %s observations given its seasonal period length of %s.",
        n_obs, seasonality
      )
    }
  }
  
  return(explanation)
}
