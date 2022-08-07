explain_family <- function(object, anomalies) {
  explanation <- NA_character_
  
  if (object$family == "gaussian" && length(anomalies) > 1) {
    explanation <- "The model is not robust against anomalies. Depending on their size and frequency, anomalies may cause the model to project overly large forecast uncertainty, and to estimate seasonality and trend poorly."
  }
  
  if (object$family == "student") {
    explanation <- "The model is robust against individual anomalous observations. As long as anomalies do not persist over several periods, they shouldn't impact the model's estimates of trend and seasonality."
  }
  
  return(explanation)
}
