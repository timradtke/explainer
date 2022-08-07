explain_variance_reduction <- function(object, burn) {
  rsquare <- bsts::summary.bsts(object = object, burn = burn)$rsquare
  
  description <- "a good share"
  if (rsquare < 0.25) description <- "not too much"
  if (rsquare > 0.75) description <- "most"
  
  explanation <- sprintf(
    "The model explains %s of the observed variation in the series at hand.",
    description
  )
  
  if (rsquare > 0.75) {
    explanation <- paste0(
      explanation,
      " This indicates that the series contains signals such as trend or seasonality, but it doesn't guarantee that the model's forecast projects them appropriately into the future."
    )
  } else if (rsquare < 0.25) {
    explanation <- paste0(
      explanation,
      " The series might have a small signal-to-noise ratio, or the chosen model not appropriate for the series' patterns."
    )
  }
  
  return(
    list(
      explanation = explanation,
      rsquare = rsquare
    )
  )
}
