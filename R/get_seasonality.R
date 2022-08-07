assert_seasonality <- function(seasonality) {
  seasonality <- as.integer(seasonality)
  if (is.na(seasonality)) {
    stop("The provided `seasonality` could not be cast as integer. Please provide an integer seasonality.")
  }
  if (seasonality < 1L) {
    stop("Please provide an integer `seasonality` that is at least 1.")
  }
  
  return(seasonality)
}

get_seasonality <- function(object) {
  seasonality <- 1
  
  components <- vapply(
    X = object$state.specification, 
    FUN.VALUE = NA_character_,
    FUN = function(x) attr(x, which = "class")[1]
  )
  
  if ("Seasonal" %in% components) {
    seasonality <- 
      object$state.specification[[which(components == "Seasonal")]]$nseasons
  }
  
  return(seasonality)
}
