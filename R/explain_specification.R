explain_specification <- function(
    specification = c("SeasonalSemilocalLinearTrend",
                      "SeasonalLocalLinearTrend",
                      "SeasonalLocalLevel",
                      "Seasonal",
                      "SemilocalLinearTrend",
                      "LocalLinearTrend",
                      "LocalLevel")[1]
) {
  
  if (is.null(specification)) {
    stop("Please provide a specification different from NULL.")
  }
  
  if (specification == "SeasonalSemilocalLinearTrend") {
    return("The chosen model takes into account seasonality and allows for a long-run trend with short-term fluctuation around the trend.")
  } else if (specification == "SeasonalLocalLinearTrend") {
    return("The chosen model takes into account seasonality and allows for a trend that adjusts over time and is projected into the future.")
  } else if (specification == "SeasonalLocalLevel") {
    return("The chosen model takes into account seasonality and adjust to historical changes in the level of the series.")
  } else if (specification == "Seasonal") {
    return("The chosen model takes into account seasonality, but does not adjust to changes in the level or trend of the series.")
  } else if (specification == "SemilocalLinearTrend") {
    return("The chosen model allows for a long-run trend with short-term fluctuation around said trend. It does not account for seasonality.")
  } else if (specification == "LocalLevel") {
    return("The chosen model adjusts to changes in the level of the series. It does not account for trends or seasonality.")
  } else if (specification == "LocalLinearTrend") {
    return("The chosen model allows for a trend that adjusts over time and is projected into the future. It does not account for seasonality.")
  } else {
    return("The model at hand has an unknown combination of components.")
  }
}
