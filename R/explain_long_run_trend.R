explain_long_run_trend <- function(object, burn) {
  
  mcmc_index <- 1:object$niter
  if (burn > 0) mcmc_index <- mcmc_index[-(1:burn)]
  
  is_likely_positive <- mean(object$trend.slope.mean[mcmc_index] > 0) > 0.8
  is_likely_negative <- mean(object$trend.slope.mean[mcmc_index] < 0) > 0.8
  
  is_stable <- mean(object$trend.slope.ar.coefficient[mcmc_index] < 0.25) > 0.8
  
  explanation <- "The model does not predict a distinct long-run trend."
  if (is_likely_positive || is_likely_negative) {
    direction <- c("positive", "negative")[1 + is_likely_negative]
    explanation <- paste0(
      "The model estimates a ",
      direction,
      " long-run trend which determines the long-term forecast."
    )
    
    if (!is_stable) {
      strong <- ""
      if (mean(object$trend.slope.ar.coefficient[mcmc_index] > 0.8) > 0.8) {
        strong <- "strong "
      }
      
      explanation <- paste0(
        explanation, 
        " However, the model also projects ",
        strong,
        "short-term fluctuations around that trend."
      )
    }
  }
  
  return(explanation)
}
