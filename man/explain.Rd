% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain.R
\name{explain}
\alias{explain}
\title{Explain a Fitted Structural Time Series Model}
\usage{
explain(
  object,
  burn = bsts::SuggestBurn(0.1, object),
  seasonality = NULL,
  start_period = NULL
)
}
\arguments{
\item{object}{The BSTS model fitted via \code{bsts::bsts()}}

\item{burn}{Integer; defines the number of MCMC iterations that will be
discarded as burn-in. No samples will be discarded for burn-in if
\code{burn <= 0}. See also \code{bsts::SuggestBurn()}.}

\item{seasonality}{Integer defining the period length of the time series,
can be used even if model is not seasonal.}

\item{start_period}{Integer indicating the period in which the series starts,
for example, if the series has yearly seasonality and starts in March,
then \code{start_period = 3} could be provided. If \code{start_period} is \code{NULL},
the runrate benchmark cannot be computed.}
}
\value{
A list of strings that are explanations of components of the object
}
\description{
Explain a Fitted Structural Time Series Model
}
\examples{
library(bsts)

y <- log(AirPassengers)
ss <- bsts::AddSemilocalLinearTrend(list(), y)
ss <- bsts::AddSeasonal(ss, y, nseasons = 12)
model <- bsts::bsts(y, state.specification = ss, niter = 5000)

explanation <- explain(
  object = model,
  burn = 1000,
  seasonality = 12,
  start_period = 1
)
print(explanation)

names(explanation)
explanation$anomalies
explanation$benchmark

}
