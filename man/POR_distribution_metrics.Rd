% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POR_distribution_metrics.R
\name{POR_distribution_metrics}
\alias{POR_distribution_metrics}
\title{Calculates various metrics that describe the distribution of a time series of streamflow}
\usage{
POR_distribution_metrics(value, quantile_type = 8, na.rm = TRUE)
}
\arguments{
\item{value}{'numeric' vector of values (assumed to be streamflow) at any time step.}

\item{quantile_type}{'numeric' value. The distribution type used in the \code{\link[stats:quantile]{stats::quantile}}
function. Default is \code{8} (median-unbiased regardless of distribution). Other
types common in hydrology are \code{6} (Weibull) or \code{9} (unbiased for normal distributions).}

\item{na.rm}{'boolean' \code{TRUE} or \code{FALSE}. Should \code{NA} values be removed before computing. If
\code{NA} values are present and \code{na.rm = FALSE}, then function will return \code{NA}s. Default is \code{TRUE}.}
}
\value{
A data.frame with FDC quantiles, and distribution metrics. See \strong{Details}.
This function calculates various metrics that describe the distribution of a time series
of streamflow, which can be of any time step.
}
\description{
Calculates various metrics that describe the distribution of a time series
of streamflow, which can be of any time step.
}
\details{
Metrics computed include:
\describe{
\item{\code{p_}\emph{n}}{Flow-duration curve (FDC) percentile where \emph{n} = 1, 5, 10, 25, 50, 75, 90, 95,
and 99}
\item{\code{POR_mean}}{Period of record mean}
\item{\code{POR_sd}}{Period of record standard deviation}
\item{\code{POR_cv}}{Period of record coefficient of variation}
\item{\code{POR_min}}{Period of record minimum}
\item{\code{POR_max}}{Period of record maximum}
\item{\code{LCV}}{L-moment coefficient of variation}
\item{\code{Lskew}}{L-moment skewness}
\item{\code{Lkurtosis}}{L-moment kurtosis}
}
}
\examples{
POR_distribution_metrics(value = example_obs$streamflow_cfs)
 
}
\references{
Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H., and Kiang, J.E., 2014,
A comparison of methods to predict historical daily streamflow time series in the southeastern
United States: U.S. Geological Survey Scientific Investigations Report 2014–5231, 34 p.
[Also available at https://doi.org/10.3133/sir20145231.]

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow
frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey
Scientific Investigation Report 2017–5038, 93 p.
[Also available at https://doi.org/10.3133/sir20175038.]

Asquith, W.H., 2021, lmomco---L-moments, censored L-moments, trimmed L-moments,\cr
L-comoments, and many distributions. R package version 2.3.7, Texas Tech University,
Lubbock, Texas.
}
\seealso{
\code{\link[lmomco]{lmoms}}, \code{\link[stats]{quantile}}
}
\keyword{period-of-record}
