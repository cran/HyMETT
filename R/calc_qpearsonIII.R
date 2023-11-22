# calc_qpearsonIII
#' Quantile of Pearson Type III distribution
#' 
#' @description Quantile of Pearson Type III distribution
#' 
#' @param p Vector of non-exceedance probabilities, between 0 and 1, to calculate quantiles.
#' @param mean Vector of means of the distribution of the data.
#' @param sd Vector of standard deviation of the distribution of the data.
#' @param skew Vector of skewness of the distribution of the data.
#' 
#' @return Quantiles for the described distribution
#' 
#' @details 
#' [`calc_qpearsonIII`] and [`calc_qlpearsonIII`] are functions to fit a log-Pearson type III 
#' distribution from a given mean, standard deviation, and skew. This source code is replicated, 
#' unchanged, from the `swmrBase` package in order to reduce the dependency on that package.
#' 
#' @export
#'
#' @keywords period-of-record
#'
#' @references
#' Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow 
#' frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey 
#' Scientific Investigation Report 2017–5038, 93 p. 
#' \[Also available at https://doi.org/10.3133/sir20175038.\]
#' 
#' Lorenz, D.L., 2015, smwrBase—An R package for managing hydrologic data, version 1.1.1: U.S. 
#' Geological Survey Open-File Report 2015–1202, 7 p.\cr
#' \[Also available at https://doi.org/10.3133/ofr20151202.\]
#' 
#' @examples 
#' calc_qpearsonIII(0.1)
#' 
calc_qpearsonIII <- function(p, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  Nout <- max(length(p), length(mean), length(sd), length(skew))
  p <- rep(p, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- stats::qnorm(p)
  }
  shape <- 4/skew^2
  rets <- ifelse(skew > 0, (stats::qgamma(p, shape) - shape)/sqrt(shape),
                 (shape - stats::qgamma(1 - p, shape))/sqrt(shape))
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) + 
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets * sd + mean)
}

####################################################################################################
# calc_qlpearsonIII
#' Quantile of Pearson Type III distribution for log-transformed data
#' 
#' @description Quantile of Pearson Type III distribution for log-transformed data
#' 
#' @param p Vector of non-exceedance probabilities, between 0 and 1, to calculate quantiles.
#' @param meanlog Vector of mean of the distribution of the log-transformed data.
#' @param sdlog Vector of standard deviation of the distribution of the log-transformed data.
#' @param skew Vector of skewness of the distribution of the log-transformed data.
#' 
#' @return Quantiles for the described distribution
#' 
#' @details 
#' [`calc_qpearsonIII`] and [`calc_qlpearsonIII`] are functions to fit a log-Pearson type III 
#' distribution from a given mean, standard deviation, and skew. This source code is replicated, 
#' unchanged, from the `swmrBase` package in order to reduce the dependency on that package.
#' 
#' @export
#'
#' @seealso \code{\link{calc_qpearsonIII}}
#' 
#' @keywords period-of-record
#'
#' @references
#' Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow 
#' frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey 
#' Scientific Investigation Report 2017–5038, 93 p. 
#' \[Also available at https://doi.org/10.3133/sir20175038.\]
#' 
#' Lorenz, D.L., 2015, smwrBase—An R package for managing hydrologic data, version 1.1.1: U.S. 
#' Geological Survey Open-File Report 2015–1202, 7 p.\cr
#' \[Also available at https://doi.org/10.3133/ofr20151202.\]
#'  
#' @examples 
#' calc_qlpearsonIII(0.1)
#' 
calc_qlpearsonIII <- function(p, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(calc_qpearsonIII(p, meanlog, sdlog, skew)))
}
