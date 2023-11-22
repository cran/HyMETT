# POR_distribution_metrics
#' Calculates various metrics that describe the distribution of a time series of streamflow
#' 
#' @description Calculates various metrics that describe the distribution of a time series 
#' of streamflow, which can be of any time step.
#' 
#' @param value 'numeric' vector of values (assumed to be streamflow) at any time step.
#' @param quantile_type 'numeric' value. The distribution type used in the [`stats::quantile`] 
#'   function. Default is `8` (median-unbiased regardless of distribution). Other 
#'   types common in hydrology are `6` (Weibull) or `9` (unbiased for normal distributions).
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If 
#'   `NA` values are present and `na.rm = FALSE`, then function will return `NA`s. Default is `TRUE`.
#'
#' @return A data.frame with FDC quantiles, and distribution metrics. See **Details**.
#' This function calculates various metrics that describe the distribution of a time series 
#' of streamflow, which can be of any time step. 
#' 
#' @details 
#' Metrics computed include:
#' \describe{
#'   \item{`p_`*n*}{Flow-duration curve (FDC) percentile where *n* = 1, 5, 10, 25, 50, 75, 90, 95, 
#'   and 99}
#'   \item{`POR_mean`}{Period of record mean}
#'   \item{`POR_sd`}{Period of record standard deviation}
#'   \item{`POR_cv`}{Period of record coefficient of variation}
#'   \item{`POR_min`}{Period of record minimum}
#'   \item{`POR_max`}{Period of record maximum}
#'   \item{`LCV`}{L-moment coefficient of variation}
#'   \item{`Lskew`}{L-moment skewness}
#'   \item{`Lkurtosis`}{L-moment kurtosis}
#' }
#' 
#' @export
#' 
#' @keywords period-of-record
#'
#' @seealso \code{\link[lmomco]{lmoms}}, \code{\link[stats]{quantile}} 
#'
#' @references 
#' Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H., and Kiang, J.E., 2014, 
#' A comparison of methods to predict historical daily streamflow time series in the southeastern 
#' United States: U.S. Geological Survey Scientific Investigations Report 2014–5231, 34 p. 
#' \[Also available at https://doi.org/10.3133/sir20145231.\]
#'
#' Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow 
#' frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey 
#' Scientific Investigation Report 2017–5038, 93 p. 
#' \[Also available at https://doi.org/10.3133/sir20175038.\]
#'
#' Asquith, W.H., 2021, lmomco---L-moments, censored L-moments, trimmed L-moments,\cr 
#' L-comoments, and many distributions. R package version 2.3.7, Texas Tech University, 
#' Lubbock, Texas.
#'
#' @examples 
#' POR_distribution_metrics(value = example_obs$streamflow_cfs)
#'  
# Dev details:
# Based on analysis from SIR 2014-5231 (circa 16 June 2014)
# FDSS calculations based on code from Stacey Archfield
# Function redeveloped by William Farmer, 09 June 2015
# Revised by Tom Over and Jason Griffin, 2018-2019
# Revised by Sara Levin 2020
POR_distribution_metrics<-function(value,
                                   quantile_type = 8,
                                   na.rm = TRUE){
  # check assertions
  checkmate::assert_vector(value)
  checkmate::assert_choice(quantile_type, choices = c(1,2,3,4,5,6,7,8,9))
  checkmate::assert_logical(na.rm)
  
  #compute quantiles, mean, standard deviation and coefficient of variation
  quants <- tryCatch({
    as.data.frame(t(stats::quantile(value,probs = c(0.01 ,0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99),
                                            na.rm = na.rm, type = quantile_type)))
  },
  warning = function(w){
    message(paste(w))
    return(as.data.frame(t(rep(NA_real_, 9))))
  },
  error = function(e){
    message(paste(e))
    return(as.data.frame(t(rep(NA_real_, 9))))
  })
    
  names(quants) <- c("p1", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
  POR_mean <- mean(value, na.rm = na.rm)
  POR_sd <- stats::sd(value, na.rm = na.rm)
  POR_cv <- POR_sd / POR_mean
  POR_min <- min(value, na.rm = na.rm)
  POR_max <- max(value, na.rm = na.rm)
  
  #compute sample L-moments 
  lmoments <- tryCatch({lmomco::lmoms(value)},
                     warning = function(w){
                       message(paste(w))
                       return(NA_real_)
                     },
                     error = function(e){
                       message(paste(e))
                       return(NA_real_)
                     })
  if(!is.na(lmoments[1])){
    LCV <- lmoments$ratios[2]
    Lskew <- lmoments$ratios[3]
    Lkurtosis <- lmoments$ratios[4]
  } else{
    LCV <- NA_real_
    Lskew <- NA_real_
    Lkurtosis <- NA_real_
  }
  
  #combining all metrics
  values <- t(data.frame(cbind(quants, POR_mean, POR_sd, POR_cv, POR_min, POR_max, LCV, Lskew, Lkurtosis)))
  metrics <- row.names(values)
  df <- data.frame(metric = metrics,
                   value = values,
                   row.names = NULL)
  return(df)
}
