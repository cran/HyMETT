# POR_calc_lp3_quantile
#' Calculate quantile from fitted log-Pearson type III distribution
#' 
#' @description Calculate the specified flow quantile from a fitted log-Pearson type III 
#' distribution from a time series of n-day low flows.
#' 
#' @param annual_min 'numeric' vector. Vector of minimum annual n-day mean flows.
#' @param p 'numeric' value of exceedance probabilities. Quantile of fitted distribution that is 
#'   returned (`p=0.1` for 10-year return period, `p=0.5` for 2-year return period)
#'
#' @return Specified quantile from the fitted log-Pearson type 3 distribution.
#' 
#' @details 
#' `POR_calc_lp3_quantile` fits an log-Pearson type III distribution to a series of annual n-day 
#' flows and returns the quantile of a user-specified probability using [`calc_qlpearsonIII`]. This 
#' represents a theoretical return period for than n-day flow. 
#' 
#' @export
#' 
#' @seealso \code{\link{calc_qlpearsonIII}}
#' 
#' @keywords period-of-record
#' @keywords annual-statistics
#'
#' @references 
#' Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow 
#' frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey 
#' Scientific Investigation Report 2017–5038, 93 p. 
#' \[Also available at https://doi.org/10.3133/sir20175038.\]
#' 
#' @examples 
#' POR_calc_lp3_quantile(annual_min = example_annual$low_q1, p = 0.1)
#' 
# Dev details: 
# This function was modified by Sara Levin from calcFlowStats.7QT.R by Tom Over
POR_calc_lp3_quantile <- function(annual_min, p){
  # check assertions
  checkmate::assert_numeric(annual_min)
  checkmate::assert_number(p, lower = 0, upper = 1)
  
  
  annmin_pos <- annual_min[which(annual_min > 0)]
  npos <- length(annmin_pos)
  ppos <- npos / length(annual_min) # Fraction of positive values
  # Fraction of non-positive values (usually zeroes)
  p0 <- (length(annual_min) - npos) / length(annual_min) 
  
  if(npos > 2){#Need at least 3 observations to compute skewness
    log_annmin_pos <- log(annmin_pos)
    log_mean <- mean(log_annmin_pos)
    log_stdev <- stats::sd(log_annmin_pos)
    
    if(log_stdev == 0){
      log_skew = 0
    } else{
      #Skewness following eq.4 of Bull. 17B (same as eq.18.1.18 in Handbook of Hydrology p.18.4)
      log_skew <- npos / ((npos - 1) * (npos - 2) * log_stdev^3) * sum((log_annmin_pos - log_mean)^3)
    }
    
    ## Conditional probability adjustment per SW Toolbox manual, tm4-a11 (Kiang_etal,2018)
    ## See p. 4, last equation on RHS, re-solved for P(Q<Q_T|Q>0)
    ## pth percentile of seven-day average annual minimum (from LPIII fit)
    if (p0 < p){
      lp3_quant <- calc_qlpearsonIII((p - p0)/ppos, log_mean, log_stdev, log_skew)
    } else {
      lp3_quant <- 0
    }
  } else{
    lp3_quant = NA_real_
  }
  
  return(lp3_quant)
}
