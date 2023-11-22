# POR_apply_annual_lowflow_stats
#' Calculate 10-year and 2-year return periods of a streamflow time series
#' 
#' @description Calculates 10-year and 2-year return periods of a streamflow time series from 
#' annual n-day low streamflow values and returns a data.frame in the format of other 
#' period-of-record (POR) metrics.
#' 
#' @param annual_min 'numeric' vector or data.frame. Vector or data.frame with columns of annual 
#'   n-day minimum streamflows.
#'
#' @return data.frame with 10-year and 2-year return period of n-day streamflows.
#' 
#' @details 
#' `POR_apply_POR_lowflow_metrics` is a helper function that applies the [`POR_calc_lp3_quantile`] 
#' function to the data.frame of n-day moving averages, which can be computed during pre-processing 
#' step using [`preproc_precondition_data`] and [`calc_annual_flow_stats`], or [`preproc_main`] for 
#' both observed and modeled data. This function returns a data.frame with the 10-year and 2-year 
#' return period streamflows for each n-day low streamflow in the input data.frame.
#' 
#' @export
#' 
#' @keywords period-of-record
#' @keywords annual-statistics
#'
#'@seealso \code{\link{POR_calc_lp3_quantile}}, \code{\link{preproc_precondition_data}}, 
#' \code{\link{calc_annual_flow_stats}}, \cr
#' \code{\link{preproc_main}}
#'
#' @examples 
#' POR_apply_annual_lowflow_stats(annual_min = example_annual[ , c("low_q1", "low_q30")])
#' 
POR_apply_annual_lowflow_stats <- function(annual_min) {
  # check assertions
  checkmate::assert_multi_class(annual_min, classes = c("numeric", "data.frame"))
  
  annual_min <- as.data.frame(annual_min)
  ##compute lp3 quantiles for 10-yr  return periods for all annual_min time series 
  lp3_10yr_low <- apply(annual_min, MARGIN = 2, FUN = POR_calc_lp3_quantile, p = 0.1)
  df <- data.frame(metric = names(lp3_10yr_low),
                   return_period = "10-yr",
                   value = lp3_10yr_low,
                   row.names = NULL)
  
  ##compute lp3 quantiles for 2-yr  return periods for all annual_min time series 
  lp3_2yr_low <- apply(annual_min, MARGIN = 2, FUN = POR_calc_lp3_quantile, p = 0.5)
  df <- rbind(df, data.frame(metric = names(lp3_2yr_low),
                             return_period = "2-yr",
                             value = lp3_2yr_low,
                             row.names = NULL))
  
  return(df)
}
