# POR_apply_annual_hiflow_stats
#' Calculate the 50th and 90th percentiles of a streamflow time series
#' 
#' @description
#' This function computes the 50th and 90th percentiles of a streamflow time series from annual 
#' n-day high flow values and returns a data.frame in the format of other period-of-record (POR) 
#' metrics.
#' 
#' @param annual_max 'numeric' vector or data.frame. Vector or data.frame with columns of annual 
#'   n-day maximum streamflows.
#' @param quantile_type 'numeric' value. The distribution type used in the [`stats::quantile`] 
#'   function. Default is `8` (median-unbiased regardless of distribution). Other 
#'   types common in hydrology are `6` (Weibull) or `9` (unbiased for normal distributions).
#'
#' @return Data.frame of 0.5 and 0.9 non-exceedance probabilities (50th and 90th percentiles), 
#'   with metric names if `annual_max` is a data.frame with columns named by metric. 
#'
#' @details 
#' annual maximum of n-day moving averages can be computed during pre-processing step using \cr
#' [`preproc_precondition_data`] and [`calc_annual_flow_stats`], or [`preproc_main`] for both 
#' observed and modeled data. 
#'
#' @export
#' 
#' @keywords period-of-record
#' @keywords annual-statistics
#' 
#' @seealso \code{\link[stats]{quantile}}, \code{\link{preproc_precondition_data}}, 
#' \code{\link{calc_annual_flow_stats}}, \code{\link{preproc_main}}
#' 
#' @examples 
#' POR_apply_annual_hiflow_stats(annual_max = example_annual[ , c("high_q1", "high_q30")])
#' 
POR_apply_annual_hiflow_stats <- function(annual_max,
                                          quantile_type = 8){
  # check assertions
  checkmate::assert_multi_class(annual_max, classes = c("numeric", "data.frame"))
  checkmate::assert_choice(quantile_type, choices = c(1,2,3,4,5,6,7,8,9))
  
  names <- names(annual_max)
  if (is.null(names)) {
    names <- NA_character_
  }
  value <- data.frame(value = apply(as.data.frame(annual_max), MARGIN = 2, FUN = stats::quantile, probs = c(0.5), 
                                    na.rm = TRUE, type = quantile_type), row.names = NULL)
  df_50 <- data.frame(metric = names, 
                      percentile = 0.5, 
                      value = value)
  
  value <- data.frame(value = apply(as.data.frame(annual_max), MARGIN = 2, FUN = stats::quantile, probs = c(0.9),
                                    na.rm = TRUE, type = quantile_type), row.names = NULL)
  df_90 <- data.frame(metric = names,
                      percentile = 0.9,
                      value = value)

  df <- rbind(df_50, df_90, row.names = NULL)
  
  return(df)
}
