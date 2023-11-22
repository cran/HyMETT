# calc_annual_stat_trend
#' Calculate trend in annual statistics
#' 
#' @description Calculate trend in annual statistics
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `year` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param year 'numeric' vector when `data = NULL`, or 'character' string identifying year column 
#'   name when `data` is specified. Year of each value in `value` parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying value column 
#'   name when `data` is specified. Values to calculate trend on.
#' @param ... further arguments to be passed to or from [`EnvStats::kendallTrendTest`].
#' 
#' @return A tibble (see [`tibble::tibble`]) with test statistic, p-value, trend coefficients, and 
#' trend calculations. See **Details**.
#' 
#' @details 
#' This function is a wrapper for [`EnvStats::kendallTrendTest`] with the passed equation 
#' `value ~ year`. The returned values include Mann-Kendall test statistic and p-value, 
#' Theil-Sen slope and intercept values, and trend details (Millard, 2013; Helsel and others, 2020).
#' \describe{
#'   \item{`z_stat`}{Mann-Kendall test statistic, returned directly from 
#'                   [`EnvStats::kendallTrendTest`]}
#'   \item{`p_value`}{`z_stat` p-value, returned directly from 
#'     [`EnvStats::kendallTrendTest`]}
#'   \item{`sen_slope`}{Sen slope in units value per year, returned directly from 
#'                      [`EnvStats::kendallTrendTest`]}
#'   \item{`intercept`}{Sen slope intercept, returned directly from [`EnvStats::kendallTrendTest`]}
#'   \item{`trend_mag`}{Trend magnitude over entire period, in units of `value`, 
#'                      calculated as `sen_slope * (max(year)` \eqn{-} `min(year))`}
#'   \item{`val_beg/end`}{Calculated value at beginning or end of period, calculated as 
#'                        `sen_slope * year + intercept`}
#'   \item{`val_perc_change`}{Percentage change over period, calculated as 
#'                            `(val_end - val_beg) / val_beg * 100`}
#' }
#' 
#' @references 
#' Millard, S.P., 2013, EnvStats: An R Package for Environmental Statistics: New York, New York,
#' Springer, 291 p. \[Also available at https://doi.org/10.1007/978-1-4614-8456-1.\]
#' 
#' Helsel, D.R., Hirsch, R.M., Ryberg, K.R., Archfield, S.A., and Gilroy, E.J., 2020, Statistical 
#' methods in water resources: U.S. Geological Survey Techniques and Methods, book 4, chap. A3, 458 
#' p. \[Also available at https://doi.org/10.3133/tm4a3.\]
#'
#' @export
#'
#' @seealso \code{\link[EnvStats]{kendallTrendTest}}
#' 
#' @keywords annual-statistics
#' @keywords trends
#'
#' @examples 
#' calc_annual_stat_trend(data = example_annual, year = "WY", value = "annual_mean")
#' 
calc_annual_stat_trend <- function(data = NULL,
                                   year,
                                   value,
                                   ...){
  # check inputs
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(year)
    checkmate::assert_string(value)
    checkmate::assert_names(colnames(data), must.include = c(year, value))
    year <- data[[year]]
    value <- data[[value]]
  }
  checkmate::assert_numeric(year)
  checkmate::assert_numeric(value)
  if(length(year) != length(value)){
    stop("'year' and 'value' are unequal lenghts")
  }
  
  beg_Yr <- min(year, na.rm = TRUE)
  end_Yr <- max(year, na.rm = TRUE)
  
  trend <- EnvStats::kendallTrendTest(value ~ year, ci.slope = FALSE, ...)
  
  out <- data.frame(z_stat = trend$statistic,
                    p_value = trend$p.value,
                    sen_slope = trend$estimate["slope"],
                    intercept = trend$estimate["intercept"])
  
  # trend magnitude in units of value - represents total change in stat over the time period tested
  out$trend_mag <- signif(out$sen_slope * (end_Yr - beg_Yr), 3)
  
  out$val_beg <- out$sen_slope * beg_Yr + out$intercept
  out$val_end <- out$sen_slope * end_Yr + out$intercept
  out$val_perc_change <- signif(((out$val_end - out$val_beg) / out$val_beg * 100), 3)
  if(out$sen_slope == 0 && !is.na(out$sen_slope)){
    out$val_perc_change <- 0
  }
  
  return(tibble::as_tibble(out))
}
