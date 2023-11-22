# calc_logistic_regression
#' Calculate logistic regression in annual statistics with zero values
#' 
#' @description Calculate logistic regression (Everitt and Hothorn, 2009) in annual statistics with 
#' zero values. A model fit to compute the probability of a zero flow annual statistic.
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `year` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param year 'numeric' vector when `data = NULL`, or 'character' string identifying year column 
#'   name when `data` is specified. Year of each value in `value` parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying value column 
#'   name when `data` is specified. Values to calculate logistic regression on.
#' @param ... further arguments to be passed to or from [`stats::glm`].
#' 
#' @return A tibble (see [`tibble::tibble`]) with logistic regression p-value, standard error of 
#' slope, odds ratio, beginning and ending probability, and probability change. See **Details**.
#' 
#' @details 
#' This function is a wrapper for `stats::glm(y ~ year, family = stats::binomial(link="logit")`
#' with `y = 1` when `value = 0` (for example a zero flow annual statistic) and `y = 0` otherwise.
#' The returned values include
#' \describe{
#'   \item{`p_value`}{Probability value of the explanatory (`year`) variable in the logistic model}
#'   \item{`stdErr_slope`}{Standard error of the regression slope (log odds per year)}
#'   \item{`odds_ratio`}{Exponential of the explanatory coefficient (year coefficient)}
#'   \item{`prob_beg/end`}{Logistic regression predicted (fitted) values at the beginning and ending year.}
#'   \item{`prob_change`}{Change in probability from beginning to end.}
#' }
#' Example, an odds ratio of 1.05 represents the odds of a zero-flow year (versus non-zero) 
#' increase by a factor of 1.05 (or 5 percent).
#' 
#' @references 
#' Everitt, B. S. and Hothorn T., 2009, A Handbook of Statistical Analyses Using R, 2nd Ed. 
#' Boca Raton, Florida, Chapman and Hall/CRC, 376p.
#' 
#' @export
#'
#' @seealso \code{\link[stats]{glm}}
#' 
#' @keywords annual-statistics
#' @keywords trends
#' 
#' @examples 
#' calc_logistic_regression(data = example_annual, year = "WY", value = "annual_mean")
#' 
calc_logistic_regression <- function(data = NULL,
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
  
  # make the dependent variable (1/0) boolean as zero or not zero. 
  y <- rep(0, length(value))
  y[value == 0] <- 1
  
  # derive the logistic model
  logistic_model <- stats::glm(y ~ year, family = stats::binomial(link="logit"), ...)
  
  logistic_model_sum <- summary(logistic_model)# do the summary to extract p-value and std error
  zero_trend_pval <- logistic_model_sum$coefficients[2,4]# p-value of the year term (explanatory variable)
  zero_trend_se <- logistic_model_sum$coefficients[2,2]# standard error of the slope (log odds per year)
  
  # odds ratio (exponentiate the coefficients) see: https://stats.idre.ucla.edu/r/dae/logit-regression/
  zero_trend_or <- exp(stats::coef(logistic_model)[2])# so for example if x$yr OR is 1.05, that means the odds of a zero-flow year (versus non-zero) increase by a factor of 1.05 (or 5%).  
  
  # logistic_model$fitted.values are the predicted probabilities year to year of a zero flow year
  zero_trend_prob_beginning <- logistic_model$fitted.values[[1]]
  zero_trend_prob_end <- logistic_model$fitted.values[[length(logistic_model$fitted.values)]]
  zero_trend_prob_change <- zero_trend_prob_end - zero_trend_prob_beginning
  
  # output
  out <- tibble::tibble(p_value = zero_trend_pval,
                        stdErr_slope = zero_trend_se,
                        odds_ratio = zero_trend_or,
                        prob_beg = zero_trend_prob_beginning,
                        prob_end = zero_trend_prob_end,
                        prob_change = zero_trend_prob_change)
  return(out)
}
