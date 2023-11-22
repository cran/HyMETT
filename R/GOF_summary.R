# GOF_summary
#' Calculate Goodness-of-fit metrics and output into table 
#' 
#' @description Calculate Goodness-of-fit (GOF) metrics for correlation, Kling--Gupta efficiency,
#' mean absolute error, mean error, Nash--Sutcliffe efficiency, percent bias, 
#' root-mean-square error, normalized root-mean-square error, and volumetric efficiency, and output 
#' into a table.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param metrics 'character' vector. Which GOF metrics should be computed and output. Default is 
#'   `c("cor", "kge", "mae", "me", "nse", "pb", "rmse", "nrmse", "ve")`.\cr
#'   `"cor"`. Correlation tests computed from [`GOF_correlation_tests`].\cr
#'   `"kge"`. Kling--Gupta efficiency computed from [`GOF_kling_gupta_efficiency`].\cr
#'   `"mae"`. Mean absolute error computed from [`GOF_mean_absolute_error`].\cr
#'   `"me"`. Mean error computed from [`GOF_mean_error`].\cr
#'   `"nse"`. Nash--Sutcliffe efficiency computed from\cr
#'            [`GOF_nash_sutcliffe_efficiency`] with option for modified NSE specified by 
#'            parameter `nse_j`.\cr
#'   `"pb"`. Percent bias computed from [`GOF_percent_bias`].\cr
#'   `"rmse"`. Root-mean-square error computed from [`GOF_rmse`].\cr
#'   `"nrmse"`. Normalized root-mean-square error computed from [`GOF_rmse`] and
#'           "normalize" option specified in parameter `rmse_normalize`.\cr
#'   `"ve"`. Volumetric efficiency computed from [`GOF_volumetric_efficiency`].\cr
#' @param censor_threshold 'numeric' value. Threshold to censor values on utilizing 
#'   [`censor_values`] function. Default is `NULL`, no censoring. If level specified, must also 
#'   specify\cr`censor_symbol`.
#' @param censor_symbol 'character' string. Inequality symbol to censor values based on 
#'   `censor_threshold` utilizing [`censor_values`] function. Accepted values are\cr
#'     `"gt"` (greater than),\cr
#'     `"gte"` (greater than or equal to),\cr
#'     `"lt"` (less than),\cr 
#'     or `"lte"` (less than or equal to).\cr
#'   Default is `NULL`, no censoring. If symbol specified, must also specify `censor_value`. 
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#' @param kge_modified 'boolean' `TRUE` or `FALSE`. Should the KGE calculation use the original 
#'   variability ratio in the standard deviations (`kge_modified  = FALSE`) or the modified 
#'   variability ratio in the coefficient of variations (`kge_modified = TRUE`). Default is `FALSE`.
#' @param nse_j 'numeric' value. Exponent value for modified NSE (mNSE) equation, utilized if 
#'   `"nse"` option is in parameter `metrics`. Default value is `nse_j = 2`, which is traditional 
#'   NSE equation.
#' @param rmse_normalize 'character' value. Normalize option for NRMSE, utilized if "nrmse" option 
#'   is in paramter `metrics`. Default is `"mean"`. Options are\cr
#'     `'mean'`. RMSE is normalized by the mean of `obs`.\cr
#'     `'range'`. RMSE is normalized by the range `(max - min)` of `obs`.\cr
#'     `'stdev'`. RMSE is normalized by the standard deviation of `obs`.\cr
#'     `'iqr-#'`. RMSE is normalized by the inter-quartile range of `obs`, with distribution type 
#'     (see [`stats::quantile`] function) indicated by integer number (for example `"iqr-8"`). 
#'     If no type specified, default type is `iqr-7`, the quantile function default.
#' @param ... Further arguments to be passed to or from [`stats::cor.test`] if `"cor"` is in 
#'   `metrics`.
#'
#' @return A tibble (see [`tibble::tibble`]) with GOF metrics
#'
#' @export
#' 
#' @keywords goodness-of-fit
#' 
#' @details 
#' See [`GOF_correlation_tests`], [`GOF_kling_gupta_efficiency`],\cr
#' [`GOF_mean_absolute_error`], [`GOF_mean_error`], \cr
#' [`GOF_nash_sutcliffe_efficiency`], [`GOF_percent_bias`], [`GOF_rmse`],\cr
#' and [`GOF_volumetric_efficiency`].
#'
#' @seealso \code{\link{censor_values}}, \code{\link{GOF_correlation_tests}}, 
#' \code{\link{GOF_kling_gupta_efficiency}},
#' \code{\link{GOF_mean_absolute_error}}, \code{\link{GOF_mean_error}}, \cr
#' \code{\link{GOF_nash_sutcliffe_efficiency}}, \code{\link{GOF_percent_bias}}, 
#' \code{\link{GOF_rmse}}, \cr
#' \code{\link{GOF_volumetric_efficiency}}
#'
#' @examples
#' GOF_summary(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' 
GOF_summary <- function(mod,
                        obs,
                        metrics = c("cor", "kge", "mae", "me", "nse", "pb", "rmse", "nrmse", "ve"),
                        censor_threshold = NULL,
                        censor_symbol = NULL,
                        na.rm = TRUE,
                        kge_modified = FALSE,
                        nse_j = 2,
                        rmse_normalize = c('mean','range','stdev','iqr', 'iqr-1','iqr-2','iqr-3',
                                           'iqr-4','iqr-5','iqr-6','iqr-7','iqr-8','iqr-9', NULL),
                        ...){
  
  # check assertions and lengths and censoring info
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_subset(
    metrics, 
    choices = c("cor", "kge", "mae", "me", "nse", "pb", "rmse", "nrmse", "ve"))
  metrics <- match.arg(metrics, several.ok = TRUE)
  checkmate::assert_number(censor_threshold, null.ok = TRUE)
  checkmate::assert_choice(censor_symbol, choices = c("gt", "gte", "lt", "lte"), null.ok = TRUE)
  checkmate::assert_logical(na.rm)
  checkmate::assert_logical(kge_modified)
  checkmate::assert_numeric(nse_j)
  rmse_normalize <- if(!is.null(rmse_normalize)) {
    rmse_normalize <- match.arg(rmse_normalize)
  }
  checkmate::assert_choice(rmse_normalize, choices = c(NULL,'mean','range','stdev',
                                                       'iqr',
                                                       'iqr-1','iqr-2','iqr-3','iqr-4',
                                                       'iqr-5','iqr-6','iqr-7','iqr-8','iqr-9'), 
                           null.ok = TRUE)
  
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  if(!is.null(censor_threshold) & is.null(censor_symbol)){
    stop("censor_threshold specified, must specify censor_symbol")
  }
  if(is.null(censor_threshold) & !is.null(censor_symbol)){
    stop("censor_symbol specified, must specify censor_threshold")
  }
  
  # censor values if specified
  if(!is.null(censor_threshold) & !is.null(censor_symbol)){
    obs = censor_values(value = obs, censor_threshold = censor_threshold, censor_symbol = censor_symbol)
    mod = censor_values(value = mod, censor_threshold = censor_threshold, censor_symbol = censor_symbol)
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # emtpy dataframe to populate
  gof_summary <- data.frame(test = character(),
                            value = numeric(),
                            p_value = numeric(),
                            stringsAsFactors = FALSE)
  
  # correlation tests
  if("cor" %in% metrics){
    correlation_test <- GOF_correlation_tests(mod = mod, obs = obs, ... = ...)
    gof_summary <- rbind(gof_summary, correlation_test)
  }
  
  # Kling--Gupta efficiency
  if("kge" %in% metrics){
    kge <- GOF_kling_gupta_efficiency(mod = mod, obs = obs, modified = kge_modified)
    gof_summary <- rbind(gof_summary, data.frame(test = "kling_gupta_efficiency",
                                                 value = kge,
                                                 p_value = NA_real_))
  }

  
  # mean abs error
  if("mae" %in% metrics){
    mae <- GOF_mean_absolute_error(mod = mod, obs = obs)
    gof_summary <- rbind(gof_summary, data.frame(test = "mean_absolute_error",
                                                 value = mae,
                                                 p_value = NA_real_))
  }
  
  
  # mean error
  if("me" %in% metrics){
    me <- GOF_mean_error(mod = mod, obs = obs)
    gof_summary <- rbind(gof_summary, data.frame(test = "mean_error",
                                                 value = me,
                                                 p_value = NA_real_))
  }
  
  
  # NSE
  if("nse" %in% metrics){
    nse <- GOF_nash_sutcliffe_efficiency(mod = mod, obs = obs, j = nse_j)
    gof_summary <- rbind(gof_summary, data.frame(test = "nash_sutcliffe_efficiency",
                                                 value = nse,
                                                 p_value = NA_real_))
  }
  
  
  # percent bias
  if("pb" %in% metrics){
    pb <- GOF_percent_bias(mod = mod, obs = obs)
    gof_summary <- rbind(gof_summary, data.frame(test = "percent_bias",
                                                 value = pb,
                                                 p_value = NA_real_))
  }
  
  
  # rmse
  if("rmse" %in% metrics){
    rmse <- GOF_rmse(mod = mod, obs = obs, normalize = NULL)
    gof_summary <- rbind(gof_summary, data.frame(test = "RMSE",
                                                 value = rmse,
                                                 p_value = NA_real_))
  }
  
  
  # nrmse
  if("nrmse" %in% metrics){
    nrmse <- GOF_rmse(mod = mod, obs = obs, normalize = rmse_normalize)
    gof_summary <- rbind(gof_summary, data.frame(test = "NRMSE",
                                                 value = nrmse,
                                                 p_value = NA_real_))
  }
  
  
  # volumetric efficiency
  if("ve" %in% metrics){
    ve <- GOF_volumetric_efficiency(mod = mod, obs = obs)
    gof_summary <- rbind(gof_summary, data.frame(test = "volumetric_efficiency",
                                                 value = ve,
                                                 p_value = NA_real_))
  }
  
  # format output
  names(gof_summary) <- c("metric", "value", "p_value")
  gof_summary <- tibble::as_tibble(gof_summary)
  
  return(gof_summary)
}
