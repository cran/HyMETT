####################################################################################################
# GOF_correlation_tests
#' Calculates Kendall's Tau, Spearman's Rho, Pearson Correlation 
#' 
#' @description Calculates Kendall's Tau, Spearman's Rho, Pearson Correlation, and p-values
#' as a wrapper to the [`stats::cor.test`] function. Output is tidy-style data.frame.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`
#' @param ... Further arguments to be passed to or from [`stats::cor.test`].
#'
#' @return A tibble ([`tibble::tibble`]) with test statistic values and p-values.
#'
#' @export
#' 
#' @details 
#' See [`stats::cor.test`] for more details and further arguments to be passed to or from methods.
#' Defaults are used.
#'
#' @seealso \code{\link[stats]{cor.test}}
#' 
#' @keywords goodness-of-fit
#' 
#' @examples
#' GOF_correlation_tests(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' 
GOF_correlation_tests <- function(mod, 
                                  obs, 
                                  na.rm = TRUE,
                                  ...) {
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # Spearman's Rho
  rho_all <- tryCatch({stats::cor.test(obs, mod, method = "spearman", ...)},
                      error = function(e){
                        message(paste(e))
                        return(NA_real_)
                      })
  if(!is.na(rho_all[1])){
    rho_df <- data.frame(test = "spearman_rho",
                         value = rho_all$estimate[1],
                         p_value = rho_all$p.value[1],
                         stringsAsFactors = FALSE)
  } else{
    rho_df <- data.frame(test = "spearman_rho",
                         value = NA_real_,
                         p_value = NA_real_,
                         stringsAsFactors = FALSE)
  }
  
  
  # Kendall's Tau
  tau_all <- tryCatch({stats::cor.test(obs, mod, method = "kendall", ...)},
                      error = function(e){
                        message(paste(e))
                        return(NA_real_)
                      })
  if(!is.na(tau_all[1])){
    tau_df <- data.frame(test = "kendall_tau",
                         value = tau_all$estimate[1],
                         p_value = tau_all$p.value[1],
                         stringsAsFactors = FALSE)
  } else{
    tau_df <- data.frame(test = "kendall_tau",
                         value = NA_real_,
                         p_value = NA_real_,
                         stringsAsFactors = FALSE)
  }
  
  
  # Pearson correlation
  r_all <- tryCatch({stats::cor.test(obs, mod, method = "pearson", ...)},
                    error = function(e){
                      message(paste(e))
                      return(NA_real_)
                    })
  if(!is.na(r_all[1])){
    r_df <- data.frame(test = "pearson_r",
                       value = r_all$estimate[1],
                       p_value = r_all$p.value[1],
                       stringsAsFactors = FALSE)
  } else{
    r_df <- data.frame(test = "pearson_r",
                       value = NA_real_,
                       p_value = NA_real_,
                       stringsAsFactors = FALSE)
  }
  
  
  
  # combine correlation tests
  all_df <- rbind(rbind(rho_df, tau_df), r_df)
  all_df <- tibble::as_tibble(all_df)
  
  return(all_df)
}

####################################################################################################
# GOF_kling_gupta_efficiency
#' Calculate Kling--Gupta Efficiency (KGE) 
#' 
#' @description Calculate Kling--Gupta Efficiency (KGE) (or modified KGE ('KGE)) between 
#' modeled (simulated) and observed values.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param modified 'boolean' `TRUE` or `FALSE`. Should the KGE calculation use the original 
#'   variability ratio in the standard deviations (see Gupta and others, 2009) (`modified  = FALSE`)
#'   or the modified variability ratio in the coefficient of variations (see Kling and others, 2012)
#'   (`modified = TRUE`). Default is `FALSE`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'
#' @return Value of computed KGE or 'KGE.
#' 
#' @export
#' 
#' @keywords goodness-of-fit
#' 
#' @references 
#' Kling, H., Fuchs, M. and Paulin, M., 2012. Runoff conditions in the upper Danube basin under an 
#' ensemble of climate change scenarios: Journal of Hydrology, v. 424-425, p. 264-277.\cr
#' \[Also available at https://doi.org/10.1016/j.jhydrol.2012.01.011.\]
#' 
#' Gupta, H.V., Kling, H., Yilmaz, K.K., and Martinez, G.G., 2009. Decomposition of the mean 
#' squared error and NSE performance criteria: Implications for improving hydrological modelling:
#' Journal of Hydrology, v. 377, no.1-2, p. 80-91.\cr
#' \[Also available at https://doi.org/10.1016/j.jhydrol.2009.08.003.\]
#' 
#' 
#' @examples
#' GOF_kling_gupta_efficiency(
#'   mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
#' )
#' 
GOF_kling_gupta_efficiency <- function(mod,
                                       obs,
                                       modified = FALSE,
                                       na.rm = TRUE){
  
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(modified)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # get pearson correlation coefficient
  r <- tryCatch({stats::cor(obs, mod, method = "pearson")},
                error = function(e){
                  message(paste(e))
                  return(NA_real_)
                })
  
  # stat moments
  Uo <- mean(obs)
  Um <- mean(mod)
  
  SDo <- stats::sd(obs)
  SDm <- stats::sd(mod)
  
  # get bias ratio
  beta <- Um / Uo
  
  # get variability ratio. Alpha for original KGE, Gamma for modified KGE
  if (modified == TRUE) {
    CVo <- SDo / Uo
    CVm <- SDm / Um
    var_ratio <- CVm / CVo
  } else {
    var_ratio <- SDm / SDo
  }
  
  # get Euclidian distance
  ED <- sqrt(
    ((r - 1)^2) +
      ((beta - 1)^2) +
      ((var_ratio - 1)^2)
  )
  
  # return KGE
  KGE <- 1 - ED
  
  KGE
}

####################################################################################################
# GOF_mean_absolute_error
#' Calculates mean absolute error (MAE).
#' 
#' @description Calculates mean absolute error (MAE) between modeled (simulated) and observed 
#'   values. Error is defined as modeled minus observed.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed 
#'   before calculating. If `NA` values are present and `na.rm = FALSE`, then function will return 
#'   `NA`. Default is `TRUE`.
#'                       
#' @return Value of calculated mean absolute error (MAE).
#' 
#' @export
#' 
#' @details 
#' The absolute value of each modeled-observed pair error is calculated, then the mean of those 
#' values taken. Values returned are in units of input data.
#' 
#' @keywords goodness-of-fit
#' 
#' @examples
#' GOF_mean_absolute_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' 
GOF_mean_absolute_error <- function(mod,
                                    obs,
                                    na.rm = TRUE){
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate MAE
  abs_error <- abs(mod - obs)
  mae <- mean(abs_error, na.rm = FALSE)
  
  return(mae)
}

####################################################################################################
# GOF_mean_error
#' Calculates mean error.
#' 
#' @description Calculates mean error between modeled (simulated) and observed values. Error is 
#' defined as modeled minus observed.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'                       
#' @return Value of calculated mean error.
#' 
#' @export
#' 
#' @details 
#' Values returned are in units of input data.
#' 
#' @keywords goodness-of-fit
#' 
#' @examples
#' GOF_mean_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' 
GOF_mean_error <- function(mod,
                           obs,
                           na.rm = TRUE){
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate error
  error <- mod - obs
  mean_error <- mean(error, na.rm = FALSE)
  
  return(mean_error)
}

####################################################################################################
# GOF_nash_sutcliffe_efficiency
#' Calculate Nash--Sutcliffe Efficiency (NSE) 
#' 
#' @description Calculate Nash--Sutcliffe Efficiency (NSE) (with options for modified NSE) between 
#' modeled (simulated) and observed values.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param j 'numeric' value. Exponent value for modified NSE (mNSE) equation. Default value is 
#'   `j = 2`, which is traditional NSE equation.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'
#' @return Value of computed NSE or mNSE.
#' 
#' @export
#' 
#' @keywords goodness-of-fit
#' 
#' @references 
#' Krause, P., Boyle, D.P., and Base, F., 2005. Comparison of different efficiency criteria for 
#' hydrological model assessment: Advances in Geosciences, v. 5, p. 89-97.\cr
#' \[Also available at https://doi.org/10.5194/adgeo-5-89-2005.\]
#' 
#' Legates D.R and McCabe G.J., 1999, Evaluating the use of "goodness-of-fit" measures in hydrologic 
#' and hydroclimatic model validation: Water Resources Research. v. 35, no. 1, p. 233-241. 
#' \[Also available at https://doi.org/10.1029/1998WR900018.\]
#' 
#' Nash, J.E. and Sutcliffe, J.V., 1970, River flow forecasting through conceptual models part I: 
#' A discussion of principles: Journal of Hydrology, v. 10, no. 3, p. 282-290. 
#' \[Also available at https://doi.org/10.1016/0022-1694(70)90255-6.\]
#' 
#' @examples
#' GOF_nash_sutcliffe_efficiency(
#'   mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
#' )
#' 
GOF_nash_sutcliffe_efficiency <- function(mod,
                                          obs,
                                          j = 2,
                                          na.rm = TRUE){
  
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_numeric(j)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate NSE or mNSE
  norm_var_obs <- sum(abs(obs - mean(obs))^j) 
  
  if(norm_var_obs != 0 & !is.na(norm_var_obs)){
    NSE <- 1 - (sum(abs(obs - mod)^j) / norm_var_obs) 
  }else{
    NSE <- NA_real_
  }
  
  return(NSE)
}

####################################################################################################
# GOF_percent_bias
#' Calculates percent bias.
#' 
#' @description Calculates percent bias between modeled (simulated) and observed values.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'
#' @return Value of calculated percent bias as percent.
#' 
#' @export
#' 
#' @details 
#' Values returned are in percent.
#' 
#' @keywords goodness-of-fit
#' 
#' @examples
#' GOF_percent_bias(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' 
GOF_percent_bias <- function(mod,
                             obs,
                             na.rm = TRUE){
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate percent bias
  sum_obs <- sum(obs)
  
  if(sum_obs != 0 & !is.na(sum_obs)){
    percent_bias <- 100 * (sum(mod - obs) / sum_obs) 
  }else{
    percent_bias <- NA_real_
  }
  
  return(percent_bias)
}

####################################################################################################
# GOF_rmse
#' Calculate root-mean-square error with options to normalize 
#' 
#' @description Calculate root-mean-square error (RMSE) between modeled (simulated) and observed 
#'   values. Error is defined as modeled minus observed.
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param normalize 'character' value. Option to normalize the root-mean-square error (NRMSE) by 
#'   several normalizing options. Default is `'none'`(no normalizing). RMSE is 
#'   returned.\cr
#'     `'mean'`. RMSE is normalized by the mean of `obs`.\cr
#'     `'range'`. RMSE is normalized by the range `(max - min)` of `obs`.\cr
#'     `'stdev'`. RMSE is normalized by the standard deviation of `obs`.\cr
#'     `'iqr-#'`. RMSE is normalized by the inter-quartile range of `obs`, with distribution type 
#'     (see [`stats::quantile`] function) indicated by integer number (for example `"iqr-8"`). 
#'     If no type specified, default type is `iqr-7`, the quantile function default.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'                       
#' @return 'numeric' value of computed root-mean-square error (RMSE) or 
#'   normalized root-mean-square error (NRMSE)
#' 
#' @export
#'
#' @keywords goodness-of-fit
#'
#' @examples
#' # RMSE
#' GOF_rmse(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
#' # NRMSE
#' GOF_rmse(
#'   mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs, normalize = 'stdev'
#' )
#' 
GOF_rmse <- function(mod,
                     obs,
                     normalize = c('none','mean','range','stdev','iqr', 'iqr-1','iqr-2','iqr-3','iqr-4','iqr-5',
                                   'iqr-6','iqr-7','iqr-8','iqr-9', NULL),
                     na.rm = TRUE){
  
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  if(!is.null(normalize)) {
    normalize <- match.arg(normalize)
    if(normalize == 'none') {
      normalize <- NULL
    }
  }
  
  checkmate::assert_choice(normalize, choices = c('none','mean','range','stdev',
                                                  'iqr',
                                                  'iqr-1','iqr-2','iqr-3','iqr-4',
                                                  'iqr-5','iqr-6','iqr-7','iqr-8','iqr-9'), null.ok = TRUE)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate root mean square error
  ## sum squared errors
  sum_sq_error <- sum((mod - obs)^2)
  ## mean square error
  mean_sq_error <- sum_sq_error/length(obs)
  ## root mean square error
  rmse <- sqrt(mean_sq_error)
  
  # normalize RMSE by OBS if normalize option selected
  if(!is.null(normalize)){
    if(normalize == "mean"){
      norm <- mean(obs) 
    }
    if(normalize == "range"){ 
      norm <- max(obs) - min(obs) 
    }
    if(normalize == "stdev"){
      norm <- stats::sd(obs)
    }
    if(normalize %in% c('iqr',
                        'iqr-1','iqr-2','iqr-3','iqr-4',
                        'iqr-5','iqr-6','iqr-7','iqr-8','iqr-9')){
      qtype <- as.numeric(unlist(strsplit(normalize, split = "-", fixed = TRUE))[2])
      if(is.na(qtype)){qtype <- 7}
      norm <- unname(stats::quantile(
        obs, 0.75, na.rm = T, type = qtype) - stats::quantile(obs, 0.25, na.rm = TRUE, type = qtype)
      ) 
    }
    if(norm != 0 | is.na(norm)){
      rmse <- rmse / norm
    }else{
      rmse <- NA_real_
      warning("RMSE normalization method equals 0, returning NA")
    }
  }
  
  return(rmse)
}

####################################################################################################
# GOF_volumetric_efficiency
#' Calculate Volumetric Efficiency 
#' 
#' @description Calculate Volumetric efficiency (VE) between modeled (simulated) and observed 
#' values. VE is defined as the fraction of water delivered at the proper time 
#' (Criss and Winston, 2008).
#' 
#' @param mod 'numeric' vector. Modeled or simulated values. Must be same length as `obs`.
#' @param obs 'numeric' vector. Observed or comparison values. Must be same length as `mod`.
#' @param na.rm 'boolean' `TRUE` or `FALSE`. Should `NA` values be removed before computing. If any 
#'   `NA` values are present in `mod` or `obs`, the *i*th position from each will be removed before 
#'   calculating. If `NA` values are present and `na.rm = FALSE`, then function will return `NA`. 
#'   Default is `TRUE`.
#'                       
#' @return Value of computed Volumetric efficiency.
#' 
#' @export
#' 
#' @details 
#' Volumetric efficiency was proposed in order to circumvent some problems associated to the
#' Nash--Sutcliffe efficiency. It ranges from `0` to `1` and represents the fraction of water 
#' delivered at the proper time; its compliment represents the fractional volumetric mismatch 
#' (Criss and Winston, 2008).
#' 
#' @keywords goodness-of-fit
#' 
#' @references 
#' Criss, R.E. and Winston, W.E., 2008, Do Nash values have value? Discussion and alternate 
#' proposals: Hydrological Processes, v. 22, p. 2723-2725.\cr
#' \[Also available at https://doi.org/10.1002/hyp.7072.\]
#' 
#' Zambrano-Bigiarini, M., 2020, hydroGOF: Goodness-of-fit functions for comparison of simulated 
#' and observed hydrological time series R package version 0.4-0. 
#' accessed September 16, 2020, at https://github.com/hzambran/hydroGOF. 
#' \[Also available at https://doi.org/10.5281/zenodo.839854.\]
#' 
#' @examples
#' GOF_volumetric_efficiency(
#'   mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
#' )
#' 
GOF_volumetric_efficiency <- function(mod,
                                      obs,
                                      na.rm = TRUE){
  
  # check assertions and lengths
  checkmate::assert_numeric(mod)
  checkmate::assert_numeric(obs)
  checkmate::assert_logical(na.rm)
  if(length(mod) != length(obs)){
    stop("'mod' and 'obs' are unequal lenghts")
  }
  
  # remove NA indexes
  if(na.rm == TRUE){
    na_index <- which(!is.na(mod) & !is.na(obs))
    mod <- mod[na_index]
    obs <- obs[na_index]
  }
  
  # calculate Vol Eff
  sum_obs <- sum(obs)
  
  if(sum_obs != 0 & !is.na(sum_obs)){
    vol_eff <- 1 - (sum(abs(obs - mod)) / sum_obs) 
  }else{
    vol_eff <- NA_real_
  }
  
  return(vol_eff)
}
