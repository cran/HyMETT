# benchmark_KGE_DOY
#' Calculate benchmark Kling--Gupta efficiency (KGE) values from day-of-year (DOY) observations
#'
#' @description Calculate benchmark Kling--Gupta efficiency (KGE) values from daily observed 
#' time-series data
#'
#' @param obs_preproc 'data.frame' of daily observational data, preprocessed as output from \cr
#'   [`preproc_precondition_data`] or [`preproc_main`] `"daily"`.
#'
#' @return A data.frame with columns `"KGE_DOY_mean"` and `"KGE_DOY_median"`.
#'
#' @details
#' This function calculates a "benchmark" KGE value (see Knoben and others, 2020) from a daily 
#' observed data time-series. First, the interannual mean and median is calculated for each day of 
#' the calendar year. Next, the interannual mean and median values are joined to each corresponding 
#' day in the observation time series. Finally, a KGE value ([`GOF_kling_gupta_efficiency`]) is 
#' calculated comparing the mean or median value repeated time series to the daily observational 
#' time series. These benchmark KGE values can be used as comparisons for modeled (simulated) 
#' calibration results.
#'
#' @export
#' 
#' @keywords benchmark
#' @keywords goodness-of-fit
#' 
#' @references 
#' Knoben, W.J.M, Freer, J.E., Peel, M.C., Fowler, K.J.A, Woods, R.A., 2020. A Brief Analysis of 
#' Conceptual Model Structure Uncertainty Using 36 Models and 559 Catchments: Water Resources 
#' Research, v. 56.\cr
#' \[Also available at https://doi.org/10.1029/2019WR025975.\]
#' 
#' @examples 
#' benchmark_KGE_DOY(obs_preproc = example_preproc)
#' 
benchmark_KGE_DOY <- function(obs_preproc) {

  # check assertions
  checkmate::assert_data_frame(obs_preproc)
  checkmate::assert_names(colnames(obs_preproc), must.include = c("value", "month", "day", "Date"))

  # calculate DOY mean and median
  obs_mean   <- stats::aggregate(obs_preproc$value, by = list(obs_preproc$month, obs_preproc$day), FUN = mean)
  obs_median <- stats::aggregate(obs_preproc$value, by = list(obs_preproc$month, obs_preproc$day), FUN = stats::median)
  colnames(obs_mean)   <- c("month", "day", "DOY_mean")
  colnames(obs_median) <- c("month", "day", "DOY_median")

  # merge to observations
  obs_preproc <- merge(obs_preproc, obs_mean,   by = c("month", "day"), all = TRUE, sort = FALSE)
  obs_preproc <- merge(obs_preproc, obs_median, by = c("month", "day"), all = TRUE, sort = FALSE)
  obs_preproc <- obs_preproc[order(obs_preproc$Date), ]

  # calculate KGE
  KGE_DOY_mean   <- HyMETT::GOF_kling_gupta_efficiency(obs_preproc$DOY_mean,   obs_preproc$value)
  KGE_DOY_median <- HyMETT::GOF_kling_gupta_efficiency(obs_preproc$DOY_median, obs_preproc$value)

  # return data.frame
  df <- data.frame(
    KGE_DOY_mean   = KGE_DOY_mean,
    KGE_DOY_median = KGE_DOY_median,
    stringsAsFactors = FALSE
  )
  return(df)
}