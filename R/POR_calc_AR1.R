# POR_calc_AR1
#' calculates lag-one autocorrelation (AR1) coefficient for a time series
#' 
#' @description calculates lag-one autocorrelation (AR1) coefficient for a time series
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'numeric' vector of Dates corresponding to each `value` when `data = NULL`, or 
#'   'character' string identifying Date column name when `data` is specified.
#' @param value 'numeric' vector of values (often streamflow) when `data = NULL`, or 
#'   'character' string identifying value column name when `data` is specified. Assumed to be 
#'   daily or monthly.
#' @param time_step 'character' value. Either `"daily"` or `"monthly"`.
#'
#' @return A data.frame with calculated seasonal amplitude and phase.
#' 
#' @details 
#' The function  calculates lag-one autocorrelation (AR1) coefficient for a time series using the\cr
#' [`stats::ar`] function. When applied to an observed or modeled time series of streamflow, the \cr
#' [`POR_deseasonalize`] function can be applied to the raw data prior to running the 
#' `POR_calc_AR1` function.
#' 
#' @export
#'
#' @seealso \code{\link{POR_deseasonalize}}, \code{\link[stats]{ar}}
#' 
#' @keywords period-of-record
#' 
#' @references 
#' Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H., and Kiang, J.E., 2014, 
#' A comparison of methods to predict historical daily streamflow time series in the southeastern 
#' United States: U.S. Geological Survey Scientific Investigations Report 2014–5231, 34 p. 
#' \[Also available at https://doi.org/10.3133/sir20145231.\]
#' 
#' @examples 
#' POR_calc_AR1(data = example_obs, Date = "Date", value = "streamflow_cfs")
#' 
# Dev details:
# FDSS calculations based on code from Stacey Archfield
# Function redeveloped by William Farmer, 09 June 2015
# Revised by Tom Over and Jason Griffin, 2018-19
# Function revised by Sara Levin, May, 2020
POR_calc_AR1<-function(data = NULL, Date, value, time_step = c("daily","monthly")){
  # check inputs
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(value)
    checkmate::assert_names(colnames(data), must.include = c(Date, value))
    Date <- data[[Date]]
    value <- data[[value]]
  }
  # check assertions on inputs and lengths
  checkmate::assert_date(Date)
  checkmate::assert_numeric(value)
  if(length(Date) != length(value)){
    stop("'Date' and 'value' are unequal lenghts")
  }
  time_step <- match.arg(time_step)
  # validate no daily gaps
  if(time_step == "daily"){
    preproc_validate_daily(Date = Date, value = value)
  }
  
  
  # Auto-regressive Lag-1 Correlation (FDSS 5)
  # Compute AR1 on monthly deasonalized real (nonlog) daily-flow values
  ar1 <- tryCatch({
    armdl <- stats::ar(scale(value, center = TRUE, scale = TRUE),
                aic = FALSE, order.max = 1, method = "yule-walker", na.action = stats::na.pass)
    armdl$ar
  },
  warning = function(w){
    message(paste(w))
    return(NA_real_)
  },
  error = function(e){
    message(paste(e))
    return(NA_real_)
  })
  
  return(ar1)
} #end function
