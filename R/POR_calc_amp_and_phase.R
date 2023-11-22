#POR_calc_amp_and_phase
#' Calculate the seasonal amplitude and phase of a daily time series
#' 
#' @description Calculates the  seasonal amplitude and phase of a daily time series.
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'numeric' vector of Dates corresponding to each `value` when `data = NULL`, or 
#'   'character' string identifying Date column name when `data` is specified.
#' @param value 'numeric' vector of values (often streamflow) when `data = NULL`, or 'character' 
#'   string identifying value column name when `data` is specified. Assumed to be daily or monthly.
#' @param time_step 'character' value. Either `"daily"` or `"monthly"`, Default is `"daily"`.
#' 
#' @return A data.frame with calculated seasonal amplitude and phase
#' 
#' @references 
#' Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H., and Kiang, J.E., 2014, 
#' A comparison of methods to predict historical daily streamflow time series in the southeastern 
#' United States: U.S. Geological Survey Scientific Investigations Report 2014–5231, 34 p. 
#' \[Also available at https://doi.org/10.3133/sir20145231.\]
#' 
#' @export
#' 
#' @keywords period-of-record
#' 
#' @examples 
#' POR_calc_amp_and_phase(data = example_obs, Date = "Date", value = "streamflow_cfs")
#' 
# Dev details:
# Based on analysis from SIR 2014-5231 (circa 16 June 2014)
# FDSS calculations based on code from Stacey Archfield
# Function redeveloped by William Farmer, 09 June 2015
# Revised by Tom Over and Jason Griffin, 2018-2019
# Revised by Sara Levin 2020
POR_calc_amp_and_phase<-function(data = NULL, Date, value, time_step = c("daily","monthly")){
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
  time_step <- match.arg(time_step)
  if(length(Date) != length(value)){
    stop("'Date' and 'value' are unequal lenghts")
  }
  
  #check time_step parameter values
  if (!time_step %in% c("daily", "monthly")){
    warning("time step parameter must be either 'daily' or 'monthly' ")
    stop()
  }
  
  # Seasonal amplitude and phase on real (non-log) flow values(FDSS 6&7)
  seasonal_amplitude <- tryCatch({
    value.s <- scale(value, center = TRUE, scale = TRUE)[,1] # scale() returns a one-column matrix
    decimal_year <- as.double(format(Date, "%Y")) + as.POSIXlt(Date,"yyyy-mm-dd")$yday/365.25 
    seasonfit <- stats::lm(value.s ~ cos(2*pi*decimal_year) + sin(2*pi*decimal_year))
    seasCos <- as.vector(seasonfit$coefficients[2])
    seasSin <- as.vector(seasonfit$coefficients[3])
    
    sqrt((seasCos^2) + (seasSin^2)) #return seasonal amplitude value
  },
  warning = function(w){
    message(paste(w))
    return(NA_real_)
  },
  error = function(e){
    message(paste(e))
    return(NA_real_)
  })
  if(is.na(seasonal_amplitude)){
    seasCos <- NA_real_
    seasSin <- NA_real_
  }
  
  # Compute phase angle following Wilks, Stat. Methods in the Atmos. Sciences, 1995, Sec. 8.4.3
  if(!is.na(seasCos) && !is.na(seasSin)){
    seasTol <- 0.000001  # tolerance for seasCos being effectively zero
    seasPhs.base <- atan(seasSin/seasCos)
    if (abs(seasCos) < seasTol){
      seasPhs <- pi/2
    } else if (seasCos>0){
      if (seasSin > 0){
        seasPhs <- seasPhs.base
      } else {
        seasPhs <- seasPhs.base + 2*pi
      }  
    } else {
      if ((seasPhs.base + pi >= 0) & (seasPhs.base + pi < (2*pi))){
        seasPhs <- seasPhs.base + pi
      } else  {
        seasPhs <- seasPhs.base - pi
      }
    } #end if
  } else{
    seasPhs <- NA_real_
  }
  
  # convert seasPhs to day-of-the-calendar year (in the range [0,365.25])
  if (time_step == "daily"){
    seasonal_phase <- seasPhs*365.25/(2*pi)
  }  else {
    seasonal_phase <- seasPhs*12/(2*pi)
  } 
  
  
  df <- data.frame(metric = c("seasonal_amplitude","seasonal_phase"),
                   value = c(seasonal_amplitude,seasonal_phase))  
  return(df)  
}  #end function
