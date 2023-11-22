####################################################################################################
# deseasonalize
#' Removes seasonal trends from a daily or monthly time series.
#' 
#' @description Removes seasonal trends from a daily or monthly time series. Daily data are 
#' deseasonalized by subtracting monthly mean values. Monthly data are deseasonalized by 
#' subtracting mean monthly values.
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'              Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'numeric' vector of Dates corresponding to each `value` when `data = NULL`, or\cr
#'             'character' string identifying Date column name when `data` is specified.
#' @param value 'numeric' vector of values (often streamflow) when `data = NULL`, or\cr
#'              'character' string identifying value column name when `data` is specified.\cr
#'               (assumed to be daily or monthly).
#' @param time_step 'character' value. Either `"daily"` or `"monthly"`.
#' 
#' @return Deseasonalized values.
#' 
#' @details 
#' The deseasonalize function removes seasonal trends from a daily or monthly time series
#' and returns a deseasonalized time series, which can be used in the [`POR_calc_AR1`] function.
#' 
#' @export
#'
#' @seealso \code{\link{POR_calc_AR1}}
#' 
#' @keywords preprocessing-data
#' @keywords period-of-record
#' 
#' @examples 
#' POR_deseasonalize(data = example_obs, Date = "Date", value = "streamflow_cfs")
#' 
POR_deseasonalize<-function(data = NULL, Date, value, time_step = c("daily","monthly")){
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
    warning("time step parameter must be either daily or monthly")
    stop()
  }
  
  if(time_step == "daily"){    
    df <- data.frame(value = value,
                     Date = Date)
    preproc_validate_daily(data = df, Date = "Date", value = "value")
    df$decimal.month <- as.numeric(format(df$Date,"%Y")) + 
                        as.numeric(format(df$Date,"%m")) / 12
    monthly_means <- as.data.frame(stats::aggregate(df$value ~ df$decimal.month, FUN = mean))
    names(monthly_means) <- c("decimal.month", "value_monthly")
    df <- plyr::join(df, monthly_means, by = "decimal.month")
    value_deseasonalized <- df$value - df$value_monthly
  }
  
  if(time_step=="monthly"){
    df <- data.frame(value = value,
                     Date = Date)
    df$month <- format(df$Date, "%m")
    mean_monthly <- as.data.frame(stats::aggregate(df$value ~ df$month, FUN = mean))
    names(mean_monthly) <- c("month","value_ave")
    df <- plyr::join(df, mean_monthly, by = "month")
    value_deseasonalized <- df$value - df$value_ave
  }#end if
  
  return(value_deseasonalized)
}  #end function
