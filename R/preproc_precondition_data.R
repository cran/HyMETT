# preproc_precondition_data
#' Pre-conditions data with time information and n-day moving averages
#' 
#' @description Pre-conditions data with time information and n-day moving averages, with options 
#' to fill missing days with `NA` values.
#'
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'Date' or 'character' vector when `data = NULL`, or 'character' string identifying 
#'   Date column name when `data` is specified. Dates associated with each value in `value` 
#'   parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying year column 
#'   name when `data` is specified. Values to precondition and calculate n-day moving averages 
#'   from. N-day moving averages only calculated for daily data.
#' @param date_format 'character' string. Format of `Date`. Default is `"%Y-%m-%d"`.
#' @param fill_daily 'logical' value. Should gaps in `Date` and `value` be filled using \cr
#'   [`preproc_fill_daily`]. Default is `TRUE`.
#'                     
#' @return A data.frame with Date, value, and additional columns with time and n-day moving average 
#' information.
#' 
#' @details 
#' These columns are added to the data:
#' \describe{
#'   \item{`year`}{}
#'   \item{`month`}{}
#'   \item{`day`}{}
#'   \item{`decimal_date`}{}
#'   \item{`WY`}{Water Year: October 1 to September 30}
#'   \item{`CY`}{Climate Year: April 1 to March 30}
#'   \item{`Q3`}{3-Day Moving Average: computed at end of moving interval}
#'   \item{`Q7`}{7-Day Moving Average: computed at end of moving interval}
#'   \item{`Q30`}{30-Day Moving Average: computed at end of moving interval}
#'   \item{`jd`}{Julian date}
#' }
#'  
#' This function also checks the time step of the data to make sure that it is daily time step. Daily 
#' values with gaps are important to fill with `NA` to ensure proper calculation of n-day moving 
#' averages. Use `fill_daily = TRUE` or [`preproc_fill_daily`]. Other time steps are currently not 
#' supported and will return the data.frame without moving averages computed.
#' 
#' @export
#' 
#' @keywords preprocessing-data
#'
#' @seealso \code{\link{preproc_fill_daily}}, \code{\link[zoo]{rollmean}}
#'
#' @examples
#' preproc_precondition_data(data = example_obs, Date = "Date", value = "streamflow_cfs")
#' 
# TODO Make it so that script doesn't break if there are NAs/empty rows at end of dataset.
# TODO drop the zoo dependency in exchange for stats. This is from the moving average section. I just have not had time to test the stats version
preproc_precondition_data <- function(data = NULL,
                                      Date,
                                      value,
                                      date_format = "%Y-%m-%d",
                                      fill_daily = TRUE){
  # check inputs
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(value)
    checkmate::assert_names(colnames(data), must.include = c(Date, value))
    df <- data.frame(Date = data[[Date]],
                     value = data[[value]],
                     stringsAsFactors = FALSE)
    df$Date <- base::as.Date(df$Date, format = date_format)
  } else {
    # check assertions on inputs and lengths
    Date <- base::as.Date(Date, format = date_format)
    checkmate::assert_date(Date)
    checkmate::assert_numeric(value)
    if(length(Date) != length(value)){
      stop("'Date' and 'value' are unequal lenghts")
    }
    df <- data.frame(Date = Date,
                     value = value)
  }
  
  
  # create dataframe, fill and validate daily
  if(fill_daily == TRUE){
    df <- preproc_fill_daily(data = data,
                             Date = Date,
                             value = value,
                             date_format = date_format)
  }
  preproc_validate_daily(data = df,
                         Date = "Date",
                         value = "value",
                         date_format = date_format)

  # add columns
  df$year         <- lubridate::year(df$Date)
  df$month        <- lubridate::month(df$Date)
  df$day          <- lubridate::day(df$Date)
  df$decimal_date <- lubridate::decimal_date(df$Date) # decimal date
  df$WY <- ifelse(df$month >= 10, df$year + 1, df$year)# water year column
  df$CY <- ifelse(df$month >= 4, df$year + 1, df$year)# climatic year column
  
  # check daily data
  diff_in_days = difftime(df$Date[2], df$Date[1], units = "days") 
  if (diff_in_days == 1){
    message("The first timestep is one day. This dataset can be processed as is.")}
  if (diff_in_days > 1){
    message("The first timestep is greater one day. This dataset may not be appropriate for 
          the evaluation as it is for n-day moving averages computed.")
    #return(df)
  }
  if (diff_in_days < 1){
    message("The first timestep is less than one day. This dataset needs to be aggregated to daily
        before n-day moving averages computed.")
    return(df)
  }
  
  # compute n-day moving averages (computed at end of moving interval)
  # please note that we do not subset by wy/cy before running these moving averages. 
  # As such we are compiling data across wy/cy s which in a few cases may result in double counting of events. 
  # We determined that this was prefered compared to our other option of subseting data first which would result in the removal of a significant amount of data.
  ndays <- c(3, 7, 30)
  new.cols <- c("Q3", "Q7", "Q30")
  df[,new.cols] <- NA_real_
  for (nd in ndays){
    # This is a smwrBase dependency which we do not want. 
    # smwr_mean <- round(smwrBase::movingAve(df$Q,span=nd,order=0,pos="end"),digits=3) 
    # This is a moving average function with a "zoo" dependency
    # zoo_mean <- round(zoo::rollmean(df$Q, k=nd, align = "right", na.pad = TRUE), digits=3)
    # This is a moving average with a "stats" dependency. 
    # ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
    # stats_mean <- round(ma(df$Q, 3),digits = 3)
    
    df[,new.cols[which(nd==ndays)]] <- round(zoo::rollmean(df$value, k=nd, align = "right", na.pad = TRUE), digits=3)
  }
  
  # compute Julian dates on Calendar Year basis (for WSCVD calculations)
  df$jd <- lubridate::yday(df$Date)
  
  return(df)
}
