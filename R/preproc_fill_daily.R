# preproc_fill_daily
#' Fills daily data with missing dates as `NA` values
#' 
#' @description Fills daily data with missing dates as `NA` values. Days that are 
#' absent from the daily time series are inserted with a corresponding value of `NA`.
#'
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'Date' or 'character' vector when `data = NULL`, or 'character' string identifying 
#'   Date column name when `data` is specified. Date associated with each value in `value` 
#'   parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying values 
#'   column name when `data` is specified.
#' @param POR_start 'character' value. Optional period of record start. If not specified, defaults 
#'   to `min(Date)`.
#' @param POR_end 'character' value. Optional period of record end. If not specified, defaults to 
#'   `max(Date)`.
#' @param date_format 'character' string. Format of Date. Default is `"%Y-%m-%d"`.
#'                     
#' @return A data.frame with `Date` and `value`, sequenced from `POR_start` to `POR_end` by 1 day.
#' 
#' @details
#' Can be used prior to [`preproc_precondition_data`] to fill daily data before computation
#' of n-day moving averages, or prior to [`preproc_audit_data`].
#' 
#' @export
#' 
#' @keywords preprocessing-data
#'
#' @seealso \code{\link{preproc_audit_data}}, \code{\link{preproc_precondition_data}}
#' 
#' @examples 
#' Dates = c(seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "1 day"),
#'           seq.Date(as.Date("2020-01-20"), as.Date("2020-01-31"), by = "1 day"))
#' values = c(seq.int(1, 22, 1))
#' preproc_fill_daily(Date = Dates, value = values)
#' 
preproc_fill_daily <- function(data = NULL,
                               Date,
                               value,
                               POR_start = NA,
                               POR_end = NA,
                               date_format = "%Y-%m-%d"){
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
                     value = value,
                     stringsAsFactors = FALSE)
  }
  checkmate::assert_character(POR_start, max.len = 1)
  checkmate::assert_character(POR_end, max.len = 1)
  
  # POR
  if(is.na(POR_start)){
    POR_start <- min(df$Date)
  }
  if(is.na(POR_end)){
    POR_end <- max(df$Date)
  }
  
  # fill DF with all dates between POR start and end
  fill_dates <- data.frame(Date = seq.Date(from = as.Date(POR_start, format = date_format),
                                           to = as.Date(POR_end, format = date_format),
                                           by = "1 day"))
  
  # join data
  fill_dates <- plyr::join(fill_dates, df, by = "Date")
  
  return(fill_dates)
}
