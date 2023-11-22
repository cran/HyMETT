# preproc_validate_daily
#' Validates that daily data do not contain gaps
#' 
#' @description Validates that daily data do not contain gaps
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
#'
#' @return An error message with missing dates, otherwise nothing.
#' 
#' @details
#' Used to validate there are no gaps in the daily record before computing n-day moving averages in 
#' [`preproc_precondition_data`] or lag-1 autocorrelation in [`POR_calc_AR1`]. If gaps are present,
#' [`preproc_fill_daily`] can be used to fill them with `NA` values.
#' 
#' @export
#' 
#' @keywords preprocessing-data
#' 
#' @examples 
#' preproc_validate_daily(data = example_obs, Date = "Date", value = "streamflow_cfs")
#' 
preproc_validate_daily <- function(data = NULL,
                                   Date = "Date",
                                   value = "value",
                                   date_format = "%Y-%m-%d") {
  # check inputs
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(value)
    checkmate::assert_names(colnames(data), must.include = c(Date, value))
    check_dates <- data[[Date]]
    check_dates <- base::as.Date(check_dates, format = date_format)
  } else {
    # check assertions on inputs and lengths
    Date <- base::as.Date(Date, format = date_format)
    checkmate::assert_date(Date)
    checkmate::assert_numeric(value)
    if(length(Date) != length(value)){
      stop("'Date' and 'value' are unequal lenghts")
    }
    check_dates <- Date
  }
  # check dates against all dates
  all_dates <- seq.Date(from = min(check_dates, na.rm = TRUE), to = max(check_dates, na.rm = T), by = "1 day")
  
  missing <- !(all_dates %in% check_dates)
  
  missing_dates <- all_dates[missing]
  
  if( length(missing_dates) > 0) {
    stop(paste0("Date gaps exist on the following days:\n", paste0(missing_dates, collapse = "\n"),
                "\nUse 'preproc_fill_daily' function to fill gaps with NA values."))
  }
}
