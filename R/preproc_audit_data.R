# preproc_audit_data
#' Audit daily data for total days in year
#' 
#' @description Audit daily data for total days in year. An audit is performed to inventory and 
#' flag missing days in daily data and help determine if further analyses are appropriate.
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'Date' or 'character' vector when `data = NULL`, or 'character' string identifying 
#'   Date column name when `data` is specified. Dates associated with each value in value parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying year column 
#'   name when `data` is specified. Values to audit, must be daily data.
#' @param year_group 'numeric' vector when `data = NULL`, or 'character' string identifying 
#'   grouping column name when `data` is specified. Year grouping for each daily value in `value` 
#'   parameter. Must be same length as `value`.
#' @param use_specific_years 'boolean' value. Flag to clip data to a certain set of years in 
#'   `year_group`. Default is `FALSE`.
#' @param begin_year 'numeric' value. If `use_specific_years = TRUE`, beginning year to clip value. 
#'   Default is `NULL`.
#' @param end_year 'numeric' value. If `use_specific_years = TRUE`, ending year to clip value. 
#'   Default is `NULL`.
#' @param days_cutoff 'numeric' value. Designating the number of days required for a year to be 
#'   counted as full. Default is `360`. 
#' @param date_format 'character' string. Format of Date. Default is `"%Y-%m-%d"`.
#'                     
#' @return A data.frame with `year_group`, count (n, excluding `NA` values) 
#' of days in each `year_group`, and a complete years 'boolean' flag.
#' 
#' @details 
#' Year grouping is commonly water year, climate year, or calendar year.
#' 
#' @export
#'
#' @keywords preprocessing-data
#'
#' @seealso \code{\link{preproc_fill_daily}}, \code{\link{preproc_precondition_data}}
#'
#' @examples 
#' preproc_audit_data(
#'   data = example_preproc, Date = "Date", value = "value", year_group = "WY"
#' )
#' 
preproc_audit_data <- function(data = NULL,
                               Date,
                               value,
                               year_group,
                               use_specific_years = FALSE,
                               begin_year = NULL,
                               end_year = NULL,
                               days_cutoff = 360,
                               date_format = "%Y-%m-%d"){
  # check inputs
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(value)
    checkmate::assert_string(year_group)
    checkmate::assert_names(colnames(data), must.include = c(Date, value, year_group))
    df <- data.frame(Date = data[[Date]],
                     value = data[[value]],
                     year_group = data[[year_group]],
                     stringsAsFactors = FALSE)
    df$Date <- base::as.Date(df$Date, format = date_format)
  } else {
    # check assertions on inputs and lengths, and daily data
    Date <- base::as.Date(Date, format = date_format)
    checkmate::assert_date(Date)
    checkmate::assert_numeric(value)
    checkmate::assert_numeric(year_group)
    if(length(Date) != length(value)){
      stop("'Date' and 'value' are unequal lenghts")
    }
    if(length(value) != length(year_group)){
      stop("'value' and 'year_type' are unequal lengths")
    }
    ## check daily data
    diff_in_days = difftime(Date[2], Date[1], units = "days") 
    if (diff_in_days != 1){
      warning("Audit only available for daily data")
    }
    # build dataframe
    df = data.frame(Date = Date,
                    value = value,
                    year_group = year_group,
                    stringsAsFactors = FALSE)
    
  }
  checkmate::assert_choice(use_specific_years, choices = c(TRUE, FALSE))
  checkmate::assert_number(begin_year, null.ok = TRUE)
  checkmate::assert_number(end_year, null.ok = TRUE)
  if(use_specific_years == TRUE & is.null(begin_year)){
    stop("Please specify 'begin_year'")
  }
  if(use_specific_years == TRUE & is.null(end_year)){
    stop("Please specify 'end_year'")
  }
  checkmate::assert_number(days_cutoff)
  
  # use specific years
  if(use_specific_years == TRUE){
    df <- df[df$year_group >= begin_year & df$year_group <= end_year,]
  }
  
  df = df[stats::complete.cases(df), ]
  # Count the number of days per year, inventory missing years and count full years by days_cutoff 
  counts <- dplyr::count(df, year_group)
  counts$complete[counts$n < days_cutoff] <- FALSE
  counts$complete[counts$n >= days_cutoff] <- TRUE
  missing_years <- counts$year_group[counts$n < days_cutoff]
  complete_years <- counts$year_group[counts$n >= days_cutoff]
  
  if(length(missing_years) == 0){
    message(paste0("No incomplete years"))
  } else{
    message(paste0(c("These years are incomplete:", missing_years), collapse = " "))
  }
  
  # replace year_group with group name if dataframe input
  if(!is.null(data)) {
    names(counts)[names(counts) == 'year_group'] <- year_group
  }
  
  return(counts)
}
