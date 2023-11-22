# preproc_main
#' A wrapper function for preproc_precondition_data, preproc_audit_data, and calc_annual_flow_stats
#' 
#' @description A wrapper function for [`preproc_precondition_data`], 
#' [`preproc_audit_data`], and \cr [`calc_annual_flow_stats`]
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date` and `value`. 
#'   Column names are specified as strings in the corresponding parameter. Default is `NULL`.
#' @param Date 'Date' or 'character' vector when `data = NULL`, or 'character' string identifying 
#'   Date column name when `data` is specified. Dates associated with each value in `value` 
#'   parameter.
#' @param value 'numeric' vector when `data = NULL`, or 'character' string identifying year column 
#'   name when `data` is specified. Values to precondition and calculate n-day moving averages 
#'   from. N-day moving averages only calculated for daily data.
#' @param date_format 'character' string. Format of Date. Default is `"%Y-%m-%d"`.
#' @param year_group 'character' value. Specify either `"year"` for calendar year, `"WY"` for water 
#'   year, or `"CY"` for climate year. Used to select data after preconditioning for audit and 
#'   annual statistics. Default is `"WY"`.
#' @param use_specific_years 'boolean' value. Flag to clip data to a certain set of years in 
#'   `year_group`. Default is `FALSE`.
#' @param begin_year 'numeric' value. If `use_specific_years = TRUE`, beginning year to clip 
#'   `value`. Default is `NULL`.
#' @param end_year 'numeric' value. If `use_specific_years = TRUE`, ending year to clip `value`. 
#'   Default is `NULL`.
#' @param days_cutoff 'numeric' value. Designating the number of days required for a year to be 
#'   counted as full. Default is `360`.
#' @param calc_high 'boolean' value. Calculate high streamflow statistics for years in `year_group`. 
#'   Default is `TRUE`. See **Details** for more information.
#' @param calc_low 'boolean' value. Calculate low streamflow statistics for years in `year_group`. 
#'   Default is `TRUE`. See **Details** for more information.
#' @param calc_percentiles 'boolean' value. Calculate percentiles for years in `year_group`. 
#'   Default is `TRUE`. See **Details** for more information.
#' @param calc_monthly 'boolean' value. Calculate monthly statistics for years in `year_group`. 
#'   Default is `TRUE`. See **Details** for more information.
#' @param calc_WSCVD 'boolean' value. Calculate winter-spring center volume date for years in 
#'   `year_group`. Default is `TRUE`. See **Details** for more information.
#' @param longitude 'numeric' value. Site longitude in NAD83, required in WSCVD calculation. 
#'   Default is `NA`. See **Details** for more information.
#' @param calc_ICVD 'boolean' value. Calculate inverse center volume date for years in 
#'   `year_group`. Default is `FALSE`. See **Details** for more information.
#' @param zero_threshold 'numeric' value as percentage. The percentage of years of a statistic that 
#'   need to be zero in order for it to be deemed a zero streamflow site for that statistic. For 
#'   use in trend calculation. See **Details** on attributes. Default is `33` (33 percent) of 
#'   the annual statistic values.
#' @param quantile_type 'numeric' value. The distribution type used in the [`stats::quantile`] 
#'   function. Default is `8` (median-unbiased regardless of distribution). Other 
#'   types common in hydrology are `6` (Weibull) or `9` (unbiased for normal distributions).
#' @param na.action 'character' string indicating na.action passed to [`stats::aggregate`] 
#'   `na.action` parameter. Default is `"na.omit"`, which removes `NA` values before aggregating 
#'   statistics, or `"na.pass"`, which will pass `NA` values and return `NA` in the grouped 
#'   calculation if any `NA` values are present.
#'
#' @return A list of three data.frames: 1 of preconditioned data, 1 data audit, and 1 annual 
#'   statistics.
#' 
#' @details 
#' This is a wrapper function of [`preproc_precondition_data`], [`preproc_audit_data`], and\cr
#' [`calc_annual_flow_stats`]. Data are first passed to the precondition function, then audited, 
#' then annual statistics are computed.\cr
#' It also checks the timestep of the data to make sure that it is daily timestep. 
#' Other time steps are currently not supported and will return the data.frame without moving 
#' averages computed.
#' 
#' @export
#' 
#' @keywords preprocessing-data
#' @keywords annual-statistics
#' 
#' @seealso \code{\link{preproc_audit_data}}, \code{\link{preproc_precondition_data}}, 
#' \code{\link{calc_annual_flow_stats}}
#'
#' @examples
#' preproc_main(data = example_obs, Date = "Date", value = "streamflow_cfs", longitude = -68)
#' 
preproc_main <- function(data = NULL,
                         Date,
                         value,
                         date_format = "%Y-%m-%d",
                         year_group = c("WY", "CY", "year"),
                         use_specific_years = FALSE,
                         begin_year = NULL,
                         end_year = NULL,
                         days_cutoff = 360,
                         calc_high = TRUE,
                         calc_low = TRUE,
                         calc_percentiles = TRUE,
                         calc_monthly = TRUE,
                         calc_WSCVD = TRUE,
                         longitude = NA,
                         calc_ICVD = FALSE,
                         zero_threshold = 33,
                         quantile_type = 8,
                         na.action = c("na.omit", "na.pass")){
  # check assertions that are not already checked by called functions
  year_group <- match.arg(year_group)
  if (!is.null(data)){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(value)
    checkmate::assert_names(colnames(data), must.include = c(Date, value))
  }
  na.action <- match.arg(na.action)
  
  #################################################################################################
  d <- preproc_precondition_data(data = data,
                                 Date = Date,
                                 value = value,
                                 date_format = date_format)
  
  #################################################################################################
  a <- preproc_audit_data(Date = d$Date,
                          value = d$value,
                          year_group = d[[year_group]],
                          use_specific_years = use_specific_years,
                          begin_year = begin_year,
                          end_year = end_year,
                          days_cutoff = days_cutoff)
  if(use_specific_years == TRUE){
    d <- d[d[[year_group]] %in% a$year_group,]
  }
  
  #################################################################################################
  c <- calc_annual_flow_stats(Date = d$Date,
                              year_group = d[[year_group]],
                              Q = d$value,
                              Q3 = d$Q3,
                              Q7 = d$Q7,
                              Q30 = d$Q30,
                              jd = d$jd,
                              calc_high = calc_high,
                              calc_low = calc_low,
                              calc_percentiles = calc_percentiles,
                              calc_monthly = calc_monthly,
                              calc_WSCVD = calc_WSCVD,
                              longitude = longitude,
                              calc_ICVD = calc_ICVD,
                              zero_threshold = zero_threshold,
                              na.action = na.action)
  
  #################################################################################################
  # replace year_group with group name
  names(a)[names(a) == 'year_group'] <- year_group
  names(c)[names(c) == 'year_group'] <- year_group
  
  #################################################################################################
  o <- list(daily = d,
            audit = a,
            annual = c)
  return(o)
}
