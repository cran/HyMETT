# calc_annual_flow_stats
#' Calculate annual flow statistics from daily data
#' 
#' @description Calculate annual flow statistics from daily data
#' 
#' @param data 'data.frame'. Optional data.frame input, with columns containing `Date`,\cr 
#'   `year_group`, `Q`, and `Q3, Q7, Q30, jd` (if required). Column names are specified as strings 
#'   in the corresponding parameter. Default is `NULL`.
#' @param Date 'Date' or 'character' vector when `data = NULL`, or character' string identifying 
#'   Date column name when `data` is specified. Date associated with each value in `Q` parameter.
#' @param year_group 'numeric' vector when `data = NULL`, or 'character' string identifying 
#'   grouping column name when `data` is specified. Year grouping for each daily value in `Q` 
#'   parameter. Must be same length as `Q` parameter. Often `year_group` is water year or climate 
#'   year.
#' @param Q 'numeric' vector when `data = NULL`, or 'character' string identifying streamflow values 
#'   column name when `data` is specified. Daily streamflow data. Must be same length as `year_group`.
#' @param Q3 'numeric' vector when `data = NULL`, or 'character' string identifying Q3 column name 
#'   when `data` is specified. 3-day moving average of daily streamflow data `Q` parameter, often 
#'   returned from [`preproc_precondition_data`]. Default is `NA_real_`, required if `calc_high` or 
#'   `calc_low = TRUE`. If specified, must be same length as `Q` parameter.
#' @param Q7 'numeric' vector when `data = NULL`, or 'character' string identifying Q7 column name 
#'   when `data` is specified. 7-day moving average of daily streamflow data `Q` parameter, often 
#'   returned from [`preproc_precondition_data`]. Default is `NA_real_`, required if `calc_high` or 
#'   `calc_low = TRUE`. If specified, must be same length as `Q` parameter.
#' @param Q30 'numeric' vector when `data = NULL`, or 'character' string identifying Q30 column name 
#'   when `data` is specified. 30-day average of daily streamflow data `Q` parameter, often returned 
#'   from [`preproc_precondition_data`]. Default is `NA_real_`, required if `calc_high` or 
#'   `calc_low = TRUE`. If specified, must be same length as `Q` parameter.
#' @param jd 'numeric' vector when `data = NULL`, or 'character' string identifying jd column name 
#'   when `data` is specified. Calendar Julian day of daily streamflow data `Q` parameter, often 
#'   returned from [`preproc_precondition_data`]. Default is `NA_integer_`, required if `calc_high`, 
#'   `calc_low`, `calc_WSCVD` or `calc_ICVD = TRUE`. If specified, must be same length as `Q` 
#'   parameter.
#' @param calc_high 'boolean' value. Calculate high flow statistics for years in `year_group`. 
#'   Default is `FALSE`. See **Details** for more information.
#' @param calc_low 'boolean' value. Calculate low flow statistics for years in `year_group`. 
#'   Default is `FALSE`. See **Details** for more information.
#' @param calc_percentiles 'boolean' value. Calculate percentiles for years in `year_group`. 
#'   Default is `FALSE`. See **Details** for more information.
#' @param calc_monthly 'boolean' value. Calculate monthly statistics for years in `year_group`. 
#'   Default is `FALSE`. See **Details** for more information.
#' @param calc_WSCVD 'boolean' value. Calculate winter-spring center volume date for years in 
#'   `year_group`. Default is `FALSE`. See **Details** for more information.
#' @param longitude 'numeric' value. Site longitude in North American Datum of 1983 (NAD83), 
#' required in WSCVD calculation. Default is `NA`. See **Details** for more information.
#' @param calc_ICVD 'boolean' value. Calculate inverse center volume date for years in `year_group`. 
#'   Default is `FALSE`. See **Details** for more information.
#' @param zero_threshold 'numeric' value as percentage. The percentage of years of a statistic that 
#'   need to be zero in order for it to be deemed a zero flow site for that statistic. For use in 
#'   trend calculation. See **Details** on attributes. Default is `33` (33 percent) of the 
#'   annual statistic values.
#' @param quantile_type 'numeric' value. The distribution type used in the [`stats::quantile`] 
#'   function. Default is `8` (median-unbiased regardless of distribution). Other 
#'   types common in hydrology are `6` (Weibull) or `9` (unbiased for normal distributions).
#' @param na.action 'character' string indicating na.action passed to [`stats::aggregate`] 
#'   `na.action` parameter. Default is `"na.omit"`, which removes `NA` values before aggregating 
#'   statistics, or `"na.pass"`, which will pass `NA` values and return `NA` in the grouped calculation 
#'   if any `NA` values are present.
#'
#' @return A tibble (see [`tibble::tibble`]) with annual statistics depending on options selected. 
#' See **Details**.
#' 
#' @details 
#' `year_group` is commonly water year, climate year, or calendar year.\cr
#' 
#' Default annual statistics returned: 
#' \describe{
#'   \item{`annual_mean`}{annual mean in `year_group`}
#'   \item{`annual_sd`}{annual standard deviation in `year_group`}
#'   \item{`annual_sum`}{annual sum in `year_group`}
#' }
#' 
#' If `calc_high/low` are selected, annual statistics returned:\cr
#' 1-, 3-, 7-, and 30-day high/low and Julian date (jd) of n-day high/low.
#' \describe{
#'   \item{`high_q`*n*}{where *n* = 1, 3, 7, and 30}
#'   \item{`high_q`*n*`_jd`}{where *n* = 1, 3, 7, and 30}
#'   \item{`low_q`*n*}{where *n* = 1, 3, 7, and 30}
#'   \item{`low_q`*n*`_jd`}{where *n* = 1, 3, 7, and 30}
#' }
#' 
#' If `calc_percentiles` is selected, annual statistics returned:\cr
#' 1, 5, 10, 25, 50, 75, 90, 95, 99 percentile based on daily streamflow.
#' \describe{
#'   \item{`annual_`*n*`_percentile`}{where *n* = 1, 5, 10, 25, 50, 75, 90, 95, and 99}
#' }
#' 
#' If `calc_monthly` is selected, annual statistics returned:\cr
#' Monthly mean, standard deviation, max, min, percent of annual for each month in `year_group`.
#' \describe{
#'   \item{*month*`_mean`}{monthly mean, where *month* = [`month.abb`]}
#'   \item{*month*`_sd`}{monthly standard deviation, where *month* = [`month.abb`]}
#'   \item{*month*`_max`}{monthly maximum, where *month* = [`month.abb`]}
#'   \item{*month*`_min`}{monthly minimum, where *month* = [`month.abb`]}
#'   \item{*month*`_percent_annual`}{monthly percent of annual, where *month* = [`month.abb`]}
#' }
#' 
#' If `calc_WSCVD` is selected, Julian date of annual winter-spring center volume date is returned.\cr
#' Longitude (in NAD83 datum) is used to determine the ending month of spring. July for longitudes 
#' West of \eqn{-}95 degrees, May for longitudes east of \eqn{-}95 degrees. See **References** 
#' Dudley and others, 2017. Commonly calculated when `year_group` is water year. 
#' \describe{
#'   \item{`WSCVD`}{Julian date of winter-spring center volume}
#' }
#' 
#' If `calc_ICVD` is selected, Julian date of annual inverse center volume date is returned.\cr
#' Commonly calculated when `year_group` is climate year.
#' \describe{
#'   \item{`ICVD`}{Julian date of inverse center volume date}
#' }
#' 
#' **Attribute:** `zero_flow_years`\cr
#' A data.frame with each annual statistic calculated, the percentage of years where the 
#' statistic = 0, a flag indicating if the percentage is over the `zero_threshold` parameter,
#' and the number of years with a zero value. Columns in `zero_flow_years`:
#' \describe{
#'   \item{`annual_stat`}{annual statistic}
#'   \item{`percent_zeros`}{percentage of years with 0 statistic value}
#'   \item{`over_threshold`}{boolean if percentage is over threshold}
#'   \item{`number_years`}{number of years with 0 value statistic}
#' }
#' The `zero_flow_years` attribute can be useful in trend calculation, where a trend may not be 
#' appropriate to calculate with many zero flow years.
#' 
#' @importFrom rlang :=
#' @export
#'
#' @seealso \code{\link{preproc_precondition_data}}
#'
#' @keywords annual-statistics
#'
#' @references 
#' Dudley, R.W., Hodgkins, G.A, McHale, M.R., Kolian, M.J., Renard, B., 2017, Trends in 
#' snowmelt-related streamflow timing in the conterminous United States: Journal of Hydrology, v. 
#' 547, p. 208-221. \[Also available at https://doi.org/10.1016/j.jhydrol.2017.01.051.\]
#'
#' @examples 
#' calc_annual_flow_stats(data = example_preproc, Date = "Date", year_group = "WY", Q = "value")
#' 
calc_annual_flow_stats <- function(data = NULL,
                                   Date,
                                   year_group,
                                   Q,
                                   Q3 = NA_real_,
                                   Q7 = NA_real_,
                                   Q30 = NA_real_,
                                   jd = NA_integer_,
                                   calc_high = FALSE,
                                   calc_low = FALSE,
                                   calc_percentiles = FALSE,
                                   calc_monthly = FALSE,
                                   calc_WSCVD = FALSE,
                                   longitude = NA,
                                   calc_ICVD = FALSE,
                                   zero_threshold = 33,
                                   quantile_type = 8,
                                   na.action = c("na.omit", "na.pass")){
  ###################################################################################################
  # check assertions
  checkmate::assert_choice(calc_high, choices = c(TRUE, FALSE))
  checkmate::assert_choice(calc_low, choices = c(TRUE, FALSE))
  checkmate::assert_choice(calc_percentiles, choices = c(TRUE, FALSE))
  checkmate::assert_choice(calc_monthly, choices = c(TRUE, FALSE))
  checkmate::assert_choice(calc_WSCVD, choices = c(TRUE, FALSE))
  checkmate::assert_number(longitude, lower = -180, upper = 0, na.ok = TRUE)
  if(calc_WSCVD == TRUE ){
    if((length(jd) == 1 && is.na(jd)) || 
       is.na(longitude)){
      stop("'jd' and 'longitude' must be specified if calc_WSCVD = TRUE")
    }
  }
  checkmate::assert_choice(calc_ICVD, choices = c(TRUE, FALSE))
  if(calc_ICVD == TRUE && (length(jd) == 1 && is.na(jd))){
    stop("'jd' must be specified if calc_ICVD = TRUE")
  }
  checkmate::assert_choice(quantile_type, choices = c(1,2,3,4,5,6,7,8,9))
  na.action <- match.arg(na.action)
  # checks if data specified
  if( !is.null(data) ){
    checkmate::assert_data_frame(data)
    checkmate::assert_string(Date)
    checkmate::assert_string(year_group)
    checkmate::assert_string(Q)
    checkmate::assert_string(Q3, na.ok = TRUE)
    checkmate::assert_string(Q7, na.ok = TRUE)
    checkmate::assert_string(Q30, na.ok = TRUE)
    checkmate::assert_string(jd, na.ok = TRUE)
    cn <- c(Date, year_group, Q, Q3, Q7, Q30, jd)
    cn <- cn[!is.na(cn)]
    checkmate::assert_names(colnames(data), must.include = cn)
  } else {
    # checks if vectors specified, checks lengths, and required inputs
    checkmate::assert_date(Date)
    checkmate::assert_numeric(year_group)
    checkmate::assert_numeric(Q)
    checkmate::assert_numeric(Q3)
    checkmate::assert_numeric(Q7)
    checkmate::assert_numeric(Q30)
    checkmate::assert_numeric(jd)
    if(length(Date) != length(year_group)){
      stop("'Date' and 'year_group' are unequal lenghts")
    }
    if(length(Q) != length(year_group)){
      stop("'Q' and 'year_group' are unequal lengths")
    }
    if(calc_high == TRUE || calc_low == TRUE){
      if(is.null(Q3) || is.null(Q7) || is.null(Q30) || is.null(jd)){
        stop("'Q3','Q7','Q30',and 'jd' must all be specified if calc_high or calc_low is TRUE")
      }
      if(length(Q3) != length(year_group)){
        stop("'Q3' and 'Q/Date' are unequal lengths")
      }
      if(length(Q7) != length(year_group)){
        stop("'Q7' and 'Q/Date' are unequal lengths")
      }
      if(length(Q30) != length(year_group)){
        stop("'Q30' and 'Q/Date' are unequal lengths")
      }
      if(length(jd) != length(year_group)){
        stop("'jd' and 'Q/Date' are unequal lengths")
      }
    }
  }
  ###################################################################################################
  # build dataframe
  if (is.null(data)){
    df = data.frame(Date = Date,
                    year_group = year_group,
                    Q = Q,
                    Q3 = Q3,
                    Q7 = Q7,
                    Q30 = Q30,
                    jd = jd)
  } else {
    df = data.frame(Date = data[[Date]],
                    year_group = data[[year_group]],
                    Q = data[[Q]],
                    Q3 =
                      if(is.na(Q3)){
                        NA_real_
                      } else{
                        data[[Q3]]
                      },
                    Q7 =
                      if(is.na(Q7)){
                        NA_real_
                      } else{
                        data[[Q7]]
                      },
                    Q30 =
                      if(is.na(Q30)){
                        NA_real_
                      } else{
                        data[[Q30]]
                      },
                    jd =
                      if(is.na(jd)){
                        NA_real_
                      } else{
                        data[[jd]]
                      })
  }
  ## check daily data
  diff_in_days = difftime(df$Date[2], df$Date[1], units = "days") 
  if (diff_in_days != 1){
    stop("Flow statistics only available for daily data")
  }
  
  # mean annual flows (year_group basis)
  annual_mean <- stats::aggregate(Q ~ year_group, data = df, mean, na.action = na.action)
  annual_sum <- stats::aggregate(Q ~ year_group, data = df, sum, na.action = na.action)
  
  # sd annual flows (year_group basis)
  annual_sd <- stats::aggregate(Q ~ year_group, data = df, stats::sd, na.action = na.action)
  
  # annual DF for all combined statistics
  annual_df <- annual_mean
  names(annual_df) <- c("year_group", "annual_mean")
  annual_df["annual_sd"] <- annual_sd$Q
  annual_df["annual_sum"] <- annual_sum$Q
  
  ###################################################################################################
  # calc high flow statistics (typically for WY year_group)
  if(calc_high == TRUE){
    # n-day flows
    annual_high_q1 <- stats::aggregate(Q ~ year_group, data = df, max, na.action = na.action) # year_group basis, 1-day high
    annual_high_q3 <- stats::aggregate(Q3 ~ year_group, data = df, max, na.action = na.action) # year_group basis, 3-day high
    annual_high_q7 <- stats::aggregate(Q7 ~ year_group, data = df, max, na.action = na.action) # year_group basis, 7-day high
    annual_high_q30 <- stats::aggregate(Q30 ~ year_group, data = df, max, na.action = na.action) # year_group basis, 30-day high
    names(annual_high_q1) <- c("year_group", "high_q1")
    names(annual_high_q3) <- c("year_group", "high_q3")
    names(annual_high_q7) <- c("year_group", "high_q7")
    names(annual_high_q30) <- c("year_group", "high_q30")
    
    # merge to annual DF
    annual_df <- plyr::join(annual_df, annual_high_q1, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q3, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q7, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q30, by = "year_group")
    
    # get Julian Date of high flows.
    annual_high_q1_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_high_q1, df), mean, na.action = na.action)
    annual_high_q3_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_high_q3, df), mean, na.action = na.action)
    annual_high_q7_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_high_q7, df), mean, na.action = na.action)
    annual_high_q30_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_high_q30, df), mean, na.action = na.action)
    names(annual_high_q1_jd) <- c("year_group", "high_q1_jd")
    names(annual_high_q3_jd) <- c("year_group", "high_q3_jd")
    names(annual_high_q7_jd) <- c("year_group", "high_q7_jd")
    names(annual_high_q30_jd) <- c("year_group", "high_q30_jd")
    
    # merge to annual DF
    annual_df <- plyr::join(annual_df, annual_high_q1_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q3_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q7_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_high_q30_jd, by = "year_group")
    
    rm(annual_mean, annual_sd, annual_high_q1, annual_high_q3, annual_high_q7, 
       annual_high_q30, annual_high_q1_jd, annual_high_q7_jd, annual_high_q30_jd, annual_high_q3_jd)
  }
  ###################################################################################################
  # calc low flow statistics (typical for Climate Year year_group)
  if(calc_low == TRUE){
    annual_low_q7 <- stats::aggregate(Q7 ~ year_group, data = df, min, na.action = na.action) # Climatic Year basis, 7-day low
    annual_low_q30 <- stats::aggregate(Q30 ~ year_group, data = df, min, na.action = na.action) # Climatic Year basis, 30-day low
    annual_low_q1 <- stats::aggregate(Q ~ year_group, data = df, min, na.action = na.action) # Climatic Year basis, 1-day low
    annual_low_q3 <- stats::aggregate(Q3 ~ year_group, data = df, min, na.action = na.action) # Climatic Year basis, 3-day low
    names(annual_low_q7) <- c("year_group", "low_q7")
    names(annual_low_q30) <- c("year_group", "low_q30")
    names(annual_low_q1) <- c("year_group", "low_q1")
    names(annual_low_q3) <- c("year_group", "low_q3")
    annual_df <- plyr::join(annual_df, annual_low_q7, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q30, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q3, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q1, by = "year_group")
    
    # get Julian Date of low flows.
    annual_low_q1_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_low_q1, df), mean, na.action = na.action)
    annual_low_q3_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_low_q3, df), mean, na.action = na.action)
    annual_low_q7_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_low_q7, df), mean, na.action = na.action)
    annual_low_q30_jd <- stats::aggregate(jd ~ year_group, data = merge(annual_low_q30, df), mean, na.action = na.action)  
    names(annual_low_q7_jd) <- c("year_group", "low_q7_jd")
    names(annual_low_q30_jd) <- c("year_group", "low_q30_jd")
    names(annual_low_q1_jd) <- c("year_group", "low_q1_jd")
    names(annual_low_q3_jd) <- c("year_group", "low_q3_jd")
    annual_df <- plyr::join(annual_df, annual_low_q7_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q30_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q3_jd, by = "year_group")
    annual_df <- plyr::join(annual_df, annual_low_q1_jd, by = "year_group")

    rm(annual_low_q1, annual_low_q1_jd, annual_low_q3, annual_low_q3_jd, annual_low_q7, annual_low_q7_jd,
       annual_low_q30, annual_low_q30_jd)
  }
  ###################################################################################################
  if(calc_percentiles == TRUE){
    # Pull quantile flows. 
    percentiles <- c(1,5,10,25,50,75,90,95,99)
    for( percentile in percentiles){
      percentile_name <- paste0("annual_",percentile,"_percentile")
      x <- data.frame(year_group = annual_df$year_group)
      for(year in x$year_group){
        q_sub <- df[df$year_group == year,]
        x$Qp[x$year_group == year] <- stats::quantile(q_sub$Q, probs = (percentile / 100), type = quantile_type, na.rm = TRUE)
        # see Hyndman and Fan (1996) and also https://robjhyndman.com/hyndsight/sample-quantiles-20-years-later/
      }
      annual_df[percentile_name] <- x$Qp
      rm(x, q_sub, percentile_name)
    }
    rm(percentile, percentiles)
  }
  ###################################################################################################
  if(calc_monthly == TRUE){
    # Monthly flows
    df$month <- lubridate::month(df$Date)
    for(month in 1:12){
      q_sub <- df[df$month == month,]
      if(nrow(q_sub) == 0 || all(is.na(q_sub$Q))){
        annual_df[paste0(month.abb[month],"_mean")] <- NA_real_
        annual_df[paste0(month.abb[month],"_sd")] <- NA_real_
        annual_df[paste0(month.abb[month],"_max")] <- NA_real_
        annual_df[paste0(month.abb[month],"_min")] <- NA_real_
        annual_df[paste0(month.abb[month],"_percent_annual")] <- NA_real_
        next
      }
      # mean
      # annual_df[paste0(month.abb[month],"_mean")] <- aggregate(Q ~ year_group, data = q_sub, mean)[,2]
      monthly_df <- stats::aggregate(Q ~ year_group, data = q_sub, mean, na.action = na.action)
      monthly_df <- dplyr::rename(monthly_df, !!paste0(month.abb[month],"_mean") := Q)
      annual_df <- dplyr::left_join(annual_df, monthly_df, by = 'year_group')
      # sd
      # annual_df[paste0(month.abb[month],"_sd")] <- aggregate(Q ~ year_group, data = q_sub, sd)[,2]
      monthly_df <- stats::aggregate(Q ~ year_group, data = q_sub, stats::sd, na.action = na.action)
      monthly_df <- dplyr::rename(monthly_df, !!paste0(month.abb[month],"_sd") := Q)
      annual_df <- dplyr::left_join(annual_df, monthly_df, by = 'year_group')
      # max
      # annual_df[paste0(month.abb[month],"_max")] <- aggregate(Q ~ year_group, data = q_sub, max)[,2]
      monthly_df <- stats::aggregate(Q ~ year_group, data = q_sub, max, na.action = na.action)
      monthly_df <- dplyr::rename(monthly_df, !!paste0(month.abb[month],"_max") := Q)
      annual_df <- dplyr::left_join(annual_df, monthly_df, by = 'year_group')
      # min
      # annual_df[paste0(month.abb[month],"_min")] <- aggregate(Q ~ year_group, data = q_sub, min)[,2]
      monthly_df <- stats::aggregate(Q ~ year_group, data = q_sub, min, na.action = na.action)
      monthly_df <- dplyr::rename(monthly_df, !!paste0(month.abb[month],"_min") := Q)
      annual_df <- dplyr::left_join(annual_df, monthly_df, by = 'year_group')
      # percent of ann
      month_sum <- stats::aggregate(Q ~ year_group, data = q_sub, sum, na.action = na.action)
      annual_df <- dplyr::left_join(annual_df, month_sum, by = 'year_group')
      annual_df$Q <- (annual_df$Q/annual_df$annual_sum)* 100 # changed this as it was clobbering year group
      annual_df$Q[annual_df$annual_sum == 0] <- 0 # set values that had zero annual flow from NA to 0.
      annual_df <- dplyr::rename(annual_df, !!paste0(month.abb[month],"_percent_annual") := Q)
      
      rm(q_sub, month_sum, monthly_df)
    }
    rm(month)
  }
  
###################################################################################################  
  # Winter-Spring Centroid Volume date
  if(calc_WSCVD == TRUE){
    # end month by longitude
    end_month <- ifelse(longitude < -95, 7, 5)
    # subset data on winter-spring months
    df_ws <- df[df$month <= end_month,]
    # compute 1/2 runoff volumes
    x <- stats::aggregate(Q ~ year_group, data = df_ws, sum)
    
    # Get WSV 
    # annual_df['WSV'] <- x$Q * (60*60*24)
    wsv_df <- dplyr::rename(x, WSV = Q)
    annual_df <- dplyr::left_join(annual_df, wsv_df, by = 'year_group')
    
    # Get 1/2 runoff volumes
    x$Q <- 0.5 * x$Q
    
    x$wscvd <- NA_real_
    for (year in x$year_group){
      half_volume <- x$Q[x$year_group == year]
      q_sub <- df_ws[df_ws$year_group == year,]
      q_sub$cumulative_sum <- cumsum(q_sub$Q)
      # get the JD of the cumulative_sum nearest to half.vol
      x$wscvd[x$year_group==year] <- q_sub$jd[which.min(abs(q_sub$cumulative_sum - half_volume))]
    }
    x <- x[c('year_group', 'wscvd')]
    # annual_df["WSCVD"] <- x$wscvd
    annual_df <- dplyr::left_join(annual_df, x, by = 'year_group')
    rm(end_month, df_ws, x, half_volume, q_sub, year)
  }
  ###################################################################################################  
  # Inverse-center volume date
  if(calc_ICVD == TRUE){
    # subset data on low flow months
    start_month = 5
    end_month = 11
    df_low <- df[which(df$month >= start_month & df$month <= end_month),]
    # compute invert volumes, note that IQ is inverse streamflow
    df_low$IQ <- 1/(df_low$Q + 0.001) # Adding 0.001 to deal with zero flows
    x <- stats::aggregate(IQ ~ year_group, data = df_low, sum, na.action = na.action)
    
    # Get 1/2 runoff volumes
    x$IQ <- 0.5 * x$IQ
    
    x$icvd <- NA_real_
    for (year in x$year_group){
      half_volume <- x$IQ[which(x$year_group == year)]
      q_sub <- df_low[which(df_low$year_group == year),]
      q_sub$cumulative_sum <- cumsum(q_sub$IQ)
      # get the JD of the cumulative_sum nearest to half.vol
      x$icvd[which(x$year_group == year)] <- q_sub$jd[which.min(abs(q_sub$cumulative_sum - half_volume))]
    }
    annual_df["ICVD"] <- x$icvd
    rm(start_month,end_month,x, q_sub, df_low, year, half_volume)
  }
  ###################################################################################################  
  # Calculate percent zeros.
  zero_df <- as.data.frame(colSums(annual_df == 0))
  names(zero_df) <- "number_years"
  zero_df$annual_stat <- rownames(zero_df)
  zero_df$percent_zeros <- zero_df$number_years/nrow(annual_df)*100
  # Calculate if threshold is met. 
  zero_df$over_threshold <- ifelse(zero_df$percent_zeros >= zero_threshold, TRUE, FALSE)
  zero_df <- zero_df[,c("annual_stat", "percent_zeros", "over_threshold", "number_years")]
  attr(annual_df, "zero_flow_years") <- tibble::as_tibble(zero_df)
  
  ###################################################################################################
  # replace year_group with group name if dataframe input
  if(!is.null(data)) {
    names(annual_df)[names(annual_df) == 'year_group'] <- year_group
  }
  
  annual_df <- tibble::as_tibble(annual_df)
  
  return(annual_df)
}
