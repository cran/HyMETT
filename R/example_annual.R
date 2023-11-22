#' Example Annual Observations 
#'
#' An example dataset with daily observed streamflow processed to annual water year values.
#'
#' @format A data.frame with the following variables:
#' \describe{
#'   \item{`WY`}{water year}
#'   \item{`annual_mean`}{annual mean}
#'   \item{`annual_sd`}{annual standard deviation}
#'   \item{`annual_sum`}{annual sum}
#'   \item{`high_q1`}{annual maximum of daily mean}
#'   \item{`high_q3`}{annual maximum of 3-day mean}
#'   \item{`high_q7`}{annual maximum of 7-day mean}
#'   \item{`high_q30`}{annual maximum of 30-day mean}
#'   \item{`high_q1_jd`}{Julian day of annual maximum of daily mean}
#'   \item{`high_q3_jd`}{Julian day of annual maximum of 3-day mean}
#'   \item{`high_q7_jd`}{Julian day of annual maximum of 7-day mean}
#'   \item{`high_q30_jd`}{Julian day of annual maximum of 30-day mean}
#'   \item{`low_q7`}{annual minimum of 7-day mean}
#'   \item{`low_q30`}{annual minimum of 30-day mean}
#'   \item{`low_q3`}{annual minimum of 3-day mean}
#'   \item{`low_q1`}{annual minimum of daily mean}
#'   \item{`low_q7_jd`}{Julian day of annual minimum of 7-day mean}
#'   \item{`low_q30_jd`}{Julian day of annual minimum of 30-day mean}
#'   \item{`low_q3_jd`}{Julian day of annual minimum of 3-day mean}
#'   \item{`low_q1_jd`}{Julian day of annual minimum of daily mean}
#'   \item{`annual_1_percentile`}{annual first percentile}
#'   \item{`annual_5_percentile`}{annual 5th percentile}
#'   \item{`annual_10_percentile`}{annual 10th percentile}
#'   \item{`annual_25_percentile`}{annual 25th percentile}
#'   \item{`annual_50_percentile`}{annual 50th percentile}
#'   \item{`annual_75_percentile`}{annual 75th percentile}
#'   \item{`annual_90_percentile`}{annual 90th percentile}
#'   \item{`annual_95_percentile`}{annual 95th percentile}
#'   \item{`annual_99_percentile`}{annual 99th percentile}
#'   \item{`Jan_mean`}{annual January mean}
#'   \item{`Jan_sd`}{annual January standard deviation}
#'   \item{`Jan_max`}{annual January maximum}
#'   \item{`Jan_min`}{annual January minimum}
#'   \item{`Jan_percent_annual`}{annual January percentage of annual sum}
#'   \item{`Feb_mean`}{annual February mean}
#'   \item{`Feb_sd`}{annual February standard deviation}
#'   \item{`Feb_max`}{annual February maximum}
#'   \item{`Feb_min`}{annual February minimum}
#'   \item{`Feb_percent_annual`}{annual February percentage of annual sum}
#'   \item{`Mar_mean`}{annual March mean}
#'   \item{`Mar_sd`}{annual March standard deviation}
#'   \item{`Mar_max`}{annual March maximum}
#'   \item{`Mar_min`}{annual March minimum}
#'   \item{`Mar_percent_annual`}{annual March percentage of annual sum}
#'   \item{`Apr_mean`}{annual April mean}
#'   \item{`Apr_sd`}{annual April standard deviation}
#'   \item{`Apr_max`}{annual April maximum}
#'   \item{`Apr_min`}{annual April minimum}
#'   \item{`Apr_percent_annual`}{annual April percentage of annual sum}
#'   \item{`May_mean`}{annual May mean}
#'   \item{`May_sd`}{annual May standard deviation}
#'   \item{`May_max`}{annual May maximum}
#'   \item{`May_min`}{annual May minimum}
#'   \item{`May_percent_annual`}{annual May percentage of annual sum}
#'   \item{`Jun_mean`}{annual June mean}
#'   \item{`Jun_sd`}{annual June standard deviation}
#'   \item{`Jun_max`}{annual June maximum}
#'   \item{`Jun_min`}{annual June minimum}
#'   \item{`Jun_percent_annual`}{annual June percentage of annual sum}
#'   \item{`Jul_mean`}{annual July mean}
#'   \item{`Jul_sd`}{annual July standard deviation}
#'   \item{`Jul_max`}{annual July maximum}
#'   \item{`Jul_min`}{annual July minimum}
#'   \item{`Jul_percent_annual`}{annual July percentage of annual sum}
#'   \item{`Aug_mean`}{annual August mean}
#'   \item{`Aug_sd`}{annual August standard deviation}
#'   \item{`Aug_max`}{annual August maximum}
#'   \item{`Aug_min`}{annual August minimum}
#'   \item{`Aug_percent_annual`}{annual August percentage of annual sum}
#'   \item{`Sep_mean`}{annual September mean}
#'   \item{`Sep_sd`}{annual September standard deviation}
#'   \item{`Sep_max`}{annual September maximum}
#'   \item{`Sep_min`}{annual September minimum}
#'   \item{`Sep_percent_annual`}{annual September percentage of annual sum}
#'   \item{`Oct_mean`}{annual October mean}
#'   \item{`Oct_sd`}{annual October standard deviation}
#'   \item{`Oct_max`}{annual October maximum}
#'   \item{`Oct_min`}{annual October minimum}
#'   \item{`Oct_percent_annual`}{annual October percentage of annual sum}
#'   \item{`Nov_mean`}{annual November mean}
#'   \item{`Nov_sd`}{annual November standard deviation}
#'   \item{`Nov_max`}{annual November maximum}
#'   \item{`Nov_min`}{annual November minimum}
#'   \item{`Nov_percent_annual`}{annual November percentage of annual sum}
#'   \item{`Dec_mean`}{annual December mean}
#'   \item{`Dec_sd`}{annual December standard deviation}
#'   \item{`Dec_max`}{annual December maximum}
#'   \item{`Dec_min`}{annual December minimum}
#'   \item{`Dec_percent_annual`}{annual December percentage of annual sum}
#'   \item{`WSV`}{winter-spring volume}
#'   \item{`wscvd`}{Julian date of winter-spring center volume}
#' }
#'
#' @details 
#' Generated with [`example_obs`] from \preformatted{
#' HyMETT::preproc_main(data = example_obs, 
#'                      Date = "Date", value = "streamflow_cfs", longitude = -68)$annual}
#' 
#' @keywords datasets
#'
#' @seealso \code{\link{example_obs}}, \code{\link{preproc_main}}
#'
#' @examples
#' str(example_annual)
#'
"example_annual"
