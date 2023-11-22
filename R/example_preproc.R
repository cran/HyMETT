#' Example Observations prepocessed
#'
#' An example dataset with daily observed streamflow preprocessed to include additional timing and 
#' n-day moving averages.
#'
#' @format A data.frame with the following variables:
#' \describe{
#'   \item{`Date`}{}
#'   \item{`value`}{}
#'   \item{`year`}{}
#'   \item{`month`}{}
#'   \item{`day`}{}
#'   \item{`decimal_date`}{}
#'   \item{`WY`}{Water Year: October 1 - September 30}
#'   \item{`CY`}{Climate Year: April 1 - March 30}
#'   \item{`Q3`}{3-Day Moving Average: computed at end of moving interval}
#'   \item{`Q7`}{7-Day Moving Average: computed at end of moving interval}
#'   \item{`Q30`}{30-Day Moving Average: computed at end of moving interval}
#'   \item{`jd`}{Julian date}
#' }
#'
#' @details 
#' Generated with [`example_obs`] from \preformatted{
#' HyMETT::preproc_main(data = example_obs, 
#'                      Date = "Date", value = "streamflow_cfs", longitude = -68)$daily`}
#' 
#' @keywords datasets
#'
#' @seealso \code{\link{example_obs}}, \code{\link{preproc_main}}
#'
#' @examples
#' str(example_preproc)
#'
"example_preproc"
