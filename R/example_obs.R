#' Example Observations
#'
#' An example dataset with daily observed streamflow.
#'
#' @format A data.frame with the following variables:
#'   \describe{
#'     \item{`date`}{date as 'character' column class.}
#'     \item{`streamflow_cfs`}{observed streamflow in units of feet^3/second.}
#'     \item{`quality_cd`}{qualifier for value in `streamflow_cfs` (U.S. Geological Survey, 2020b)}
#'     \item{`Date`}{date as 'Date' column class.}
#'   }
#'
#' @details 
#' Generated from example data available at `system.file("extdata", "01013500_OBS.csv", package = "HyMETT")`
#' 
#' @references
#' De Cicco, L.A., Hirsch, R.M., Lorenz, D., and Watkins, W.D., 2021, dataRetrieval: R packages for 
#' discovering and retrieving water data available from Federal hydrologic web services, accessed 
#' September 16, 2020 at https://doi.org/10.5066/P9X4L3GE.
#' 
#' U.S. Geological Survey, 2020a, USGS water data for the Nation: U.S. Geological Survey National 
#' Water Information System database, accessed September 16, 2020, at\cr
#' https://doi.org/10.5066/F7P55KJN.
#' 
#' U.S. Geological Survey, 2020b, Instantaneous and Daily Data-Value Qualification Codes, in USGS 
#' water data for the Nation: U.S. Geological Survey National Water Information System database, 
#' accessed September 16, 2020, at https://doi.org/10.5066/F7P55KJN. 
#' \[information directly accessible at 
#' https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd.\]
#' 
#' @keywords datasets
#'
#' @examples
#' str(example_obs)
#'
"example_obs"
