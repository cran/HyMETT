#' Example Model Output with zero flows
#'
#' An example dataset with daily modeled (simulated) streamflow that includes zero flows.
#'
#' @format A data.frame with the following variables:
#'   \describe{
#'     \item{`date`}{date as 'character' column class.}
#'     \item{`streamflow_cfs`}{modeled streamflow in units of feet^3/second.}
#'     \item{`Date`}{date as 'Date' column class.}
#'   }
#'
#' @details 
#' Generated from example data available at `system.file("extdata", "08202700_MOD.csv", package = "HyMETT")`
#' 
#' @references 
#' Johnson, M., D. Blodgett, 2020, NOAA National Water Model Reanalysis Data at RENCI, HydroShare, 
#' accessed September 17, 2020 at\cr https://doi.org/10.4211/hs.89b0952512dd4b378dc5be8d2093310f	
#' 
#' Johnson, M., 2021, nwmHistoric: National Water Model Historic Data. R package version 0.0.0.9000,
#' accessed September 17, 2020 at https://github.com/mikejohnson51/nwmHistoric
#' 
#' @keywords datasets
#'
#' @examples
#' str(example_mod_zf)
#'
"example_mod_zf"
