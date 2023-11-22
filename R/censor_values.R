# censor_values
#' Censor values above or below a threshold
#' 
#' @description Replaces values in a vector with `NA` when above or below a censor level.\cr
#'              Censoring is `values censor_symbol censor_threshold` are censored, 
#'              for example with the defaults (values lte 0 set to `NA`) all values <= 0 are 
#'              replaced with `NA`.
#' @param value 'numeric' vector. Values to censor.
#' @param censor_threshold 'numeric' value.
#'   Threshold to censor values on. Default is 0.
#' @param censor_symbol 'character' string.\cr
#'                       Inequality symbol to censor values based on censor_threshold.\cr
#'                       Accepted values are `"gt"` (greater than),\cr
#'                                            `"gte"` (greater than or equal to),\cr
#'                                            `"lt"` (less than),\cr 
#'                                         or `"lte"` (less than or equal to).\cr
#'                       Default is `"lte"`.
#'                       
#' @return 'numeric' vector with censored values replaced with `NA`
#' 
#' @export
#' 
#' @keywords preprocessing-data
#' 
#' @examples
#' censor_values(value = seq.int(1, 10, 1), censor_threshold = 5)
#'
censor_values <- function(value,
                          censor_threshold = 0,
                          censor_symbol = c("lte", "lt", "gt", "gte")){
  
  # check assertions
  checkmate::assert_numeric(value)
  checkmate::assert_number(censor_threshold)
  censor_symbol <- match.arg(censor_symbol)
  
  # censor values
  if(censor_symbol %in% c("gt","gte","lt","lte")){
    if(censor_symbol == "gt"){ 
      value[value > censor_threshold] <- NA_real_
    }
    if(censor_symbol == "gte"){ 
      value[value >= censor_threshold] <- NA_real_
    }
    if(censor_symbol == "lt"){ 
      value[value < censor_threshold] <- NA_real_ 
    }
    if(censor_symbol == "lte"){ 
      value[value <= censor_threshold] <- NA_real_
    }
  } else {
    warning(paste0("Unrecognized censor_symbol = '", censor_symbol,
                   "'. Defaulting to 'lte' (less than or equal to)"))
    value[value <= censor_threshold] <- NA_real_
  }
  
  return(value)
}
