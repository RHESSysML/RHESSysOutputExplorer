#' @title Remove multicollinearity using VIF
#' @description Finds and removes variables with high Variance Inflation Factor (VIF) based on a user-specified preference order. Essentially just a wrapper around Blas Benito's auto_vif function. More information can be found here: https://github.com/BlasBenito/spatialRF/blob/main/R/auto_vif.R
#'
#' @param predictors.df A data frame containing predictor variables
#' @param vif.threshold Numeric greater than or equal to 1 representing maximum VIF for any selected variables. Default is 5
#' @param preference.order Character vector indicating order of preference to keep variables.
#'
#' @return
#' @export
#'
#' @examples
remove_vif <- function(predictors.df, vif.threshold = 5, preference.order = pref_order0) {
  if (!(class(vif.threshold) %in% c("numeric"))) {
    stop("VIF threshold must be numeric.")
  }
  if (vif.threshold < 1) {
    stop("VIF threshold must be greater than or equal to 1.")
  }
  
  variable.selection <- auto_vif(x = predictors.df,
                                 vif.threshold = vif.threshold,
                                 preference.order = preference.order) 
  
  return(variable.selection)
}