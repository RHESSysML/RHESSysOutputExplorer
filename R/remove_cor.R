#' @title Remove correlation
#' @description Finds and removes highly correlated variables based on a user-specified preference order. Essentially just a wrapper around Blas Benito's auto_cor function. More information can be found here: https://github.com/BlasBenito/spatialRF/blob/main/R/auto_cor.R
#'
#' @param predictors.df A data frame containing predictor variables.
#' @param vif.threshold Numeric between 0 and 1 representing maximum Pearson correlation between any two selected variables. Default is 0.75.
#' @param preference.order Character vector indicating order of preference to keep variables. 
#'
#' @return List with three items: (1) `vif`: Data frame of selected variables and respective VIF values.
#'                                (2) `selected.variables`: character vector of selected column names.
#'                                (3) `selected.variables.df`: data frame of selected columns.
remove_cor <- function(predictors.df, cor.threshold = 0.75, preference.order = pref_order0) {
  if (!(class(cor.threshold) %in% c("numeric"))) {
    stop("Correlation threshold must be numeric.")
  }
  if(cor.threshold < 0 | cor.threshold > 1) {
    stop("Correlation threshold must be between 0 and 1.")
  }
  
  variable.selection <- auto_cor(x = predictors.df,
                                 cor.threshold = cor.threshold,
                                 preference.order = preference.order) 
  
  return(variable.selection)
}