#' @title Get important predictors
#' @description From a data frame containing values of importance obtained from a random forest model, returns the predictor variable with the desired rank of importance
#' 
#' @param df_imp Data frame containing two columns: Variable and Rank
#' @param rank Rank of desired predictor variable to return
#'
#' @return Variable name (str) of the variable at rank = rank
get_important_predictors <- function(df_imp, rank) {
  df_imp$Variable[df_imp$Rank == rank]
}