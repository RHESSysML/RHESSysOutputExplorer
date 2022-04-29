#' @title Tune random forest models
#' @description Given an input data frame, returns a vector of MSE values for different possible values of mtry that can be used to determine the best mtry value
#'
#' @param df Input data frame with response variable as the first column
#'
#' @return Vector of MSE values for each value of mtry
tune_rf_model <- function(df) {
  
  # Create tuning vector the length of the number of predictor variables
  tuning <- vector(length = (ncol(df)-1))
  
  x = df[,-1]
  y = df[,1]
  
  # Run a small random forest (ntree=50) for each mtry value
  for (i in 1:length(tuning)) {
    rf_tuning <- randomForest(x, y, mtry = i, ntree = 50)
    tuning[i] <- tail(rf_tuning$mse, 1)
  }
  
  return(tuning)
}