#' @title Removes variables with high Variance Inflation Factors (VIF)
#' @description The purpose of this function is to remove multicollinearity from a dataset. This is achieved through the following steps: 1. Calculate VIF using a linear model of all numeric predictors.
#'                   2. Finds the highest VIF value. If greater than the input threshold, that variable is removed.
#'                   3. Calculate VIF using a linear model of predictors, with the variable from the previous step removed.
#'                   4. Repeat steps 2-3 until all VIF values are below the input threshold.
#'
#' @param df Dataframe: Contains numeric response variable and numeric predictors. Non-numeric predictors are removed.
#' @param response_col String: Name of response column within df. Default: "response".
#' @param vif_threshold Numeric: Defines the selection threshold. Higher values result in fewer variables removed and more potential for multicollinearity. Default: 5.
vif_remove <- function(df, response_col = "response", vif_threshold = 5) {

  # Select only numeric columns
  non_nums <- colnames(df)[!sapply(df, is.numeric)]
  if (length(non_nums) > 0) {
    warning("Removing non-numeric columns: ", paste(non_nums, collapse = ", "))
    df <- df[, !(colnames(df) %in% non_nums)]
  }

  # Check that the data frame contains numeric columns
  if (length(df) == 0) {
    stop("df must contain numeric columns.")
  }

  # Rename response column to be used in linear model
  df %>% dplyr::rename(response = tidyselect::all_of(response_col))

  # Remove variable with highest VIF and recalculate until all variables are under the threshold
  vif_check <- FALSE
  while (vif_check == FALSE) {
    model <- stats::lm(response ~ ., data = df)

    vif <- car::vif(model)
    highest_vif <- (vif %>% sort(decreasing = TRUE))[1]

    if (highest_vif > vif_threshold) {
      df <- df %>% dplyr::select(-names(highest_vif))
    } else if (highest_vif < vif_threshold) {
      vif_check <- TRUE
    }
  }

  return(colnames(df))
}
