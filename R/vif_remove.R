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
