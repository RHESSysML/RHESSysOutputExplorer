#' @title Plot preliminary importance containing removed variables
#' @description Creates a variable importance plot with groups for variables removed/selected from the random forest model based on multicollinearity
#'
#' @param reduced_df Dataframe containing the selected predictor variables
#'
#' @return ggplot object
plot_removed_imp <- function(reduced_df) {

  
  df_name <- deparse(substitute(reduced_df))
  
  imp_df <- summarize_removed_vars(df_name, plot_prep = TRUE)
  
  removed_imp_plot <- ggplot(imp_df,
                             aes(x = importance, y = reorder(variable, importance), fill = selected)) +
    geom_col() + labs(x = "Preliminary Importance", y = "Variable") +
    theme_light()
  
  return(removed_imp_plot)
  
}