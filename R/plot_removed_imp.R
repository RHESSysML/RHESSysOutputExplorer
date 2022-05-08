#' @title Plot preliminary importance containing removed variables
#' @description Creates a variable importance plot with groups for variables removed/selected from the random forest model based on multicollinearity
#'
#' @param clim Climate scenario, options are clim=0 or clim=2.
#'
#' @return ggplot object
plot_removed_imp <- function(clim) {
  
  removed_imp_plot <- ggplot(summarize_removed_vars(clim = clim),
                             aes(x = importance, y = reorder(variable, importance), fill = selected)) +
    geom_col() + labs(x = "Preliminary Importance", y = "Variable") +
    theme_light()
  
  return(removed_imp_plot)
  
}