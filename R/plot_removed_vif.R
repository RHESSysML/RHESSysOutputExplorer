#' @title Plot VIF containing removed variables
#' @description Creates a ggplot object showing Variance Inflation Factors (VIF) with groups for variables removed/selected from the random forest model based on multicollinearity
#'
#' @param clim Climate scenario, options are clim=0 or clim=2.
#'
#' @return ggplot object
plot_removed_vif <- function(clim) {
  
  removed_vif_plot <- ggplot(summarize_removed_vars(clim = clim),
                             aes(x = vif, y = reorder(variable, vif), fill = selected)) + 
    geom_col() + labs(x = "Variable Inflation Factor", y = "Variable") +
    theme_light()
  
  return(removed_vif_plot)
  
}