plot_removed_vif <- function(reduced_df) {
  
  df_name <- deparse(substitute(reduced_df))
  
  vif_df <- summarize_removed_vars(df_name, plot_prep = TRUE)
  
  removed_vif_plot <- ggplot(vif_df,
                             aes(x = vif, y = reorder(variable, vif), fill = selected)) + 
    geom_col() + labs(x = "Variable Inflation Factor", y = "Variable") +
    theme_light()
  
  return(removed_vif_plot)
  
}