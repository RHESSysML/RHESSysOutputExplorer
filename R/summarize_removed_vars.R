#'@title Summarize removed variables
#'
#'@description Given "reduced" dataframe of predictor variables, returns a dataframe or formatted datatable with summary information associated with multicollinearity removal
#'
#' @param reduced_df Input dataframe returned after multicollinearity removal step
#' @param table Logical which determines whether output will be a formatted datatable
#'
#' @return dataframe or datatable containing the following values for all predictor variables: removed/selected status, importance rank, and VIF
#' @export
#'
summarize_removed_vars <- function(reduced_df, table = FALSE, plot_prep = FALSE) {
  
  if (plot_prep == TRUE) {
    df_name <- as.character(reduced_df)
  } 
  else {
    df_name <- deparse(substitute(reduced_df))
  }
  df_id <- str_extract(df_name, pattern = "wy\\d?")
  clim <- gsub("\\D", "", df_name)
  
  select_variables <- get(paste0(df_id, "_select_variables"))
  imp <- get(paste0("imp", clim))
  df <- get(paste0("df_wy", clim))
  all_preds.df <- df %>% 
    select(!response)
  
  
  removed_importance <- imp$finalModel["importance"] %>% 
    data.frame() %>% 
    rownames_to_column("variable") %>%
    rename("importance" = "importance..IncMSE") %>% 
    select(!importance.IncNodePurity) %>% 
    mutate("importance_rank" = rank(-importance)) %>% 
    mutate(selected = case_when(variable %in% select_variables ~ "selected",
                                !variable %in% select_variables ~ "removed")) %>% 
    relocate("selected", .after = "variable")
  
  
  # Calculating vif again - auto_vif() only returns values for selected variables
  removed_vif <- vif(all_preds.df)
  
  # joining dfs to create summary table of removed and selected variables
  removed_summary <- removed_importance %>% 
    left_join(removed_vif, by = "variable") %>% 
    filter(!(variable %in% factor_vars))
  
  if (table == TRUE) {
    
    removed_summary <- removed_summary %>% 
      datatable()
    
  }
  
  return(removed_summary)
  
}