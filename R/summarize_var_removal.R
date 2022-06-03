#'@title Summarize variable removal process
#'
#'@description Given "reduced" dataframe of predictor variables, returns a dataframe or formatted datatable with summary information associated with multicollinearity removal
#'
#' @param input_df Input dataframe containing numeric predictor variables
#' @param table Logical which determines whether output will be a formatted datatable
#'
#' @return dataframe or datatable containing the following values for all predictor variables: removed/selected status, importance rank, and VIF
#' @export
#'
summarize_var_removal <- function(df, select_variables, prelim_imp, table = TRUE) {
  
  
  removed_importance <- prelim_imp$finalModel["importance"] %>% 
    data.frame() %>% 
    rownames_to_column("variable") %>%
    mutate("importance_rank" = rank(-importance..IncMSE)) %>% 
    select(!c(importance.IncNodePurity, importance..IncMSE)) %>% 
    mutate(selected = case_when(variable %in% select_variables ~ "selected",
                                !variable %in% select_variables ~ "removed")) %>% 
    relocate("selected", .after = "variable")
  
  # Calculating vif again - auto_vif() only returns values for selected variables
  # First removing perfectly collinear variables (aliases) which break vif()
  df_num <- df %>% 
    select(where(is.numeric))
  model <- lm(response ~ ., df_num)
  aliases <- attributes(alias(model)$Complete)$dimnames[[1]]
  df_num_preds <- df_num %>% 
    select(!response & !aliases)
  
  removed_vif <- vif(df_num_preds)
  
  # joining dfs to create summary table of removed and selected variables
  removed_summary <- removed_importance %>% 
    left_join(removed_vif, by = "variable") %>% 
    filter(!(variable %in% factor_vars))  %>% 
    rename("Preliminary importance rank" = importance_rank,
           Variable = variable,
           VIF = vif,
           "Selected Status" = selected)
  
  
  if (table == TRUE) {
    
    removed_summary <- removed_summary %>% 
      datatable()
    
  }
  
  return(removed_summary)
  
}

