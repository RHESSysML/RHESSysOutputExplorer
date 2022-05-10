#' @title Summarize correlation of predictor variables
#'
#' @param reduced_df Input dataframe returned after multicollinearity removal step
#' @param selected_removed Logical which determines whether selected or removed variable correlations will be returned
#'
#' @return
#' @export
#'
#' @examples
summarize_cor <- function(reduced_df, selected_removed) {
  
  df_name <- deparse(substitute(reduced_df))
  clim <- gsub("\\D", "", df_name)
  
  num_preds_df <-  get(paste0("df_wy", clim, "_num_preds"))
  cor_matrix <- cor(num_preds_df)
  select_variables <- get(paste0("wy", clim, "_select_variables"))
  
  cor_df <- reshape2::melt(cor_matrix)
  cor_df <- cor_df %>% 
    filter(Var1 != Var2)
  dups <- duplicated(sort(cor_df$value))
  cor_df <- cor_df[!dups, ] %>% 
    rename("correlation" = "value") %>% 
    mutate(var1_selected = case_when(Var1 %in% select_variables ~ "selected",
                                     !Var1 %in% select_variables ~ "removed")) 
  
  if (selected_removed == "selected") {
    cor_df <- cor_df %>% 
      filter(var1_selected == "selected")
  }
  
  if (selected_removed == "removed") {
    cor_df <- cor_df %>% 
      filter(var1_selected == "removed")
  }
  
  cor_df <- cor_df[order(cor_df$Var1), ]
  
  cor_table <- cor_df %>%
    mutate_if(is.numeric, round, 4) %>% 
    datatable(options = list())
  
  return(cor_table)
  
}