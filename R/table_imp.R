#' @title Create dataframe of variable importance
#' @description Using a dataframe containing variables and variable importance, returns a `kable` table with rank importance.
#'
#' @param imp Dataframe containing variable importance derived from `varimp`. 
#'
#' @return kable table object
table_imp <- function(imp) {
  
  df_imp <- imp %>% 
    select(-Overall) %>% 
    arrange(Rank)
  
  df_imp %>%
    kable(align = "r") %>%
    kable_styling(bootstrap_options = c("striped", "hover"),
                  full_width=F)
  
}