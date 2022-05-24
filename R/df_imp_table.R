#' @title Create dataframe comparing variable importance
#' @description Using two dataframes containing variables and variable importance, returns a `kable` table comparing rank importance between the two.
#'
#' @param imp_wy_df1 Dataframe containing two columns - "Variable" (with variable names) and "Overall" (variable importance derived from `varimp`). 
#' @param imp_wy_df2 Same as `imp_wy_df1` for a second simulation.
#'
#' @return kable table object
df_imp_table <- function(imp_wy_df1, imp_wy_df2) {
  
  df_imp <- imp_wy_df1 %>% 
    full_join(imp_wy_df2, by = "Variable") %>% 
    mutate(Diff = Rank.x-Rank.y) %>% 
    select(-c(Overall.x, Overall.y)) %>% 
    rename("Rank (0)" = Rank.x,
           "Rank (2)" = Rank.y) %>% 
    arrange(`Rank (0)`)
  
  df_imp %>%
    kable(align = "r",
          caption = "Difference in variable importance for the two climate scenarios") %>%
    kable_styling(bootstrap_options = c("striped", "hover"),
                  full_width=F) %>%
    column_spec(column = 4,
                color = ifelse(is.na(df_imp$Diff), "grey",
                               ifelse(df_imp$Diff > 0, "green",
                                      ifelse(df_imp$Diff == 0, "grey", "red"))))
  
}