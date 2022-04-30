#' @title Create dataframe of top two important predictors
#' @description Given an input dataframe, returns a dataframe containing values for the response variable, the variables deemed to be the two most important predictors, and a column labeling the second most important predictor values by quartile called `bins`---this returned dataframe is intended to be input for the function: `create_binned_plot()`.
#'
#' @param df Input dataframe with response variable as the first column and all selected predictors
#'
#' @return Dataframe with response term, top two important predictors, and quartile labels for predictor two
create_binned_df <- function(df) {
  df_pred_binned <- df_wy %>% 
    select(all_of(pred1_clim0), all_of(pred2_clim0), response) %>%
    mutate(pred1 = df_wy[[pred1_clim0]],
           pred2 = df_wy[[pred2_clim0]]) %>% 
    mutate(bin = as.numeric(ntile(pred2, 4)))
  
  return(df_pred_binned)
}