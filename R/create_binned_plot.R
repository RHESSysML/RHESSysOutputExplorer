#' @title Create plot of top two important predictors
#' @description Given an input dataframe containing values for the response variable, the variables deemed to be the two most important predictors, and a column labeling the second most important predictor values by quartile called `bins`. This returned dataframe is intended to be input for the function: `create_binned_plot()`.
#' 
#' @param df_binned Dataframe with response term, top two important predictors, and quartile labels for predictor two
#'
#' @return ggplot object
create_binned_plot <- function(df_binned) {
  
  df_name <- deparse(substitute(df_binned))
  clim_id <- str_extract(df_name, pattern = "\\d")
  
  if (!is.na(clim_id)) {
    pred1 <- get(paste0("pred1_clim", clim_id))
    pred2 <- get(paste0("pred2_clim", clim_id))
    
    title <- paste(pred1, "vs", response_var, "given different value classes of",
                   pred2, "\n in", clim_id, "degree climate scenario")
  }
  
  else {
    title <- paste(pred1, "vs", response_var, "given different value classes of",
                   pred2)
  }
  
  ggplot(df_binned, aes(x = pred1, y = .data[["response"]])) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap("bin") +
    labs(x = str_to_title(pred1), y = str_to_title(response_var),
         title = title) +
    theme_minimal()
}