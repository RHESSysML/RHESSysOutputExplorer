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