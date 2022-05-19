#' Title
#'
#' @param select_variables_vec 
#' @param df_num_preds 
#'
#' @return
#' @export
#'
#' @examples
plot_removed_cor <- function(select_variables_vec, df_num_preds) {
  
  removed_variables_vec <- setdiff(names(df_num_preds), select_variables_vec)
  
  cor_matrix <- cor(df_num_preds)
  cor_df <- reshape2::melt(cor_matrix) %>% 
    filter(Var1 %in% removed_variables_vec,
           value != 1)
  
  dups <- duplicated(sort(cor_df$value))
  cor_df <- cor_df[!dups, ] %>% 
    mutate(abs_value = abs(value)) %>% 
    group_by (Var1) %>% 
    slice_max(order_by = abs_value, n = 5)
  
  cor_plot <- ggcharts::bar_chart(cor_df, y = abs_value, x = Var2,
                                  facet = Var1, fill = Var1) +
    labs(y = "Correlation (absolute value)",
         x = "Correlated variables", fill = NULL,
         title = "Top correlates of removed variables") +
    theme_bw() +
    theme(
      strip.background  = element_blank(),
      panel.grid.major = element_line(colour = "grey80"),
      panel.border = element_rect(),
      axis.ticks = element_line(size = 0),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")
  
  return(cor_plot)
  
}