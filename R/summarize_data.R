#' @title Summarize a Data Frame
#' @description Creates a datatable object that summarizes any data table, providing information including variable name, count of observations, mean, standard deviation, min, max, range, and class
#'
#' @param df Data frame object
#'
#' @return DT::datatable object
summarize_data <- function(df) {
  df_name <- deparse(substitute(df))
  describe(df) %>% 
    select(vars, n, mean, sd, min, max, range) %>% 
    mutate(class = lapply(df, class)) %>% 
    mutate_if(is.numeric, round, 4) %>% 
    DT::datatable(options = list(pageLength = 7),
                  caption = paste0("Summary of ", df_name))
}