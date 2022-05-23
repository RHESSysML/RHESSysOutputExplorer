#' @title Remove highly correlated predictor variables
#' @description The purpose of this function is to remove predictor variables that are highly correlated with other predictor variables. Correlation values for all variable combinations are calculated, correlation values are summed for each variable, the variable with the greatest sum of correlation values is removed, and the process repeats until all correlation values are below the given threshold.
#'
#' @param df dataframe containing all numeric predictors
#' @param cor_threshold desired limit for correlation between predictors
#'
#' @return character vector containing names of variables selected to remain in dataset
#'
cor_remove <- function(df, cor_threshold = 0.75) {
  
  # Select only numeric columns
  non_nums <- colnames(df)[!sapply(df, is.numeric)]
  if (length(non_nums) > 0) {
    warning("Removing non-numeric columns: ", paste(non_nums, collapse = ", "))
    df <- df[, !(colnames(df) %in% non_nums)]
  }
  
  # Check that the data frame contains numeric columns
  if (length(df) == 0) {
    stop("df must contain numeric columns.")
  }
  
  threshold <- cor_threshold
  
  cor <- abs(cor(df)) %>% 
    reshape2::melt() %>% 
    filter(Var1 != Var2)
  
  # Iteratively calculate variable correlations until all are below threshold
  while (max(cor$value >= threshold)) {
    
    cor <- abs(cor(df)) %>% 
      reshape2::melt() %>% 
      filter(Var1 != Var2)
    
    dups <- duplicated(sort(cor$value))
    cor <- cor[!dups, ]
    
    cor_sums <- cor %>% 
      group_by(Var2) %>% 
      mutate(sum = sum(value))
    
    # Variable with highest sum of correlation values is removed
    top_cor <- cor_sums %>% 
      filter(sum == max(cor_sums$sum))
    
    drop_var <- as.character(unique(top_cor$Var2))
    
    df <- df %>% 
      select(-all_of(drop_var))
    
  }
  
  return(names(df))
  
}



