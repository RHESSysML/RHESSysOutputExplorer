full_name_units <- function(variable, metadata_df, units = TRUE) {
  
  if (as.character(variable) %in% metadata_df$variable) {
  
    if (units) {
      
      name <-    paste(metadata_df[metadata_df$variable == variable, ]$full_name,
                       metadata_df[metadata_df$variable == variable, ]$units)
      
    } else {
      
      name <- metadata_df[metadata_df$variable == variable, ]$full_name
      
    }
    
    return(name)
    
  } else {
    
    name <- as.character(variable)
    
    return(as.character(name))
    
  }
  
}