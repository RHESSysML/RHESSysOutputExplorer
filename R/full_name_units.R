#' @title Full name units
#' @description Retrieve the full name and the units for a variable from the metadata dataframe. If the input variable isn't in the metadata dataframe, the function will just return the variable name as it appears in the RHESSys output data.
#'
#' @param variable the variable from the RHESSys dataset that you'd like the full name and units for.
#' @param metadata_df the metadata dataframe used in the RHESSysML shiny app.
#' @param units chooses to return the units included or not. Default set to TRUE. Set to FALSE to not return units
#'
#' @return string of variable full name and units.

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