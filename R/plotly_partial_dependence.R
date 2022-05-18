#' @title Create an interactive partial dependence plot using the plot_ly package
#' @description Largely an adaptation of Jeffrey S. Evans' bivariate.partialDependence function from the rfUtilities package, found here: #https://rdrr.io/cran/rfUtilities/src/R/bivariate.partialDependence.R. Provides a visualization of the marginal effect of two variables on the response variable.
#'
#' @param x Random forest or gradient boosting object
#' @param pred.data data.frame of independent variables used in the model
#' @param v1 Variable 1 used in partial dependence plot
#' @param v2 Variable 2 used in partial dependence plot
#' @param grid.size Number of grid cells
#'
#' @return plot_ly visualization
#' 
#' @references Jeffrey S. Evans <jeffrey_evans<at>tnc.org>
#' 
#' @example 
#' plotly_partial_dependence(x = rf_wy2$finalModel, 
#'                           pred.data = df_wy2_reduced[-1], 
#'                           v1 = pred1_clim2, 
#'                           v2 = pred2_clim2, 
#'                           grid.size = 15)
plotly_partial_dependence <- function(x,
                                      pred.data,
                                      v1,
                                      v2,
                                      grid.size) {
  
  s1 <- seq(from = min(pred.data[,v1]), to = max(pred.data[,v1]),
            by = (max(pred.data[,v1]) - min(pred.data[,v1]))/(grid.size-1))
  s2 <- seq(from = min(pred.data[,v2]), to = max(pred.data[,v2]),
            by = (max(pred.data[,v2]) - min(pred.data[,v2]))/(grid.size-1))
  
  v <- expand.grid(s1, s2)
  v <- v[with(v, order(Var1, Var2)),]
  
  vrep <- pred.data[rep(1:nrow(pred.data), nrow(v)),]
  vrep[,v1] <- rep(v$Var1, each = nrow(pred.data))
  vrep[,v2] <- rep(v$Var2, each = nrow(pred.data))
  
  vrep$pred <- stats::predict(x, vrep[,which(names(vrep) %in% rownames(x$importance))])
  
  idx <- sort(rep(1:nrow(v), length.out=nrow(vrep)))   
  idx.med <- as.numeric(tapply(vrep$pred, idx, FUN=stats::median)) 
  z <- matrix(idx.med, nrow = length(s1), byrow = TRUE) 
  
  fig <- plot_ly(x = ~s1, y = ~s2, z = ~z, type = 'surface',
                 hovertemplate = paste0("predicted value: %{z}</b><br><br>",
                                        paste0(v1,": "), "%{x}<br>",
                                        paste0(v2,": "), "%{y}<br>",
                                        "<extra></extra>")) %>% 
    layout(
      scene = list(
        camera = list(eye = list(x = -1.5, y = -1.5, z = 1.25)),
        xaxis = list(title = full_name_units(v1, metadata)), 
        yaxis = list(title = full_name_units(v2, metadata)),
        zaxis = list(title = "Predicted Value")
        ),
      font = list(size = 9)
      )
  
  return(fig)

}
