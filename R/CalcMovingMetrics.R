#' Calculate Metric
#'
#' Used to calculate Moving Value Metrics. Used for approximate CPk calculations
#'
#' @param data = Database to calculate metrics
#'
#' @export CalcMovingMetrics

CalcMovingMetrics <- function(data){
   
   data$MovingVolume = c(0,diff(data$Volume))
   data$MovingArea = c(0,diff(data$Area))
   data$MovingHeight = c(0,diff(data$Height))
   
   return(data)
}
c