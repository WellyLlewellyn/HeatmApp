#' Use Metric
#'
#' Used Percentage Metrics from stored Data. Dataframe must include VolumePerc,AreaPerc,HeightPerc columns. If HeightPerc is not available. Use Stencil Thickness
#'
#' @param Data = Database to calculate metrics
#'
#' @export UseMetrics

UseMetrics <- function(Data){
   
   dataEdit <- Data
   dataEdit$Height <- dataEdit$HeightPerc
   dataEdit$Area <- dataEdit$AreaPerc
   dataEdit$Volume <- dataEdit$VolumePerc
   
   dataEdit <- CalcMovingMetrics(dataEdit)
   
   return(dataEdit)
}
