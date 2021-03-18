#' Calculate Metric
#'
#' Used to calculate Percentage Metrics along with the Moving Value between them. Used for approximate CPk calculations. PADs should be consistent
#'
#' @param Data = Database to calculate metrics
#' @param StencilThickness = Stencil Thickness
#' @param SizeX = Longest PAD side
#' @param SizeY = Shortest PAD side
#' @param Shape = Shape of pad S,C,SR,HT,HR,HTc
#' @param HomebaseX  = Longest side of the Homebase
#' @param HomebaseY = Shortest side of the Homebase
#' @param Corners = Radius of any corners
#'
#' @export CalcMetrics

CalcMetrics <- function(Data,StencilThickness,SizeX,SizeY,Shape,HomebaseX=NULL,HomebaseY=NULL,Corners=NULL){
  
 dataEdit <- Data
  #CalcAreaSize(Data,StencilThickness,SizeX,SizeY,Shape,HomebaseX,HomebaseY,Corners)
  
  if(Shape == "S"){dataEdit$AreaSize <- (1)*(SizeX*SizeY)
  }  else if(Shape == "C"){dataEdit$AreaSize <- (1)*(pi*((SizeX/2)^2))
  }  else if(Shape == "HT"){dataEdit$AreaSize <- (1)*((SizeX*SizeY)+((HomebaseX*HomebaseY)/2))
  }  else if(Shape == "HR"){dataEdit$AreaSize <- (1)*((SizeX*SizeY)+(pi*((HomebaseX/2)*HomebaseY)/2))
  }  else if(Shape == "HTc"){dataEdit$AreaSize <- (1)*((SizeX*SizeY)+(pi*(HomebaseX*HomebaseY)/2)-Corners)
  }  else if(Shape == "SR"){dataEdit$AreaSize <- (1)*((SizeX*SizeY)-(Corners^2*(4-pi)))}

  dataEdit$Height = 100*(dataEdit$RealHeight/StencilThickness)
  dataEdit$Area = 100*(dataEdit$RealArea/dataEdit$AreaSize)
  dataEdit$Volume = 100*(dataEdit$RealVolume/(dataEdit$AreaSize*StencilThickness))
  
  dataEdit <- CalcMovingMetrics(dataEdit)
  
  return(dataEdit)
}
