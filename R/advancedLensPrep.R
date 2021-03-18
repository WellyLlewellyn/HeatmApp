#' advancedLensPrep
#' 
#' Rounding is applied to SizeArea(um^2) to 3 significant figures 
#'
#' @param Data Enter Direct CSV from Process Lens SQL, Either the Standard or Advanced Export
#' @param ComponentSplit Choose the Component grouping ID
#'
#' @export advancedLensPrep

advancedLensPrep <- function(Data,ComponentSplit="Group"){

  Data$BoardNo = (Data$PCBIndex - (min(Data$PCBIndex)-1))
  Data$PrintDir = (Data$BoardNo %% 2)
  Data$RealHeight <- Data$Height
  Data$RealMaxHeight <- Data$MaxHeight
  Data$RealVolume <- Data$RealVol
  Data$RealArea <- Data$RealArea
  
  ####Calculate Targets
  Data$HeightTarget <- Data$RealVolume/((Data$Volume*0.01)*Data$SizeArea)
  Data$AreaTarget <- Data$SizeArea
  Data$VolumeTarget <- Data$SizeArea*Data$HeightTarget
  
  ###Recalculate Percentages
  #Data$VolumePerc  <- Data$RealVol/Data$VolumeTarget
  #Data$AreaPerc  <- Data$RealArea/Data$AreaTarget
  #Data$HeightPerc  <- Data$RealHeight/Data$HeightTarget
  
  ###Use Lens Percentages
  Data$VolumePerc  <- Data$Volume
  Data$AreaPerc  <- Data$Area
  Data$HeightPerc  <- Data$Height
  
  Data$Component <- Data[[ComponentSplit]]
  Data$Feature <- Data$PinNumber
  Data$Package <- Data$Group
  Data$Location <- Data$ComponentID
  Data$`PosX(mm)` <- Data$PosX
  Data$`PosY(mm)` <- Data$PosY
  
  ###Roundings
  Data$HeightTarget <- round(Data$HeightTarget,0) #single Micron
  Data$AreaTarget <- round(Data$AreaTarget,0) #single Micron^2
  Data$VolumeTarget <- round(Data$VolumeTarget,0) #single Micron^3
  Data$HeightPerc <- round(Data$HeightPerc,0)
  Data$AreaPerc   <- round(Data$AreaPerc,0)
  Data$VolumePerc <- round(Data$VolumePerc,0) 
  Data$SizeArea <- round(Data$SizeArea,-2) #hundred Microns
  Data$SizeX <- round(Data$SizeX,0) #single Micron
  Data$SizeY <- round(Data$SizeY,0) #single Micron
  
  Data <- base::subset(Data,
                       select = c(BoardNo,PrintDir,Package,Location,Feature,PadID,`PosX(mm)`,`PosY(mm)`,Component,SizeX,SizeY,SizeArea,
                                  RealVolume,RealArea,RealHeight,RealMaxHeight,VolumePerc,AreaPerc,HeightPerc,VolumeTarget,AreaTarget,HeightTarget,
                                  OffsetX,OffsetY))
  return(Data)
}
