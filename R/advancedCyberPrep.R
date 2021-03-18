#' advancedCyberPrep
#'
#' @param Data Enter Direct CSV from Cyber, Either the Standard or Advanced Export
#' @param ComponentSplit Choose the Component grouping ID
#'
#' @export advancedCyberPrep

advancedCyberPrep <- function(Data,ComponentSplit="Location"){

  Data$BoardNo = (Data$PanelId - (min(Data$PanelId)-1))
  Data$PrintDir = (Data$BoardNo %% 2)
  Data$Height[is.nan(Data$Height)] <- 0
  Data$XOffset[is.nan(Data$XOffset)] <- 0
  Data$YOffset[is.nan(Data$YOffset)] <- 0
  Data$RealHeight <- Data$Height
  Data$RealArea   <- Data$Area
  Data$RealVolume <- Data$Volume
  Data$OffsetX    <- Data$XOffset
  Data$OffsetY    <- Data$YOffset

  if("HeightTarget" %in% colnames(Data)){
    Data$HeightPerc     <- (Data$Height/Data$HeightTarget)*100
    Data$AreaPerc       <- (Data$Area/Data$AreaTarget)*100
    Data$VolumePerc     <- (Data$Volume/Data$VolumeTarget)*100
    
    Data$SizeArea <- Data$AreaTarget
    Data$`PosX(mm)`      <- signif(Data$`X Position`/1000,5)
    Data$`PosY(mm)`      <- signif(Data$`Y Position`/1000,5)
    Data$PadID <- Data$BLF
    Data$`X Position` <- NULL 
    Data$`Y Position` <- NULL
    
  }
  else (Data <- Data)

  Data$XOffset    <- NULL
  Data$YOffset    <- NULL
  Data$Component <- Data[[ComponentSplit]]

  if("HeightTarget" %in% colnames(Data)){
    Data <- base::subset(Data,
                         select = c(BoardNo,PrintDir,Package,Location,Feature,`PosX(mm)`,`PosY(mm)`,
                                    VolumePerc,AreaPerc,HeightPerc,RealVolume,RealArea,RealHeight,
                                    VolumeTarget,AreaTarget,HeightTarget,OffsetX,OffsetY,Component,PadID))
  }
  else (
    Data <- base::subset(Data,
                         select = c(BoardNo,PrintDir,Location,Feature,
                                    Volume,Area,Height,RealVolume,RealArea,RealHeight,
                                    OffsetX,OffsetY,Component))
  )
  Data <- as_tibble(Data)
}
