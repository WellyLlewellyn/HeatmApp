# source R Function Directory ---------------------------------------------------------------        

sourceDirectory <- function(pathToFolder=paste(getwd(),"/R/",sep = "")){
  for (x in list.files(pathToFolder)){
    source(file.path(pathToFolder, x))}}

if(xfun::dir_exists("R")==TRUE){
  sourceDirectory()
  print(".R directory found and sourced")
}else{
  print("No .R directory found")}

# Libraries ---------------------------------------------------------------

library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(ggplot2)




# Run App ---------------------------------------------------------------

folder_address <- paste0(gsub("\\", "/",dirname(rstudioapi::getActiveDocumentContext()$path), fixed=TRUE),"/")

shiny::runApp(appDir = folder_address,launch.browser = TRUE)

# End ---------------------------------------------------------------