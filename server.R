# Libraries ---------------------------------------------------------------

sourceDirectory()
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)

library(DT)






# Server Logic ------------------------------------------------------------

shinyServer(function(input, output) {

    InputCyber <- reactive({
        CyberFile <- input$CyberFile
        if (is.null(CyberFile))
        {return(NULL)}
        else (result <- read_csv(CyberFile$datapath,col_types = cols(PanelId = col_number())))
        return(result)})
    
    InputLensSQL <- reactive({
        LensSQLFile <- input$LensSQLFile
        if (is.null(LensSQLFile))
        {return(NULL)}
        else (result <- read_csv(LensSQLFile$datapath))
        return(result)})
    
    InputLensStandardEdit <- reactive({
        LensStdFile <- input$LensStdFile
        if (is.null(LensStdFile))
        {return(NULL)}
        else (result <- read_csv(LensStdFile$datapath))
        return(result)})
    
    output$CyberStraightOut <- DT::renderDT(InputCyber())
    output$LensSQLStraightOut <- DT::renderDT(InputLensSQL())
    output$LensStdStraightOut <- DT::renderDT(InputLensStandardEdit())
   

### ActiveFilesUIInput
    ActiveFiles <- reactive({
        ActFl <- list()
        if(is.null(InputCyber())){}else(ActFl <- append(ActFl,"AdvCyber"))
        if(is.null(InputLensSQL())){}else(ActFl <- append(ActFl,"LensSQL"))
        if(is.null(InputLensStandardEdit())){}else(ActFl <- append(ActFl,"LensStd"))
        return(ActFl)})
    output$ActFl <- renderUI(selectInput(inputId = "ActiveFile",label = "Choose the Data Source",choices = ActiveFiles(),selected = "LensSQL",multiple = FALSE))

### Main Data    
    MainData <- reactive({
               if (input$ActiveFile=="AdvCyber"){Data <- advancedCyberPrep(Data = InputCyber(),ComponentSplit = "Package")    
        } else if (input$ActiveFile=="LensSQL"){Data <- advancedLensPrep(Data = InputLensSQL(),ComponentSplit = "Group")
        } else if (input$ActiveFile=="LensStd"){Data <- advancedLensPrep(Data = InputLensStandardEdit(),ComponentSplit = "Group")
        } else    (Data <- data_frame("A" = c(0,0,0,0,1,0,6) ,"B" = c(0,0,0,1,0,5,0), "C" = c(0,0,1,0,4,0,15),
                                      "D" = c(0,1,0,3,0,10,0),"E" = c(1,0,2,0,6,0,20),"F" = c(0,1,0,3,0,10,0),
                                      "G" = c(0,0,1,0,4,0,15),"H" = c(0,0,0,1,0,5,0), "I" = c(0,0,0,0,1,0,6)))
        return(Data)
    })
    output$MainData <- renderDT(MainData())

    output$CompHeatPackageFilter <- renderUI(pickerInput(inputId = "CompHeatmapPkgFil",label = "Select Packages",inline = TRUE,choices = as.list(sort(unique(MainData()$Package))),
                                                 selected = as.list(unique(MainData()$Package)),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3" )))
    
    output$CompHeatComponentFilter <- renderUI(pickerInput(inputId = "CompHeatmapLocFil",label = "Select Locations",inline = TRUE,choices = as.list(sort(unique(MainData()$Location))),
                                                  selected = as.list(unique(MainData()$Location)),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3" )))
    
    output$CompHeatFeatueFilter <- renderUI(pickerInput(inputId = "CompHeatmapFeatureFil",label = "Select Features/Pins",inline = TRUE,choices = as.list(sort(unique(MainData()$Feature))),
                                                           selected = as.list(unique(MainData()$Feature)),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3" )))
    
    output$CompHeatBrdFilter <- renderUI(pickerInput(inputId = "CompHeatmapBrdFil",label = "Select Boards",inline = TRUE,choices = as.list(sort(unique(MainData()$BoardNo))),
                                                  selected = as.list(unique(MainData()$BoardNo)),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3" )))
    
    CompHeatmapDataFiltered <- reactive({
        Result <- MainData() %>% filter(Package %in% input$CompHeatmapPkgFil)
        Result <- Result %>% filter(Location %in% input$CompHeatmapLocFil)
        Result <- Result %>% filter(Feature %in% input$CompHeatmapFeatureFil)
        Result <- Result %>% filter(BoardNo %in% input$CompHeatmapBrdFil)
        return(Result)
    })
   
    CompHeatmapData <- reactive({
    Result <- CompHeatmapDataFiltered()%>%group_by(Location)%>%
        dplyr::summarise("Package"=paste(unique(Package), collapse = ', '),"PosX(mm)"=mean(`PosX(mm)`),"PosY(mm)"=mean(`PosY(mm)`),"OffsetX"=mean(OffsetX),"OffsetY"=mean(OffsetY),count =n())
    return(Result)
    })
    output$CompHeatmapDataFilteredDT <- renderDT(CompHeatmapDataFiltered())
    output$CompHeatmapDataDT <- renderDT(CompHeatmapData())
    
    output$CompHeatmapPlotly <- renderPlotly(ggplotly(
        ggplot(data = CompHeatmapData(),
               mapping = aes(x = `PosX(mm)`,y = `PosY(mm)`, label = `Location`)) + 
               geom_text(aes(color=factor(sort(`Package`))),size=4,check_overlap = TRUE)))
    
    
    
###Baisc Shiny    
        
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
                