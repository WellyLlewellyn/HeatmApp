# Libraries ---------------------------------------------------------------

sourceDirectory()
library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)



# Server Logic ------------------------------------------------------------

shinyServer(function(input, output) {
    InputCyber <- reactive({
        CyberFile <- input$CyberFile
        if (is.null(CyberFile))
        {
            return(NULL)
        }
        else
            (result <-
                 read_csv(CyberFile$datapath, col_types = cols(PanelId = col_number())))
        return(result)
    })
    
    InputLensSQL <- reactive({
        LensSQLFile <- input$LensSQLFile
        if (is.null(LensSQLFile))
        {
            return(NULL)
        }
        else
            (result <- read_csv(LensSQLFile$datapath))
        return(result)
    })
    
    InputLensStandardEdit <- reactive({
        LensStdFile <- input$LensStdFile
        if (is.null(LensStdFile))
        {
            return(NULL)
        }
        else
            (result <- read_csv(LensStdFile$datapath))
        return(result)
    })
    
    output$CyberStraightOut <- DT::renderDT(InputCyber())
    output$LensSQLStraightOut <- DT::renderDT(InputLensSQL())
    output$LensStdStraightOut <-
        DT::renderDT(InputLensStandardEdit())
    
    
    ### ActiveFilesUIInput
    ActiveFiles <- reactive({
        ActFl <- list()
        if (is.null(InputCyber())) {
        } else
            (ActFl <- append(ActFl, "AdvCyber"))
        if(is.null(InputLensSQL())){}else(ActFl <- append(ActFl,"LensSQL"))
        if(is.null(InputLensStandardEdit())){}else(ActFl <- append(ActFl,"LensStd"))
        return(ActFl)})
    output$ActFl <-
        renderUI(
            selectInput(
                inputId = "ActiveFile",
                label = "Choose the Data Source",
                choices = ActiveFiles(),
                selected = "LensSQL",
                multiple = FALSE
            )
        )
    
    ### Main Data
    MainData <- reactive({
        if (input$ActiveFile == "AdvCyber") {
            Data <-
                advancedCyberPrep(Data = InputCyber(), ComponentSplit = "Package")
        } else if (input$ActiveFile == "LensSQL") {
            Data <-
                advancedLensPrep(Data = InputLensSQL(), ComponentSplit = "Group")
        } else if (input$ActiveFile == "LensStd") {
            Data <-
                advancedLensPrep(Data = InputLensStandardEdit(), ComponentSplit = "Group")
        } else
            (Data <-
                 data_frame(
                     "A" = c(0, 0, 0, 0, 1, 0, 6) ,
                     "B" = c(0, 0, 0, 1, 0, 5, 0),
                     "C" = c(0, 0, 1, 0, 4, 0, 15),
                     "D" = c(0, 1, 0, 3, 0, 10, 0),
                     "E" = c(1, 0, 2, 0, 6, 0, 20),
                     "F" = c(0, 1, 0, 3, 0, 10, 0),
                     "G" = c(0, 0, 1, 0, 4, 0, 15),
                     "H" = c(0, 0, 0, 1, 0, 5, 0),
                     "I" = c(0, 0, 0, 0, 1, 0, 6)
                 ))
        Data$Feature[is.na(Data$Feature)] <- '$'
        return(Data)
    })
    output$MainData <- renderDT(MainData())
    boardList <- reactive({as.list(sort(unique(MainData()$BoardNo)))})
    
    # Compnent Heatmap Overview -----------------------------------------------
    
    output$CompHeatPackageFilter <-
        renderUI(
            pickerInput(
                inputId = "CompHeatmapPkgFil",
                label = "Select Packages",
                inline = TRUE,
                choices = as.list(sort(unique(MainData()$Package))),
                selected = as.list(unique(MainData()$Package)),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                )
            )
        )
    
    output$CompHeatComponentFilter <-
        renderUI(
            pickerInput(
                inputId = "CompHeatmapLocFil",
                label = "Select Locations",
                inline = TRUE,
                choices = as.list(sort(unique(
                    MainData()$Location
                ))),
                selected = as.list(unique(MainData()$Location)),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                )
            )
        )
    
    output$CompHeatFeatueFilter <-
        renderUI(
            pickerInput(
                inputId = "CompHeatmapFeatureFil",
                label = "Select Features/Pins-Broken",
                inline = TRUE,
                choices = as.list(sort(unique(
                    MainData()$Feature
                ))),
                selected = as.list(sort((
                    MainData()$Feature
                ))),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    `selected-text-format` = "count > 3"
                )
            )
        )
    
    output$CompHeatBrdFilter <- renderUI(
                pickerInput(inputId = "CompHeatmapBrdFil",label = "Select Boards",inline = TRUE,choices = as.list(sort(unique(MainData()$BoardNo))),
                selected = as.list(unique(MainData()$BoardNo)),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3"))
                )
    
    CompHeatmapDataFiltered <- reactive({
        Result <- MainData()
        Result <-
            Result %>% filter(Package %in% input$CompHeatmapPkgFil)
        Result <-
            Result %>% filter(Location %in% input$CompHeatmapLocFil)
        Result <-
            Result %>% filter(Feature %in% input$CompHeatmapFeatureFil)
        Result <-
            Result %>% filter(BoardNo %in% input$CompHeatmapBrdFil)
        return(Result)
    })
    
    CompHeatmapData <- reactive({
        Result <- CompHeatmapDataFiltered() %>% group_by(Location) %>%
            dplyr::summarise(
                "Package" = paste(unique(Package), collapse = ', '),
                "PosX(mm)" = mean(`PosX(mm)`),
                "PosY(mm)" = mean(`PosY(mm)`),
                "OffsetX" = mean(OffsetX),
                "OffsetY" = mean(OffsetY),
                count = n()
            )
        return(Result)
    })
    output$CompHeatmapDataFilteredDT <-
        renderDT(CompHeatmapDataFiltered())
    output$CompHeatmapDataDT <- renderDT(CompHeatmapData())
    
    output$CompHeatmapPlotly <- renderPlotly(ggplotly(
        ggplot(
            data = CompHeatmapData(),
            height = paste0(dev.size("px")[2], "px"),
            mapping = aes(x = `PosX(mm)`, y = `PosY(mm)`, label = `Location`)
        ) +
            geom_text(
                aes(color = factor(sort(`Package`))),
                size = 4,
                check_overlap = TRUE
            )
    ))
    
    
    xyVariableList <-
        list("PosX(mm)", "PosY(mm)", "OffsetX", "OffsetY")
    zVariableList <- reactive({
        as.list(colnames(MainData()))
    })
     # Overall Heatmaps --------------------------------------------------------
    output$overallBoardChoice <- renderUI(pickerInput(inputId = "overallBoardChoiceBoards",label = "Select Boards",choices = boardList(),
                                                      selected = boardList(),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3")))
    
    
     output$OverallGrouping <- renderUI(selectInput(inputId = "OverallGroupingInput",label = "Select Grouping",choices = zVariableList(),selected = "PadID"))
     output$OverallXOutput  <- renderUI(selectInput(inputId = "OverallXInput",label = "Select X Choice",choices = xyVariableList,selected = "PosX(mm)"))
     output$OverallYOutput  <- renderUI(selectInput(inputId = "OverallYInput",label = "Select Y Choice",choices = xyVariableList,selected = "PosY(mm)"))
     output$OverallZOutput  <- renderUI(selectInput(inputId = "OverallZInput",label = "Select Z Choice",choices = zVariableList(),selected = "VolumePerc"))
    
    OverallHeatmapData <- reactive({
        if     (input$OverallMetric=="Mean"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))     %>% dplyr::summarise("Metric" = round(mean(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="Median"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))   %>% dplyr::summarise("Metric" = round(median(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="Min"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))      %>% dplyr::summarise("Metric" = round(min(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="Max"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))      %>% dplyr::summarise("Metric" = round(max(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="IQR"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))      %>% dplyr::summarise("Metric" = round(IQR(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="StdDev"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))   %>% dplyr::summarise("Metric" = round(sd(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="Variance"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput)) %>% dplyr::summarise("Metric" = round(var(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$OverallMetric=="Mad"){Result <- MainData() %>% group_by(get(input$OverallGroupingInput))      %>% dplyr::summarise("Metric" = round(mad(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else (Result <- MainData() %>% group_by(get(input$OverallGroupingInput))                                    %>% dplyr::summarise("Metric" = round(mean(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7)))
    })
    output$OverallHeatmapDT <- renderDT(OverallHeatmapData())
    
    output$OverallHeatmapPlotly <-
        renderPlotly({
            input$OverallActionButton
            isolate(ggplotly(
                ggplot(
                    data = OverallHeatmapData(),
                    mapping = aes(
                        x = get(input$OverallXInput),
                        y = get(input$OverallYInput)
                    )
                ) +
                    geom_point(aes(color = Metric)) +
                    scale_color_gradientn(
                        limits = c(input$OverallLowerBound, input$OverallUpperBound),
                        colours = c(
                            input$OverallColourLower1,
                            input$OverallColourLower2,
                            input$OverallColourAverage,
                            input$OverallColourUpper2,
                            input$OverallColourUpper1
                        )
                    )
            ))
        })
    output$OverallHeatmapUI <- renderUI({
        input$OverallActionButton
        isolate(
            plotlyOutput(
                "OverallHeatmapPlotly",
                height = input$OverallHeight,
                width = input$OverallWidth
            )
        )
    })
    

# Individual --------------------------------------------------------------
    output$singleBoardChoice <- renderUI(pickerInput(inputId = "singleBoards",label = "Select single Boards",choices = boardList(),selected = boardList(),multiple = TRUE,options = list(`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 3")))
    
    output$singleGrouping <-renderUI(selectInput(inputId = "singleGroupingInput",label = "Select Grouping",choices = zVariableList(),selected = "Location"))
    output$singleXOutput <-renderUI(selectInput(inputId = "singleXInput",label = "Select X Choice",choices = xyVariableList,selected = "PosX(mm)"))
    output$singleYOutput <-renderUI(selectInput(inputId = "singleYInput",label = "Select Y Choice",choices = xyVariableList,selected = "PosY(mm)"))
    output$singleZOutput <-renderUI(selectInput(inputId = "singleZInput",label = "Select Z Choice",choices = zVariableList(),selected = "VolumePerc"))
    
    singleHeatmapData <- reactive({
        if     (input$singleMetric=="Mean"){Result <- MainData()  %>% group_by(BoardNo, get(input$singleGroupingInput))   %>% dplyr::summarise("Metric" = round(mean(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="Median"){Result <- MainData() %>%  group_by(BoardNo,get(input$singleGroupingInput))   %>% dplyr::summarise("Metric" = round(median(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="Min"){Result <- MainData() %>% group_by(BoardNo,get(input$singleGroupingInput))      %>% dplyr::summarise("Metric" = round(min(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="Max"){Result <- MainData() %>% group_by(get(input$singleGroupingInput))      %>% dplyr::summarise("Metric" = round(max(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="IQR"){Result <- MainData() %>% group_by(get(input$singleGroupingInput))      %>% dplyr::summarise("Metric" = round(IQR(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="StdDev"){Result <- MainData() %>% group_by(get(input$singleGroupingInput))   %>% dplyr::summarise("Metric" = round(sd(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="Variance"){Result <- MainData() %>% group_by(get(input$singleGroupingInput)) %>% dplyr::summarise("Metric" = round(var(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else if(input$singleMetric=="Mad"){Result <- MainData() %>% group_by(get(input$singleGroupingInput))      %>% dplyr::summarise("Metric" = round(mad(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7))}
        else (Result <- MainData() %>% group_by(get(input$singleGroupingInput)) %>% dplyr::summarise("Metric" = round(mean(get(input$OverallZInput)),3),"PosX(mm)" = round(mean(`PosX(mm)`), 7),"PosY(mm)" = round(mean(`PosY(mm)`), 7)))                                  
    }) 
    
    output$singleHeatmapDT <- renderDT(singleHeatmapData())
    output$singleHeatmapPlotly <- renderPlotly({
            input$singleActionButton
            isolate(ggplotly(
                ggplot(
                    data = OverallHeatmapData(),
                    mapping = aes(
                        x = get(input$singleXInput),
                        y = get(input$singleYInput)
                    )
                ) +
                    geom_point(aes(color = Metric)) +
                    facet_wrap('BoardNo') + 
                    scale_color_gradientn(
                        limits = c(input$singleLowerBound, input$singleUpperBound),
                        colours = c(
                            input$singleColourLower1,
                            input$singleColourLower2,
                            input$singleColourAverage,
                            input$singleColourUpper2,
                            input$singleColourUpper1
                        )
                    )
            ))
        })
    output$singleHeatmapUI <- renderUI({
        input$singleActionButton
        isolate(
            plotlyOutput(
                "singleHeatmapPlotly",
                height = input$singleHeight,
                width = input$OverallWidth
            )
        )
    })
    
    
# PrintDirection ----------------------------------------------------------
    output$PrintDirGrouping <-renderUI(selectInput(inputId = "PrintDirGroupingInput",label = "Select Grouping",choices = zVariableList(),selected = "PadID"))
    output$PrintDirXOutput <-renderUI(selectInput(inputId = "PrintDirXInput",label = "Select X Choice",choices = xyVariableList,selected = "PosX(mm)"))
    output$PrintDirYOutput <-renderUI(selectInput(inputId = "PrintDirYInput",label = "Select Y Choice",choices = xyVariableList,selected = "PosY(mm)"))
    output$PrintDirZOutput <-renderUI(selectInput(inputId = "PrintDirZInput",label = "Select Z Choice",choices = zVariableList(),selected = "VolumePerc"))

# CleanPeriods ------------------------------------------------------------
    output$CleanPerGrouping <-renderUI(selectInput(inputId = "CleanPerGroupingInput",label = "Select Grouping",choices = zVariableList(),selected = "PadID"))
    output$CleanPerXOutput <-renderUI(selectInput(inputId = "CleanPerXInput",label = "Select X Choice",choices = xyVariableList,selected = "PosX(mm)"))
    output$CleanPerYOutput <-renderUI(selectInput(inputId = "CleanPerYInput",label = "Select Y Choice",choices = xyVariableList,selected = "PosY(mm)"))
    output$CleanPerZOutput <-renderUI(selectInput(inputId = "CleanPerZInput",label = "Select Z Choice",choices = zVariableList(),selected = "VolumePerc"))
    
    
    
})
