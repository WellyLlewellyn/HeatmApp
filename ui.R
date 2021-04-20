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
    
library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

# Options -----------------------------------------------------------------

options(shiny.maxRequestSize = 99 * 1024 ^ 2)
options(
    DT.options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 10,
        select = list(style = 'os', items = 'row'),
        dom = 'Bfrtip',
        buttons = c('copy', 'select', 'csv', 'excel', 'colvis')
    )
)

# UI Code -----------------------------------------------------------------
ui <- shinydashboardPlus::dashboardPage(skin = "green",
                            dashboardHeader(disable = FALSE),
                        dashboardSidebar(disable = TRUE,  
                                         sidebarMenu(
                                             menuItem(text = "FileImport",tabName = "Load",icon = icon("file-csv"),selected = TRUE),
                                             menuItem(text = "Data",tabName = "Data",icon = icon("file")),
                                             menuItem(text = "Component",tabName = "ComponentHeatmap",icon = icon("dragon")),
                                             menuItem(text = "Overall",tabName = "OverallHeatmap",icon = icon("dog")),
                                             menuItem(text = "Singular",tabName = "SingularHeatmap",icon = icon("cat")),
                                             menuItem(text = "Print Dir",tabName = "PrintDirectionHeatmap",icon = icon("otter")),
                                             menuItem(text = "Cleaning Per",tabName = "CleaningPeriodHeatmap",icon = icon("hippo")),
                                             menuItem(text = "Default",tabName = "ShinyDefault",icon = icon("democrat"))
                                         )
                        ),
                        dashboardBody(
                            tabItems(

## File Load ---------------------------------------------------------------
                                tabItem(
                                    tabName = "Load",
                                    fluidRow(
                                        h4("File Load"),
                                        column(3,
                                               uiOutput("ActFl")
                                        ),
                                        column(3,
                                               fileInput(label = "Advanced Cyber File Load",inputId = "CyberFile", placeholder = "Choose One File Type Only", accept = ".csv", buttonLabel = "Load Here")
                                        ),
                                        column(3,
                                               fileInput(label = "Lens SQL File Load",inputId = "LensSQLFile", placeholder = "Choose One File Type Only", accept = ".csv", buttonLabel = "Load Here")
                                        ),
                                        column(3,
                                               fileInput(label = "Pre Editted Lens File Load",inputId = "LensStdFile", placeholder = "Choose One File Type Only", accept = ".csv", buttonLabel = "Load Here")
                                        )
                                    ),
                                    fluidRow(
                                        
                                    )
                                    ),

# Component Heatmap -------------------------------------------------------
                                tabItem(tabName = "Data",
                                        tabsetPanel(
                                            tabPanel("Choosen Data",
                                                     dataTableOutput('MainData')
                                            ),
                                            tabPanel("Cyber",
                                                     dataTableOutput('CyberStraightOut')
                                                ),
                                            tabPanel("LensSQL",
                                                     dataTableOutput('LensSQLStraightOut')
                                            ),
                                            tabPanel("LensStd",
                                                     dataTableOutput('LensStdStraightOut')
                                            )
                                        )),

# Component Heatmap -------------------------------------------------------
                                tabItem(tabName = "ComponentHeatmap",
                                        h4("Component HM"),
                                        fluidPage(
                                            fluidRow(
                                                column(3,uiOutput('CompHeatPackageFilter')),
                                                column(3,uiOutput('CompHeatComponentFilter')),
                                                column(3,uiOutput('CompHeatFeatueFilter')),
                                                column(3,uiOutput('CompHeatBrdFilter'))
                                            ),
                                            fluidRow(
                                                plotlyOutput("CompHeatmapPlotly",height = paste0(dev.size("px")[2],"px"))
                                                ),
                                                
                                            fluidRow(
                                                DTOutput("CompHeatmapDataFilteredDT"),
                                                DTOutput("CompHeatmapDataDT")
                                                
                                            )
                                                
                                            )
                                        ),

# Overall Heatmap ---------------------------------------------------------
                                tabItem(tabName = "OverallHeatmap",
                                        h4("Overall HM"),
                                        fluidPage(
                                            fluidRow(
                                                tabsetPanel(
                                                    tabPanel("Metrics&Values",
                                                column(3,
                                                       uiOutput('overallBoardChoice'),
                                                       uiOutput('OverallGrouping'),
                                                       uiOutput('OverallXOutput'),
                                                       uiOutput('OverallYOutput'),
                                                       uiOutput('OverallZOutput')
                                                       ),
                                                column(3,
                                                       numericInput(inputId = "OverallLowerBound",label = "Lower Bound",value = 40,step = 1),
                                                       numericInput(inputId = "OverallUpperBound",label = "Upper Bound",value = 160,step = 1),
                                                       selectInput(inputId = "OverallMetric",label = "Metric for use",choices = list("Mean","Median","Min","Max","","IQR","StdDev","Variance","Mad"),selected = "Mean",multiple = FALSE),
                                                       numericInput(inputId = "OverallHeight",label = "Graph Height",value = 900,step = 1),
                                                       numericInput(inputId = "OverallWidth",label = "Graph Width",value = 950,step = 1)
                                                       ),
                                                column(3,
                                                       colorPickr(inputId = "OverallColourLower1",label = "Minimum Bound Colour",selected = "#6A5ACD"),
                                                       colorPickr(inputId = "OverallColourLower2",label = "Lower Tansition Colour",selected = "#28D8C8"),
                                                       colorPickr(inputId = "OverallColourAverage",label = "Average Colour",selected = "#2AFF60"),
                                                       colorPickr(inputId = "OverallColourUpper2",label = "Upper Transition Colour",selected = "#DBE95D"),
                                                       colorPickr(inputId = "OverallColourUpper1",label = "Maximum Bound Colour",selected = "#E5FF00")
                                                       ),
                                                column(3
                                                ),
                                                actionBttn(inputId = "OverallActionButton",label = "Update Heatmap",icon = "cat",style = "pill",block = TRUE)),
                                                
                                                tabPanel("DataTable",DTOutput("OverallHeatmapDT")))),
                                        hr(),
                                            fluidRow(
                                                uiOutput('OverallHeatmapUI')
                                            )
                                            
                                        )
                                        ),

# Singular Heatmap ---------------------------------------------------------
                                tabItem(tabName = "SingularHeatmap",h4("Singular HM"),
                                        fluidPage(
                                            fluidRow(
                                                tabsetPanel(
                                                    tabPanel("Metrics&Values",
                                                             column(3,
                                                                    uiOutput('singleBoardChoice'),
                                                                    uiOutput('singleGrouping'),
                                                                    uiOutput('singleXOutput'),
                                                                    uiOutput('singleYOutput'),
                                                                    uiOutput('singleZOutput')
                                                             ),
                                                             column(3,
                                                                    numericInput(inputId = "singleLowerBound",label = "Lower Bound",value = 40,step = 1),
                                                                    numericInput(inputId = "singleUpperBound",label = "Upper Bound",value = 160,step = 1),
                                                                    selectInput(inputId = "singleMetric",label = "Metric for use",choices = list("Mean","Median","Min","Max","IQR","StdDev","Variance","Mad"),selected = "Mean",multiple = FALSE),
                                                                    numericInput(inputId = "singleHeight",label = "Graph Height",value = 1200,step = 1),
                                                                    numericInput(inputId = "singleWidth",label = "Graph Width",value = 1600,step = 1)
                                                             ),
                                                             column(3,
                                                                    colorPickr(inputId = "singleColourLower1",label = "Minimum Bound Colour",selected = "#6A5ACD"),
                                                                    colorPickr(inputId = "singleColourLower2",label = "Lower Tansition Colour",selected = "#28D8C8"),
                                                                    colorPickr(inputId = "singleColourAverage",label = "Average Colour",selected = "#2AFF60"),
                                                                    colorPickr(inputId = "singleColourUpper2",label = "Upper Transition Colour",selected = "#DBE95D"),
                                                                    colorPickr(inputId = "singleColourUpper1",label = "Maximum Bound Colour",selected = "#E5FF00")
                                                             ),
                                                             column(3
                                                             ),
                                                             actionBttn(inputId = "singleActionButton",label = "Update Heatmap",icon = "cat",style = "pill",block = TRUE)),
                                                    
                                                    tabPanel("DataTable",DTOutput("singleHeatmapDT")))),
                                            hr(),
                                            fluidRow(
                                                uiOutput('singleHeatmapUI')
                                            )
                                            
                                        )),

# Print Direction Heatmap ---------------------------------------------------------
                                tabItem(tabName = "PrintDirectionHeatmap",h4("Print Dir HM"),
                                        fluidPage(
                                            fluidRow(
                                                tabsetPanel(
                                                    tabPanel("Metrics&Values",
                                                             column(3,
                                                                    uiOutput('PrintDirBoardChoice'),
                                                                    uiOutput('PrintDirGrouping'),
                                                                    uiOutput('PrintDirXOutput'),
                                                                    uiOutput('PrintDirYOutput'),
                                                                    uiOutput('PrintDirZOutput')
                                                             ),
                                                             column(3,
                                                                    numericInput(inputId = "PrintDirLowerBound",label = "Lower Bound",value = 40,step = 1),
                                                                    numericInput(inputId = "PrintDirUpperBound",label = "Upper Bound",value = 160,step = 1),
                                                                    selectInput(inputId = "PrintDirMetric",label = "Metric for use",choices = list("Mean","Median","Min","Max","","IQR","StdDev","Variance","Mad"),selected = "Mean",multiple = FALSE),
                                                                    numericInput(inputId = "PrintDirHeight",label = "Graph Height",value = 900,step = 1),
                                                                    numericInput(inputId = "PrintDirWidth",label = "Graph Width",value = 1800,step = 1)
                                                             ),
                                                             column(3,
                                                                    colorPickr(inputId = "PrintDirColourLower1",label = "Minimum Bound Colour",selected = "#6A5ACD"),
                                                                    colorPickr(inputId = "PrintDirColourLower2",label = "Lower Tansition Colour",selected = "#28D8C8"),
                                                                    colorPickr(inputId = "PrintDirColourAverage",label = "Average Colour",selected = "#2AFF60"),
                                                                    colorPickr(inputId = "PrintDirColourUpper2",label = "Upper Transition Colour",selected = "#DBE95D"),
                                                                    colorPickr(inputId = "PrintDirColourUpper1",label = "Maximum Bound Colour",selected = "#E5FF00")
                                                             ),
                                                             column(3
                                                             ),
                                                             actionBttn(inputId = "PrintDirActionButton",label = "Update Heatmap",icon = "cat",style = "pill",block = TRUE)),
                                                    
                                                    tabPanel("DataTable",DTOutput("PrintDirHeatmapDT")))),
                                            hr(),
                                            fluidRow(
                                                uiOutput('PrintDirHeatmapUI')
                                            )
                                            
                                        )),

# Cleaning Period Heatmap ---------------------------------------------------------
                                tabItem(tabName = "CleaningPeriodHeatmap",h4("Cleaning Period HM"),
                                        fluidPage(
                                            fluidRow(
                                                tabsetPanel(
                                                    tabPanel("Metrics&Values",
                                                             column(3,
                                                                    numericInput(inputId = "CleanPerAmount",label = "Boards Printed Per Cleaning Period",value = 4,step = 1),
                                                                    uiOutput('CleanPerBoardChoice'),
                                                                    uiOutput('CleanPerGrouping'),
                                                                    
                                                             ),
                                                             column(3,
                                                                    uiOutput('CleanPerXOutput'),
                                                                    uiOutput('CleanPerYOutput'),
                                                                    uiOutput('CleanPerZOutput')
                                                             ),
                                                             column(4,
                                                                    numericInput(inputId = "CleanPerLowerBound",label = "Lower Bound",value = 40,step = 1),
                                                                    numericInput(inputId = "CleanPerUpperBound",label = "Upper Bound",value = 160,step = 1),
                                                                    selectInput(inputId = "CleanPerMetric",label = "Metric for use",choices = list("Mean","Median","Min","Max","","IQR","StdDev","Variance","Mad"),selected = "Mean",multiple = FALSE),
                                                                    numericInput(inputId = "CleanPerHeight",label = "Graph Height",value = 900,step = 1),
                                                                    numericInput(inputId = "CleanPerWidth",label = "Graph Width",value = 950,step = 1)
                                                             ),
                                                             column(2,
                                                                    colorPickr(inputId = "CleanPerColourLower1",label = "Minimum Bound Colour",selected = "#6A5ACD"),
                                                                    colorPickr(inputId = "CleanPerColourLower2",label = "Lower Tansition Colour",selected = "#28D8C8"),
                                                                    colorPickr(inputId = "CleanPerColourAverage",label = "Average Colour",selected = "#2AFF60"),
                                                                    colorPickr(inputId = "CleanPerColourUpper2",label = "Upper Transition Colour",selected = "#DBE95D"),
                                                                    colorPickr(inputId = "CleanPerColourUpper1",label = "Maximum Bound Colour",selected = "#E5FF00")
                                                             ),
                                                             actionBttn(inputId = "CleanPerActionButton",label = "Update Heatmap",icon = "cat",style = "pill",block = TRUE)),
                                                    
                                                    tabPanel("DataTable",DTOutput("CleanPerHeatmapDT")))),
                                            hr(),
                                            fluidRow(
                                                uiOutput('CleanPerHeatmapUI')
                                            )
                                            
                                        )),

# Spare Heatmap -------------------------------------------------------
                                
# Shiny Default ----------------------------------------------------------
                                tabItem(tabName = "ShinyDefault",
                                        fluidPage(
                                            fluidRow(
                                                tabsetPanel(
                                                    tabPanel("Metrics&Values",
                                                             column(3,
                                                                    uiOutput('DonkeyBoardChoice'),
                                                                    uiOutput('DonkeyGrouping'),
                                                                    uiOutput('DonkeyGrouping2')
                                                             ),
                                                             column(3,
                                                                    uiOutput('DonkeyXOutput'),
                                                                    uiOutput('DonkeyYOutput'),
                                                                    uiOutput('DonkeyZOutput')
                                                             ),
                                                             column(3,
                                                                    numericInput(inputId = "DonkeyLowerBound",label = "Lower Bound",value = 40,step = 1),
                                                                    numericInput(inputId = "DonkeyUpperBound",label = "Upper Bound",value = 160,step = 1),
                                                                    selectInput(inputId = "DonkeyMetric",label = "Metric for use",choices = list("Mean","Median","Min","Max","IQR","StdDev","Variance","Mad"),selected = "Mean",multiple = FALSE),
                                                                    numericInput(inputId = "DonkeyHeight",label = "Graph Height",value = 1200,step = 1),
                                                                    numericInput(inputId = "DonkeyWidth",label = "Graph Width",value = 1600,step = 1)
                                                             ),
                                                             column(3,
                                                                    colorPickr(inputId = "DonkeyColourLower1",label = "Minimum Bound Colour",selected = "#6A5ACD"),
                                                                    colorPickr(inputId = "DonkeyColourLower2",label = "Lower Tansition Colour",selected = "#28D8C8"),
                                                                    colorPickr(inputId = "DonkeyColourAverage",label = "Average Colour",selected = "#2AFF60"),
                                                                    colorPickr(inputId = "DonkeyColourUpper2",label = "Upper Transition Colour",selected = "#DBE95D"),
                                                                    colorPickr(inputId = "DonkeyColourUpper1",label = "Maximum Bound Colour",selected = "#E5FF00")
                                                             ),
                                                             
                                                             actionBttn(inputId = "DonkeyActionButton",label = "Update Heatmap",icon = icon("otter"),style = "pill",block = TRUE)),
                                                    
                                                    tabPanel("DataTable",DTOutput("DonkeyHeatmapDT")))),
                                            hr(),
                                            fluidRow(
                                                uiOutput('DonkeyHeatmapUI')
                                            )
                                            
                                        )
                                        
                                        )
                            )
                        )
                        
)

    

