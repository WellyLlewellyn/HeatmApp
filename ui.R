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
ui <- dashboardPagePlus(skin = "green",
                            dashboardHeaderPlus(disable = FALSE),
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

# File Load ---------------------------------------------------------------
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
                                                plotlyOutput("CompHeatmapPlotly")
                                                ),
                                                
                                            fluidRow(
                                                DTOutput("CompHeatmapDataFilteredDT"),
                                                DTOutput("CompHeatmapDataDT")
                                                
                                            )
                                                
                                            )
                                        ),

# Overall Heatmap ---------------------------------------------------------
                                tabItem(tabName = "OverallHeatmap",h4("Overall HM")),

# Singular Heatmap ---------------------------------------------------------
                                tabItem(tabName = "SingularHeatmap",h4("Singular HM")),

# Print Direction Heatmap ---------------------------------------------------------
                                tabItem(tabName = "PrintDirectionHeatmap",h4("Print Dir HM")),

# Cleaning Period Heatmap ---------------------------------------------------------
                                tabItem(tabName = "CleaningPeriodHeatmap",h4("Cleaning Period HM")),

# Spare Heatmap -------------------------------------------------------
                                
# Shiny Default ----------------------------------------------------------
                                tabItem(tabName = "ShinyDefault",
                                        # Application title
                                        titlePanel("Old Faithful Geyser Data"),
                                        
                                        # Sidebar with a slider input for number of bins
                                        sidebarLayout(
                                            sidebarPanel(
                                                sliderInput("bins",
                                                            "Number of bins:",
                                                            min = 1,
                                                            max = 50,
                                                            value = 30)
                                            ),
                                            
                                            # Show a plot of the generated distribution
                                            mainPanel(
                                                plotOutput("distPlot")
                                            )
                                        )
                                        )
                            )
                        )
                        
)

    

