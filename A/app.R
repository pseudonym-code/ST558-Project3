library(shiny)
library(dashboardthemes)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(DT)
library(caret)

data <- read.csv("../Project3/baseball.csv", fileEncoding="UTF-8-BOM")
setFactors = c("game_month","is_batter_lefty","is_pitcher_lefty","inning","outs_when_up","balls","strikes","is_home_run")
data[setFactors] <- lapply(data[setFactors], factor)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "MLB Home Runs"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = 'about', icon = icon("inbox")),
            menuItem("Data", tabName = 'data', icon = icon("th-list")),
            menuItem("Data Exploration", tabName = 'explore', icon = icon("signal")),
            menuItem("Modeling", tabName = 'model', icon = icon("fire"),
                     menuSubItem("Modeling Info", "info", icon = icon("education")),
                     menuSubItem("Model Fitting", "fit", icon = icon("sunglasses")),
                     menuSubItem("Prediction", "predict", icon = icon("screenshot"))
            )
        )
    ),
    
    dashboardBody(
        
        ### changing theme
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        
        tabItems(
            # About tab content
            tabItem(tabName = "about",
                    h1("About the Dashboard"),
                    fluidRow(
                        box(
                            HTML("<p>This applet allows for exploration into what factors lead to Home Runs (HR) in baseball. You can do this through exploring the data, altering the data set, and modeling using multiple different methods. The data set includes a number of variables about both the individual match-up of pitcher vs. batter, as well as the ballpark dimensions which often play a large part in the number of HR hit. This <a href='https://www.kaggle.com/jcraggy/baseball'>data set</a> comes from <a href='https://www.kaggle.com/'>kaggle</a>, a popular data science competition and collaboration website.</p>"),
                            uiOutput("about"),
                            br(),
                            HTML("<p> Now dig into the data and see what you can find!</p>")
                        ),
                        box(imageOutput('mlb'))
                    )
            ),
            
            # Data tab content
            tabItem(tabName = "data",
                    h1("Data"),
                    fluidRow(
                        box(width=3, status = "primary", title = "Inputs", solidHeader = TRUE,
                            checkboxInput("change", "Adjust the data set?"),
                            conditionalPanel(
                                'input.change == true',
                                br(),
                                checkboxGroupInput("show_vars", "Columns in data set to show:",
                                                   names(data), selected = names(data), inline=FALSE),
                                actionLink("clearall","Clear Columns"),
                                br(),br(),
                                textInput('filename',"Filename to save as? (default will be 'new_baseball.csv')"),
                                actionButton("generateButton","Save Data")
                            )
                        ),
                        box(width=9,status = "success", title = "Data", solidHeader = TRUE,
                            dataTableOutput("dataTable")),
                        box(width=12,status = "info", title = "Data Dictionary", solidHeader = TRUE,
                            checkboxInput("dict", "Show data dictionary?"),
                            conditionalPanel('input.dict == true',
                                             tableOutput('datadict'))
                        )
                    )
                    
            ),
            
            # Data Exploration tab content
            tabItem(tabName = "explore",
                    h1("Data Exploration"),
                    fluidRow(
                        column(width=4,
                               tabBox(width=NULL, title = "HR Inputs", id = "tabset1",
                                      tabPanel("HR Data", 
                                               radioButtons("dataChoice",label="Select Data Set", choices = c("Original Data", "Adjusted Data")),
                                               conditionalPanel(
                                                   "input.dataChoice == 'Adjusted Data'",
                                                   HTML("<b style='color:red;'>WARNING! Using an adjusted data set may lead to plots or tables not loading properly</b>"),
                                                   br(),br()
                                               ),
                                               selectInput("colorBy","Color by:", list("None","is_batter_lefty","is_pitcher_lefty","bb_type", "bearing", "pitch_name","inning","outs_when_up","balls","strikes"))
                                      ),
                                      tabPanel("HR by Team", 
                                               selectInput("team","Select Team:", c("None",sort(unique(data$batter_team)))),
                                               selectInput("colorByTeam","Color by:", list("None","is_batter_lefty","is_pitcher_lefty","bb_type", "bearing", "pitch_name","inning","outs_when_up","balls","strikes"))
                                      ),
                                      tabPanel("HR by Park", 
                                               selectInput("park","Select Park:", c("None",sort(unique(data$name)))),
                                               selectInput("colorByPark","Color by:", list("None","away_team","is_batter_lefty","is_pitcher_lefty","bb_type", "bearing", "pitch_name","inning","outs_when_up","balls","strikes"))
                                      ),
                                      tabPanel("HR by Batter", 
                                               selectInput("batter","Select Batter:", c("None",sort(unique(data$batter_name)))),
                                               selectInput("colorByBatter","Color by:", list("None","home_team","away_team","is_pitcher_lefty","bb_type", "bearing", "pitch_name","inning","outs_when_up","balls","strikes"))
                                      ),
                                      tabPanel("HR by Pitcher", 
                                               selectInput("pitcher","Select Pitcher:", c("None",sort(unique(data$pitcher_name)))),
                                               selectInput("colorByPitcher","Color by:", list("None","home_team","away_team","is_batter_lefty","bb_type", "bearing", "pitch_name","inning","outs_when_up","balls","strikes"))
                                      )
                               ),
                               box(width=NULL),
                               box(width=NULL)
                        ),
                        column(width=4,
                               box(width=NULL,title="HR Plot", status="success",solidHeader = T,
                                   conditionalPanel("input.tabset1 == 'HR Data'",
                                                    plotOutput("allDataPlot")
                                   ),
                                   conditionalPanel("input.tabset1 == 'HR by Team'",
                                                    plotOutput("teamPlot")
                                   ),
                                   conditionalPanel("input.tabset1 == 'HR by Park'",
                                                    plotOutput("parkPlot")
                                   ),
                                   conditionalPanel("input.tabset1 == 'HR by Batter'",
                                                    plotOutput("batterPlot")
                                   ),
                                   conditionalPanel("input.tabset1 == 'HR by Pitcher'",
                                                    plotOutput("pitcherPlot")
                                   )
                                   
                               ),
                               box(width=NULL),
                               box(width=NULL)
                        ),
                        column(width=4,
                               box(width=NULL,title="HR Data", status="info",solidHeader = T,
                                   tableOutput("allDataTbl")
                               ),
                               box(width=NULL),
                               box(width=NULL)
                        ),
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Data functions
    getNewData <- reactive({
        newData <- data[input$dataTable_rows_all, input$show_vars]
    })
    
    getData <- reactive({
        newData <- data[, input$show_vars]
    })
    
    getPlotData <- reactive({
        if(input$dataChoice == "Original Data"){
            if(input$tabset1 == 'HR by Team'){
                newData <- data %>% filter(batter_team == input$team)
            } else if(input$tabset1 == 'HR by Park'){
                newData <- data %>% filter(name == input$park)
            } else if(input$tabset1 == 'HR by Batter'){
                newData <- data %>% filter(batter_name == input$batter)
            } else if(input$tabset1 == 'HR by Pitcher'){
                newData <- data %>% filter(pitcher_name == input$pitcher)
            } else{
                data
            }
        } else{
            data <- getNewData()
            if(input$tabset1 == 'HR by Team'){
                newData <- data %>% filter(batter_team == input$team)
            } else if(input$tabset1 == 'HR by Park'){
                newData <- data %>% filter(name == input$park)
            } else if(input$tabset1 == 'HR by Batter'){
                newData <- data %>% filter(batter_name == input$batter)
            } else if(input$tabset1 == 'HR by Pitcher'){
                newData <- data %>% filter(pitcher_name == input$pitcher)
            } else{
                data
            }
        }
    })
    
    trainTest <- reactive({
        # Split into train and test sets
        set.seed(144)
        trainNum <- sample(1:nrow(data), size = nrow(data)*input$trainProp)
        testNum <- dplyr::setdiff(1:nrow(data), train)
        trainData <- data[trainNum, ]
        testData <- data[testNum, ]
        list(trainData,testData)
    })
    
    glmFit <- reactive({
        logFit <- train(is_home_run ~ input$, data = train, family = "binomial",
                        method="glm", preProcess = c("center","scale"),
                        trControl = trainControl(method = "cv", number = input$cvNum))
    })
    
    treeFit <- reactive({
        trctrl <- trainControl(method = "repeatedcv", number = input$cvNum, repeats = input$repNum)
        
        classTreeFit <- train(is_home_run ~ input$,
                              data = trainData, method = "rpart",
                              trControl=trctrl,
                              preProcess = c("center", "scale"))
    })
    
    forestFit <- reactive({
        trctrl <- trainControl(method = "repeatedcv", number = input$cvNum, repeats = input$repNum)
        ranForestFit <- train(is_home_run ~ input$,
                              data = diabDataTrain, method = "rf",
                              trControl=trctrl,
                              preProcess = c("center", "scale"))
    })
    
    # About tab functions
    output$about <- renderUI({
        HTML(paste("There are multiple tabs on the left.","", "The 'Data' tab allows you to explore and filter the data as desired, and gives the option to save the new data set to a CSV file.", "",
                   "The 'Data Exploration' tab contains multiple ways to summarize and visualize the data.", "", "The 'Modeling' tab is where the statistical modeling will be done. This tab contains multiple sub-tabs:",
                   "&emsp; 1) A 'Modeling Info' tab which allows you to better understand the techniques.","&emsp; 2) A 'Model Fitting' tab where the models are actually fit and settings for each model are adjusted.",
                   "&emsp; 3) A 'Prediction' tab, where the models can then be used on a prediction set or observation, and the results displayed.",sep="<br/>"))
    })
    
    output$mlb <- renderImage({
        list(src = 'mlb.png',
             contentType = 'image/png',
             width = "100%",
             height = "100%",
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    # Data tab functions
    output$dataTable <- renderDataTable(
        getData(),
        filter = "top",
        options = list(scrollX = TRUE,lengthMenu = c(15, 30, 50, 100), pageLength = 10)
    )
    
    observe({
        if(input$clearall == 0) return(NULL)
        else {
            updateCheckboxGroupInput(session,"show_vars", "Columns in data set to show:", names(data))
        }
    })
    
    observe({
        if(input$generateButton == 1) {
            if(!isTruthy(input$filename)){
                write.csv(getNewData(),"new_baseball.csv", row.names = FALSE, na = "")
            } else{
                if(grepl(".csv", input$filename, fixed=TRUE)){
                    write.csv(getNewData(),input$filename, row.names = FALSE, na = "")
                } else{
                    newFile <- paste0(input$filename, ".csv")
                    write.csv(getNewData(),newFile, row.names = FALSE, na = "")
                }
            }
        }
    })
    
    output$datadict <- renderTable({
        dict <- read.csv('../Project3/datadict.csv', fileEncoding="UTF-8-BOM")
        dict
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

