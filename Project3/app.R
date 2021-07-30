library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

data <- read.csv("../Project3/baseball.csv", fileEncoding="UTF-8-BOM")

# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "MLB Hits"),

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

    dashboardBody(tabItems(
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
                    box(width=3,
                        checkboxInput("change", "Adjust the data set?"),
                        conditionalPanel(
                            'input.change == true',
                            br(),
                            checkboxGroupInput("show_vars", "Columns in data set to show:",
                                                           names(data), selected = names(data), inline=FALSE),
                            actionLink("clearall","Clear Columns"),
                            br(),
                            textInput('filename',"Filename to save as? (default will be 'new_baseball.csv')"),
                            actionButton("generateButton","Save Data")
                        )
                    ),
                    box(width=9,dataTableOutput("dataTable")),
                    box(checkboxInput("dict", "Show data dictionary?"),
                        conditionalPanel('input.dict == true',
                                         tableOutput('datadict'))
                    )
                )
                
        ),
        
        # Data Exploration tab content
        tabItem(tabName = "explore",
                fluidRow(
                    box(),
                    box()
                )
        ),
        
        # Modeling tab content
        tabItem(tabName = "model",
                fluidRow(
                    box(),
                    box()
                )
        ),
        
        # Model info tab content
        tabItem(tabName = "info",
                fluidRow(
                    box(),
                    box()
                )
        ),
        
        # Model fitting tab content
        tabItem(tabName = "fit",
                fluidRow(
                    box(),
                    box()
                )
        ),
        
        # Prediction tab content
        tabItem(tabName = "predict",
                fluidRow(
                    box(),
                    box()
                )
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    getNewData <- reactive({
        newData <- data[input$dataTable_rows_all, input$show_vars]
    })
    
    getData <- reactive({
        newData <- data[, input$show_vars]
    })
    
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
