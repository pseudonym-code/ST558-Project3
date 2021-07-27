library(shiny)
library(shinydashboard)

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

    dashboardBody()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
