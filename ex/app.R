library(shiny)
library(shinythemes)
library(shinydashboard)

shinyApp(
    ui = navbarPage("Human Trafficking", theme = shinytheme("flatly"),
                    tabPanel("Victim",
                             sidebarLayout(
                                 sidebarPanel(
                                     h3("This tab has a sidebar")
                                 ),
                                 mainPanel(
                                     h2("Victim tab")
                                 )
                             )
                    ),
                    tabPanel("Trafficker",
                             h2("Trafficker tab")
                    )
    ),
    server = function(input, output) { }
)