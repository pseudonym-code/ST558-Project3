library(shiny)
library(dashboardthemes)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(DT)
library(caret)
library(ROCR)
library(Metrics)

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
                            HTML("<p>This applet allows for exploration into what factors lead to Home Runs (HR) in baseball. You can do this through exploring the data, altering the data set, and modeling using multiple different methods. The data set includes a number of variables about both the individual match-up of pitcher vs. batter, as well as the ballpark dimensions which often play a large part in the number of HR hit. This <a href='https://www.kaggle.com/jcraggy/baseball'>data set</a> comes from <a href='https://www.kaggle.com/'>kaggle</a>, a popular data science competition and collaboration website. We are using a randomized subset of the data for this applet.</p>"),
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
                            box(width=NULL),
                            box(width=NULL),
                            box(width=NULL)
                        ),
                        column(width=4,
                            box(width=NULL,title="HR Plot", status="success",solidHeader = T
                            ),
                            box(width=NULL),
                            box(width=NULL)
                        ),
                        column(width=4,
                            box(width=NULL,title="HR Data", status="info",solidHeader = T
                            ),
                            box(width=NULL),
                            box(width=NULL)
                        )
                    )
            ),
            
            # Modeling tab content
            # Model info tab content
            tabItem(tabName = "info",
                    fluidRow(
                        box(width=4, title = "Generalized Linear Model", status = "success", solidHeader = TRUE,
                            HTML("<p> <a href='https://en.wikipedia.org/wiki/Logistic_regression'>Logistic Regression</a> is very common generalized linear model (GLM) used for classification of success/failure. This is perfect for this case, as we are looking at a response variable with 1 being a success, a home run, and 0 being a failure to hit a home run. Due to the nature of the data, we are modeling the average number of successes for a group of given explanatory variables. This requires that predictions are bound by 0 and 1. This is done by using the logistic function.</p>"),
                            uiOutput('logEQ'),
                            HTML("<p>Logistic Regression is very simple to train and interpret. There are few asumptions needed, and performs well with linearly separable data. Some disadvantages include that it contains linear boundaries, and therefore is a linear function. Complex relationships are hard to model using this method. Overall, this is a good, simple model for classification.")
                        ),
                        box(width=4, title = "Classification Tree", status = "primary", solidHeader = TRUE,
                            HTML("<p> <a href='https://en.wikipedia.org/wiki/Decision_tree_learning'>Classification Trees</a> are a tree based method of classification. The goal of this model is to classify and predict group membership. In our case, that 'group' is either a home run or not. The basis of this method is that the predictor space is split up into different regions, with each region being given a different prediction. Splits are chosen until nodes are created for the data that lead to a prediction. These trees can also be pruned to prevent overfitting and decrease variance. </p>"),
                            br(),
                            HTML("<p>Trees are very simple to understand, and easy to interpret. There is no scaling that needs to be done and no assumptions necessary for this method. It also includes variable selection in the process as it leads to the most impactful variables. Some disadvantages include that very minor changes to the data can lead to massive changes in the tree itself. This is due to the way that it is calculated. Another disadvantage is the need for pruning, where without it there is a tendency for overfitting.</p>")
                        ),
                        box(width=4, title = "Random Forest", status = "warning", solidHeader = TRUE,
                            HTML("<p> <a href='https://en.wikipedia.org/wiki/Random_forest'>Random Forests</a> are an extension of the classification trees. Rather than relying on just one tree, we can create a forest of random trees from the data and take the average across these fitted trees. This works as we are mostly focused on the prediction accuracy of the models. Due to the nature of the forest, the variance is decreased when compared to a single tree, though the interpretability is decreased significantly. These forests are created by randomly selecting a subset of the predictors for a bootstrap sample and creating a new tree from that sample. The trees are then all averaged together to create the forest prediction.</p>"),
                                 br(),
                                 HTML("<p>Some of the advantages of the random forest algorthim is that the trees are independent. This leads to lower variance overall in the model. Like the tree model, these forests have built-in feature selection. Outliers do not affect these models very easily, and they provide an answer for both linear and non-linear relationships. There are few disadvantages to this method. First, these forests are not easy to interpret. The variable importance can be seen but the actual interpretation is a black box. Another disadvantage is that these algorithms are very computationally intensive in comparison.</p>")
                        )
                    )
            ),
            
            # Model fitting tab content
            tabItem(tabName = "fit",
                    fluidRow(
                        box(width = 4, title = "Inputs", status = "danger", solidHeader = TRUE,
                            numericInput('trainProp',"Select Proportion of Data Set for Training:", value = 80, min = 10, max = 90, step = 5),
                            selectInput('modelChoice', "Select Model Type: ", c("All Models","Logistic Regression (GLM)", "Classification Tree", "Random Forest")),
                            checkboxGroupInput('expVar', "Select Explanatory Variables: ",
                                               names(data)[-length(names(data))], selected = names(data)[-length(names(data))], inline=FALSE),
                            actionLink("clearall2","Clear Variables"),
                            br(),br(),
                            numericInput('cvNum',"Select number of Cross-Validations:", 10, min = 5, max = 25, step = 5),
                            conditionalPanel('input.modelChoice != "Logistic Regression (GLM)"',
                                             numericInput('repNum',"Select number of CV Repeats:", 3, min = 1, max = 10, step = 1)
                            ),
                            actionButton('button1',"Run Models", icon("cog"))
                        ),
                        column(width=8,
                            box(title = "Generalized Linear Model", status = "success", solidHeader = TRUE, width = 8, collapsible = T,
                                h4("Model Summary"),
                                verbatimTextOutput("logSum"),
                                br(),
                                h4("Model Confusion Matrix"),
                                verbatimTextOutput("logConf")
                            ),
                            box(title = "Classification Tree", status = "primary", solidHeader = TRUE, width = 8, collapsible = T,
                                h4("Model Summary"),
                                verbatimTextOutput("treeSum"),
                                br(),
                                h4("Model Confusion Matrix"),
                                verbatimTextOutput("treeConf")
                            ),
                            box(title = "Random Forest", status = "warning", solidHeader = TRUE, width = 8, collapsible = T,
                                h4("Model Summary"),
                                verbatimTextOutput("forSum"),
                                br(),
                                h4("Model Confusion Matrix"),
                                verbatimTextOutput("forConf")
                            )
                        )
                    )
            ),
            
            # Prediction tab content
            tabItem(tabName = "predict",
                    fluidRow(
                        box(title="Prediction Inputs", status="danger",solidHeader = T,width = 8,
                            selectInput('predChoice', "Select Model Type: ", c("Logistic Regression (GLM)", "Classification Tree", "Random Forest")),
                            br(),
                            textOutput("outputVars"),
                            br(),
                            br(),                            
                            textInput("predVal", "Input values for above variables with a ';' between each and no space. For example, '8;Coors Field;2'"),
                            actionButton('button2',"Predict!", icon("search"))
                        ),
                        box("Prediction", status = "success", solidHeader=T,
                            verbatimTextOutput("outPred"))
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    # Data functions
    getNewData <- reactive({
        newData <- data[input$dataTable_rows_all, input$show_vars]
    })
    
    getData <- reactive({
        newData <- data[, input$show_vars]
    })
    
    trainTest <- reactive({
        # Split into train and test sets
        set.seed(144)
        data <- data[complete.cases(data), c("is_home_run",input$expVar)]
        trainNum <- sample(1:nrow(data), size = nrow(data)*(input$trainProp/100))
        testNum <- dplyr::setdiff(1:nrow(data), trainNum)
        trainData <- data[trainNum, ]
        testData <- data[testNum, ]
        print(colnames(trainData))
        list(trainData,testData)
        
    })
    
    glmFit <- reactive({
        logFit <- train(as.formula(paste("is_home_run", "~", paste(input$expVar, collapse = "+"))), data = trainData, family = "binomial",
                        method="glm", preProcess = c("center","scale"),
                        trControl = trainControl(method = "cv", number = input$cvNum),
                        na.action = na.pass)
    })
    
    classFit <- reactive({
        trctrl <- trainControl(method = "repeatedcv", number = input$cvNum, repeats = input$repNum)
        
        classTreeFit <- train(as.formula(paste("is_home_run", "~", paste(input$expVar, collapse = "+"))),
                              data = trainData, method = "rpart",
                              trControl=trctrl,
                              preProcess = c("center", "scale"),
                              na.action = na.pass)
    })
    
    randomFit <- reactive({
        trctrl <- trainControl(method = "repeatedcv", number = input$cvNum, repeats = input$repNum)
        ranForestFit <- train(as.formula(paste("is_home_run", "~", paste(input$expVar, collapse = "+"))),
                              data = trainData, method = "rf",
                              trControl=trctrl,
                              preProcess = c("center", "scale"),
                              na.action = na.pass)
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
            #data <<- getNewData()
            
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
    
    # Data Exploration tab functions
    output$allDataPlot <- renderPlot({
        plotData <- getPlotData()
        if(input$colorBy == "None"){
            legend = NULL
        } else{
            legend = plotData[,input$colorBy]
        }
        g <- ggplot(plotData, aes(x = game_month, y = is_home_run, fill = legend))
        g + geom_bar(stat="identity") + labs(x = "Month", y = "Home Runs") + labs(title="Home Runs by Month")
    })
    
    
    
    # Modeling tabs functions
    # Model info
    output$logEQ <- renderUI({
        withMathJax(helpText('$$P(1|x) = \\frac{e^{\\beta_0+\\beta_1x}}{1+e^{\\beta_0+\\beta_1x}}$$'))
    })
    
    # Model fitting
    observe({
        if(input$clearall2 == 0) return(NULL)
        else {
            updateCheckboxGroupInput(session,"expVar",  "Select Explanatory Variables: ", names(data)[-length(names(data))])
        }
    })
    
    observeEvent(input$button1, {
        withProgress(message = "Training Models", value = .25, {             

            splitData <- trainTest()
            trainData <<- splitData[[1]]
            testData <<- splitData[[2]]
            
            if(input$modelChoice == "All Models"){
    
                logFit <<- glmFit()
                incProgress(amount = 0.1, message = "Model Trained: Logistic Regression, Training: Classification Tree", detail = NULL,
                            session = getDefaultReactiveDomain())
                treeFit <<- classFit()
                incProgress(amount = 0.15, message = "Model Trained: Classification Tree, Training: Random Forest", detail = NULL,
                            session = getDefaultReactiveDomain())
                forestFit <<- randomFit()
                incProgress(amount = 0.25, message = "Model Trained: Random Forest", detail = NULL,
                            session = getDefaultReactiveDomain())
                incProgress(amount = 0.25, message = "All Models Trained", detail = NULL,
                            session = getDefaultReactiveDomain())
                
                output$logSum <- renderPrint({summary(logFit)})
                output$treeSum <- renderPrint({treeFit})
                output$forSum <- renderPrint({forestFit})
                
                output$logConf <- renderPrint({
                    p <- predict(logFit, dplyr::select(testData, -"is_home_run"))
                    logTbl <- confusionMatrix(p, testData$is_home_run)
                    logTbl
                })
                output$treeConf <- renderPrint({
                    p <- predict(treeFit, dplyr::select(testData, -"is_home_run"))
                    treeTbl <- confusionMatrix(p, testData$is_home_run)
                    treeTbl
                })
                output$forConf <- renderPrint({
                    p <- predict(forestFit, dplyr::select(testData, -"is_home_run"))
                    forTbl <- confusionMatrix(p, testData$is_home_run)
                    forTbl
                })
                
            } else if(input$modelChoice == "Logistic Regression (GLM)"){
                logFit <<- glmFit()
                incProgress(amount = 0.75, message = "Model Trained: Logistic Regression", detail = NULL,
                            session = getDefaultReactiveDomain())
                output$logSum <- renderPrint({summary(logFit)})
                output$logConf <- renderTable({
                    p <- predict(logFit, dplyr::select(testData, -"is_home_run"))
                    newTbl <-  confusionMatrix(p, testData$is_home_run)
                    newTbl
                })
            } else if(input$modelChoice == "Classification Tree"){
                treeFit <<- classFit()
                incProgress(amount = 0.75, message = "Model Trained: Classification Tree", detail = NULL,
                            session = getDefaultReactiveDomain())
                output$treeSum <- renderPrint({treeFit})
                output$treeConf <- renderTable({
                    p <- predict(treeFit, dplyr::select(testData, -"is_home_run"))
                    newTbl <-  confusionMatrix(p, testData$is_home_run)
                    newTbl
                })
            } else if(input$modelChoice == "Random Forest"){
                forestFit <<- randomFit()
                incProgress(amount = 0.75, message = "Model Trained: Random Forest", detail = NULL,
                            session = getDefaultReactiveDomain())
                outputforSum <- renderPrint({forestFit})
                output$forConf <- renderTable({
                    p <- predict(forestFit, dplyr::select(testData, -"is_home_run"))
                    newTbl <-  confusionMatrix(p, testData$is_home_run)
                    newTbl
                })
            } 
        
        })

    })
    
    # Model Prediction
    output$outputVars <- renderText({
        paste0("Variables Required: ", paste0(input$expVar, collapse = ";"))
    })
    
    observeEvent(input$button2, {
        if(input$predChoice == "Logistic Regression (GLM)"){
            model <- logFit
        } else if(input$predChoice == "Classification Tree"){
            model <- treeFit
        } else if(input$predChoice == "Random Forest"){
            model <- forestFit
        }
        
        inVal <- unlist(strsplit(input$predVal,";"))
        newVals <- data.frame(t(inVal))
        colnames(newVals) <- input$expVar
        for(n in colnames(newVals)){
            newVals[n] <- as(newVals[n],class(data[n]))
        }
        p <- predict(model, newVals)
    
        if(p == 1){
            p <- "A Home Run!"
        } else{
            p <- "Not a Home Run"
        }
        output$outPred <- renderPrint({
            print(paste0("The prediction for your input is: ", p[1]))
        })
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

