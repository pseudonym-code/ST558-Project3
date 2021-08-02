# ST558-Project3
Solution for Project 3 in ST558

## Description
This applet allows for exploration into what factors lead to Home Runs (HR) in baseball. You can do this through exploring the data, altering the data set, and modeling using multiple different methods. The data set includes a number of variables about both the individual match-up of pitcher vs. batter, as well as the ballpark dimensions which often play a large part in the number of HR hit. The data set used comes from kaggle.com, a popular data science competition and collaboration website. We are using a randomized subset of the data for this applet.

### Packages Needed
Below are the packages required to run the app on your local machine. Installation code for running prior to running the applet is below.
* shiny
* dashboardthemes
* plotly
* shinydashboard
* tidyverse
* DT
* caret
* ROCR
* Metrics

#### Installation Code
```{r]
install.packages(c("shiny","dashboardthemes","plotly","shinydashboard","tidyverse","DT","caret","ROCR","Metrics"))
```

### Code to Run the App
Run this code to open the applet on your local machine.
```{r}
shiny::runGitHub("ST558-Project3", "pseudonym-code", ref='main')
```
