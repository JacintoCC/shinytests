#  List of required packages 
list.of.packages <- c("ggplot2","reshape2","devtools","ggtern","dplyr","shinydashboard")

# Checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

if(!("scmamp" %in% installed.packages()[,"Package"])){
  devtools::install_bioc("graph")
  devtools::install_bioc("Rgraphviz")
  install.packages("scmamp")
}


if(!("rNPBST" %in% installed.packages()[,"Package"])){
  devtools::install_github("JacintoCC/rNPBST")
}

library(ggplot2)
library(reshape2)
library(devtools)
library(ggtern)
library(dplyr)
library(shinydashboard)
library(scmamp)
library(rNPBST)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title = "Statistical Tests"
  ),
  dashboardSidebar(
      checkboxInput("defaultdataset", "Default datasets", TRUE),
      checkboxInput("checkwideformat", "Wide Format", TRUE),
      # DEFAULT DATASET
      conditionalPanel(
        condition = "(input.defaultdataset)",
        selectInput('default.file1', "Dataset", 
                    choices = list("results" = "results",
                                   "results.knn" = "results.knn",
                                   "results.lr" = "results.lr",
                                   "results.nb" = "results.nb",
                                   "results.nnet" = "results.nnet",
                                   "results.rf" = "results.rf"))
      ),
      conditionalPanel(
        condition = "(!input.checkwideformat)",
        selectInput("selectComparisonVariable", "Select Comparison Factor Variable", choices = NULL),
        selectInput("selectResultVariable", "Select Result Variable", choices = NULL),
        selectInput("selectScenarioVariable", "Select Scenario Factor Variable", choices = NULL),
        conditionalPanel("(input.selectScenarioVariable !=\"None\")",
                         selectInput("selectScenarioValue", "Select Scenario Value", choices = NULL)),
        checkboxGroupInput("checkBlockingVariable", "Select Blocking Variable")
      ),
      # CUSTOM FILE
      conditionalPanel(
        condition = "(!input.defaultdataset && !input.checkboxwide)",
        fileInput("file1", "Choose main CSV file",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        checkboxInput("rownames", "Rownames", FALSE),
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 Space = " "),
                     selected = ",")
      ),
      conditionalPanel(
        # condition = "(input.pair == \"Parametric\" && input.test == \"t-test\") || 
        # (input.checkboxParadigm == \"Non-Parametric\" && (input.test == \"Wilcoxon\" || input.test == \"WilcoxonRS\")) ||
        # (input.checkboxParadigm == \"Bayesian\" && (input.test == \"Corr-t-test\" || input.test == \"Sign\" || input.test == \"Signed-Rank\" || input.test == \"IDP-Wilcoxon\"))",
        condition = "output.paircomparisontest",
        selectInput("firstCompGroup", "First Comparison Group", choices = NULL, selected = NULL),
        selectInput("secondCompGroup", "Second Comparison Group",choices = NULL, selected = NULL)
      ),
      # ADDITIONAL FILE
      conditionalPanel(condition = "(input.checkwideformat)",
                       hr(),
                       checkboxInput("additional", "Multiple files", FALSE)),
      conditionalPanel(
        condition = "input.additional",
        checkboxInput("defaultdataset2", "Default datasets", TRUE),
        conditionalPanel(
          condition = "(input.defaultdataset2)",
          selectInput('default.file2', "Dataset", 
                      choices = list("results" = "results",
                                     "results.knn" = "results.knn",
                                     "results.lr" = "results.lr",
                                     "results.nb" = "results.nb",
                                     "results.nnet" = "results.nnet",
                                     "results.rf" = "results.rf"))
        ),
        conditionalPanel(condition = "(!input.defaultdataset2)",
        
        fileInput("file2", "Choose additional CSV file",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Input: Checkbox if file has header ----
        checkboxInput("header2", "Header", TRUE),
        checkboxInput("rownames2", "Rownames", FALSE),
        # Input: Select separator ----
        radioButtons("sep2", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 Space = " "),
                     selected = ",")
        )
      )
    ),
    
    ###
    #    Main panel for displaying outputs
    ###
  dashboardBody(
    tabBox(id = "body",
           width = 12, 
           tab.dataset,
           tab.result, 
           tab.plot
           
    )
  )
)
