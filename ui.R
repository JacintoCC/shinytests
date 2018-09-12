#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(shinydashboard)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title = "Statistical Tests"
  ),
  dashboardSidebar(
      checkboxInput("defaultdataset", "Default datasets", TRUE),
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
      # CUSTOM FILE
      conditionalPanel(
        condition = "(!input.defaultdataset)",
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
        condition = "(input.checkboxParadigm == \"Parametric\" && input.test == \"t-test\") || 
        (input.checkboxParadigm == \"Non-Parametric\" && (input.test == \"Wilcoxon\" || input.test == \"WilcoxonRS\")) ||
        (input.checkboxParadigm == \"Bayesian\" && (input.test == \"Sign\" || input.test == \"Signed-Rank\" || input.test == \"IDP-Wilcoxon\"))",
        numericInput("columnfirst", "First column", 1, min = 1, max = 10, step = 1),
        numericInput("columnsecond", "Second column", 2, min = 1, max = 10, step = 1)
      ),
      hr(),
      # ADDITIONAL FILE
      checkboxInput("additional", "Multiple files", FALSE),
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
      ),
      hr(),
      radioButtons("checkboxParadigm", "Select Kind of test",
                   choices = c("Parametric", "Non-Parametric", "Bayesian"),
                   selected = "Parametric"),
      selectInput('test', "Test", 
                  choices = list("None" = 0)),
      # CONDITIONAL PANEL POST-HOC
      conditionalPanel(
        condition = "input.checkboxParadigm ==  \"Non-Parametric\" && input.test == \"Friedman\" || input.test == \"FriedmanAR\" || input.test == \"Quade\"",
        checkboxInput("PostHoc", "Test Post-hoc", FALSE),
        conditionalPanel(
          condition = "input.PostHoc",
          radioButtons("posthoccomparison", "Comparison",
                       choices = c(OneVersusAll = "One vs All",
                                   AllVersusAll = "All vs All"),
                       selected = "One vs All"),
          conditionalPanel("input.posthoccomparison == \"One vs All\"",
                           numericInput("controlalgorithm", "Control Algorithm", 
                                        1, min = 1, max = 10, step = 1)),
          selectInput("posthocmethod", "Post-hoc method",
                      choices = list("None" = 0)
        )
      ))
    ),
    
    ###
    #    Main panel for displaying outputs
    ###
  dashboardBody(
    tabBox(id = "body",
           width = 12, 
           tabPanel("Show input dataset",  
                    h3("Dataset"),
                    dataTableOutput('contents'),
                    conditionalPanel(
                      condition = "input.additional", hr(), h3("Additional dataset"),
                      dataTableOutput('contents.table2')
                    )),
           tabPanel("Test results",
                    h4("Table Output"),
                    dataTableOutput("table.test.result"),
                    hr(),
                    h4("Latex Output"),
                    verbatimTextOutput("tex.test.result"),
                    h4("References"),
                    textOutput("test.reference")

           ),
           tabPanel("Plot Output",
                    conditionalPanel(
                      condition = "(!input.additional) &&  (input.checkboxParadigm == \"Bayesian\") && (input.test == \"Sign\" || input.test == \"Signed-Rank\" || input.test == \"Corr-t-test\" || input.test == \"IDP-Wilcoxon\")",
                      textInput("textDataset", "Name Dataset", "Dataset"),
                      textInput("textFirstAlgorithm", "Name First Algorithm", "1st Alg."),
                      textInput("textSecondAlgorithm", "Name Second Algorithm", "2nd Alg."),
                      # downloadLink('downloadPlot', 'Download'),
                      plotOutput("plot.test")),
                    conditionalPanel(
                      condition = "!((!input.additional) &&  (input.checkboxParadigm == \"Bayesian\") && (input.test == \"Sign\" || input.test == \"Signed-Rank\" || input.test == \"Corr-t-test\" || input.test == \"IDP-Wilcoxon\"))",
                      h5("Plot output not available for the selected test")
                    )
           )
    )
  )
)
