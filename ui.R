#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  titlePanel(title = "Statistical Tests"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(      
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
                   selected = ","),
      hr(),
      checkboxInput("additional", "Multiple files", FALSE),
      conditionalPanel(
        condition = "input.additional",
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
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      tabsetPanel(type = "tabs",
                  tabPanel("Show input dataset", 
                           h3("Dataset"),
                           column(12, dataTableOutput('contents')),
                           conditionalPanel(
                             condition = "input.additional",
                             hr(),
                             h3("Additional dataset"),
                             column(12, dataTableOutput('contents.table2'))
                           )),
                  tabPanel("Test results",
                           radioButtons("checkboxParadigm", "Select Kind of test",
                                        choices = c("Parametric", "Non-Parametric", "Bayesian"),
                                        selected = "Parametric"),
                           fluidRow(selectInput('test', "Test", 
                                                choices = list("None" = 0)),
                                    conditionalPanel("(!input.additional) &&  (input.checkboxParadigm == \"Bayesian\")",
                                                     fluidRow(checkboxInput("checkboxPlot", "Plot", FALSE),
                                                     downloadLink('downloadPlot', 'Download')))
                           ),
                           hr(), 
                           h4("Table Output"),
                           dataTableOutput("table.test.result"),
                           hr(),
                           h4("Latex Output"),
                           verbatimTextOutput("tex.test.result"),
                           conditionalPanel("(!input.additional) &&  (input.checkboxParadigm == \"Bayesian\") && (input.checkboxPlot)",
                                            plotOutput("plot.test"))
                           )
    ))
))




