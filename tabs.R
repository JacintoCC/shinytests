tab.dataset <- tabPanel("Show input dataset",  
                        h3("Dataset"),
                        dataTableOutput('contents'),
                        conditionalPanel(
                          condition = "input.additional", hr(), h3("Additional dataset"),
                          dataTableOutput('contents.table2')
                        ))


tab.result <- tabPanel("Test",
                       h4("Test Selection"),
                       fluidRow(
                         column(3, offset = 2,
                                radioButtons("checkboxParadigm", "Select Kind of test",
                                             choices = c("Parametric", "Non-Parametric", "Bayesian"),
                                             selected = "Parametric"),
                                selectInput('test', "Test", 
                                            choices = list("None" = 0)),
                                conditionalPanel(
                                  condition = "input.checkboxParadigm ==  \"Non-Parametric\" && input.test == \"Friedman\" || input.test == \"FriedmanAR\" || input.test == \"Quade\"",
                                  checkboxInput("checkPostHoc", "Perform Post-Hoc Test", FALSE))),
                         column(3, 
                                # CONDITIONAL PANEL POST-HOC
                                conditionalPanel(
                                  condition = "input.checkboxParadigm ==  \"Non-Parametric\" && (input.test == \"Friedman\" || input.test == \"FriedmanAR\" || input.test == \"Quade\") && input.checkPostHoc",
                                  radioButtons("posthoccomparison", "Comparison",
                                               choices = c(OneVersusAll = "One vs All",
                                                           AllVersusAll = "All vs All"),
                                               selected = "One vs All"),
                                  conditionalPanel("input.posthoccomparison == \"One vs All\"",
                                                   selectInput("controlalgorithm", "Control Algorithm", choices = NULL, selected = NULL)),
                                  selectInput("posthocmethod", "Post-hoc method",
                                              choices = list("None" = 0))))),
                       h4("Table Output"),
                       dataTableOutput("table.test.result"),
                       hr(),
                       h4("Latex Output"),
                       verbatimTextOutput("tex.test.result"),
                       h4("References"),
                       textOutput("test.reference"))


tab.plot <- tabPanel("Plot Output",
                     conditionalPanel(
                       condition = "output.plotAvailable",
                       conditionalPanel("input.checkboxParadigm == \"Bayesian\"",
                                        textInput("textDataset", "Name Dataset", "Dataset"),
                                        textInput("textFirstAlgorithm", "Name First Algorithm", "1st Alg."),
                                        textInput("textSecondAlgorithm", "Name Second Algorithm", "2nd Alg.")),
                       # downloadLink('downloadPlot', 'Download'),
                       plotOutput("plot.test")),
                     conditionalPanel(
                       condition = "!output.plotAvailable",
                       h5("Plot output not available for the selected test")
                     ))