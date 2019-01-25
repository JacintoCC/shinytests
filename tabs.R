tab.dataset <- tabPanel("Show input dataset",  
                        h3("Dataset"),
                        dataTableOutput('contents'),
                        conditionalPanel(
                          condition = "input.additional", hr(), h3("Additional dataset"),
                          dataTableOutput('contents.table2')
                        ))


tab.result <- tabPanel("Test results",
                       h4("Table Output"),
                       dataTableOutput("table.test.result"),
                       hr(),
                       h4("Latex Output"),
                       verbatimTextOutput("tex.test.result"),
                       h4("References"),
                       textOutput("test.reference"))


tab.plot <- tabPanel("Plot Output",
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
                     ))