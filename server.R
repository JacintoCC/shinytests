require(shiny)

require(ggplot2)
require(reshape2)
require(devtools)
require(ggtern)
require(scmamp)


if("rNPBST" %in% rownames(installed.packages())){
  library(rNPBST)
} else {
  devtools::install_github("JacintoCC/rNPBST")
}


source('ApplyTests.R')
source('FormatOutput.R')


####
#' Server
#' Define server logic
####

server <- function(input, output, session) {
  # Read first dataset
  df <- reactive({
    if(!input$defaultdataset){
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    }
    else{    
      df <- switch(input$default.file1,
                   "results" = rNPBST::results,
                   "results.knn" = rNPBST::results.knn,
                   "results.lr" = rNPBST::results.lr,
                   "results.nb" = rNPBST::results.nb,
                   "results.nnet" = rNPBST::results.nnet,
                   "results.rf" = rNPBST::results.rf,
                   "CEC17.final" = rNPBST::cec17.final,
                   "CEC17.mean" = rNPBST::cec17.mean)
    }
    
    return(df)
  })
  
  # Read additional dataset
  df2 <- reactive({
    if(input$additional){
      if(!input$defaultdataset2){
        df2 <- read.csv(input$file2$datapath,
                       header = input$header2,
                       sep = input$sep2)
        return(df2)
      }
      else{
        return(switch(input$default.file2,
                      "results" = rNPBST::results,
                      "results.knn" = rNPBST::results.knn,
                      "results.lr" = rNPBST::results.lr,
                      "results.nb" = rNPBST::results.nb,
                      "results.nnet" = rNPBST::results.nnet,
                      "results.rf" = rNPBST::results.rf))
      }
    }
  })
  
  # Update default datasets if format changes
  observeEvent(input$checkwideformat,{
    if(input$checkwideformat){
      updated.list <- list("results" = "results",
                           "results.knn" = "results.knn",
                           "results.lr" = "results.lr",
                           "results.nb" = "results.nb",
                           "results.nnet" = "results.nnet",
                           "results.rf" = "results.rf")
      updated.selected <- "results"
    }
    else{ 
      updated.list <- list("CEC17.final" = "CEC17.final", "CEC17.mean" = "CEC17.mean")
      updated.selected <- "CEC17.final"
    }
    
    updateSelectInput(session, inputId = 'default.file1', label = "Test",  
                      choices = updated.list, 
                      selected = updated.selected)
    
  })
  
  # Reactive Values
  long.format.vars <- reactiveValues(value = NULL)
  comp.group <- reactiveValues(value = NULL)
  
  
  # Update Select Variables
  observeEvent(df(),{
    df <- df()
    long.format.vars$comp <- colnames(df)[1]
    long.format.vars$result <- colnames(df)[ncol(df)]
    long.format.vars$scenario <- "None"
    long.format.vars$block <- colnames(df)[colnames(df) != long.format.vars$comp & 
                                             colnames(df) != long.format.vars$result &
                                             colnames(df) != long.format.vars$scenario]
    
    
    updateSelectInput(session, inputId = 'selectComparisonVariable', 
                      choices = colnames(df),
                      selected = long.format.vars$comp)
    updateSelectInput(session, inputId = 'selectResultVariable', 
                      choices = colnames(df),
                      selected = colnames(df)[ncol(df)])
    updateSelectInput(session, inputId = 'selectScenarioVariable', 
                      choices = c("None", colnames(df)),
                      selected = long.format.vars$scenario)
    updateSelectInput(session, inputId = 'selectScenarioValue',
                      choices = NULL,
                      selected = NULL)
    updateCheckboxGroupInput(session, inputId = 'checkBlockingVariable', 
                             choices = unlist(long.format.vars$block),
                             selected = NULL)
  })
  
  
  # Update Reactive values if input changes
  observeEvent(input$selectComparisonVariable,{
    long.format.vars$comp <- input$selectComparisonVariable
  })
  observeEvent(input$selectResultVariable,{
    long.format.vars$result <- input$selectResultVariable
  })
  observeEvent(input$selectScenarioVariable,{
    long.format.vars$scenario <- input$selectScenarioVariable
  })
  
  # Change Blocking variables
  observeEvent(c(long.format.vars$comp,
                 long.format.vars$result,
                 long.format.vars$scenario), 
               {
    df <- df()
    long.format.vars$block <- colnames(df)[colnames(df) != long.format.vars$comp & 
                                             colnames(df) != long.format.vars$result &
                                             colnames(df) != long.format.vars$scenario]
    
    updateCheckboxGroupInput(session, inputId = 'checkBlockingVariable', 
                             choices = unlist(long.format.vars$block),
                             selected = NULL)
  })
  observeEvent(long.format.vars$scenario,{
    df <- df()
    if(long.format.vars$scenario != "None" && stringi::stri_length(long.format.vars$scenario) > 0){
        updateSelectInput(session, inputId = 'selectScenarioValue',
                          choices = c("All", unique(dplyr::select_(df,long.format.vars$scenario))),
                          selected = "All")
    }
  })
  # Change groups for pair comparison tests
  observeEvent(long.format.vars$comp,{
    df <- df()
    
    if(input$checkwideformat){
      updated.group.choices <- colnames(df)
    }
    else{ 
      updated.group.choices <- levels(df[, long.format.vars$comp])
    }
    
    updateSelectInput(session, "firstCompGroup", 
                      choices = updated.group.choices,
                      selected = updated.group.choices[1])
    updateSelectInput(session, "secondCompGroup", 
                      choices = updated.group.choices,
                      selected = updated.group.choices[2])
    
    updateSelectInput(session, "controlalgorithm", 
                      choices = updated.group.choices,
                      selected = updated.group.choices[1])
  })
  output$paircomparisontest <- reactive({
    pair.test <- (input$checkboxParadigm == "Parametric" && input$test == "t-test") || 
        (input$checkboxParadigm == "Non-Parametric" && input$test %in% c("Wilcoxon", "WilcoxonRS")) ||
        (input$checkboxParadigm == "Bayesian" && input$test %in% c("Corr-t-test", "Sign", "Signed-Rank", "IDP-Wilcoxon"))
    return(pair.test)
  })
  outputOptions(output, "paircomparisontest", suspendWhenHidden = FALSE)
  
  # Update names for plot
  observeEvent(input$default.file1,{
    updateTextInput(session, "textDataset", "Name Dataset", deparse(input$default.file1))
  })
  observeEvent(input$firstCompGroup,{
    comp.group$first <- input$firstCompGroup
    updateTextInput(session, "textFirstAlgorithm", "Name First Algorithm", input$firstCompGroup)
  })
  observeEvent(input$secondCompGroup,{
    comp.group$second <- input$secondCompGroup
    updateTextInput(session, "textSecondAlgorithm", "Name Second Algorithm", input$secondCompGroup)
  })
  
  
  # Show input table
  output$contents <- renderDataTable(df()) 
  output$contents.table2 <- renderDataTable(df2()) 

  # Update available tests if there is an aditional file
  observeEvent(input$additional,{
    # Update available tests if two files
    if(input$additional){
      updateRadioButtons(session, inputId =  'checkboxParadigm', 
                         label =  "Select Kind of test",
                         choices = c("Non-Parametric", "Bayesian"),
                         selected = "Non-Parametric")
      updated.list <- switch(input$checkboxParadigm,
                             "Non-Parametric" = list("GLRT-Multiples Measures" = "GLRT-Multiple-Measures",
                                                     "Page" = "Page"),
                             "Bayesian" =  list("Multiple Measures" = "Bayesian-Multiple-Measures"))
      updated.selected <- switch(input$checkboxParadigm,
                                 "Non-Parametric" = "GLRT-Multiple-Measures",
                                 "Bayesian" =  "Bayesian-Multiple-Measures")
      updateSelectInput(session, inputId = 'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
    else{
      updateRadioButtons(session, inputId = 'checkboxParadigm', 
                         label =  "Select Kind of test",
                         choices = c("Parametric","Non-Parametric", "Bayesian"),
                         selected = "Parametric")
      updated.list <- switch(input$checkboxParadigm,
                             "Parametric" = list("ANOVA" = "ANOVA", "t-test" = "t-test"),
                             "Non-Parametric" = {
                               list("Wilcoxon" = "Wilcoxon", 
                                    "Wilcoxon Rank-Sum" = "WilcoxonRS",
                                    "Friedman" = "Friedman", 
                                    "Friedman Aligned-Rank" = "FriedmanAR",
                                    "Iman-Davenport" = "Iman-Davenport", 
                                    "Quade" = "Quade")
                             },
                             "Bayesian" = {
                               list("Friedman" = "Friedman", 
                                    "Sign test" = "Sign",
                                    "Signed-rank" = "Signed-Rank",
                                    "Correlated t-test" = "Corr-t-test",
                                    "IDP-Wilcoxon" = "IDP-Wilcoxon")
                             })
      updated.selected <- switch(input$checkboxParadigm,
                                 "Parametric" = "ANOVA",
                                 "Non-Parametric" = "Friedman",
                                 "Bayesian" = "BayesFriedman")
      updateSelectInput(session, inputId =  'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
  })
  
  # Update available tests if paradigm changes
  observeEvent(input$checkboxParadigm,{
    # Update available tests if two files
    if(input$additional){
      updated.list <- switch(input$checkboxParadigm,
                             "Non-Parametric" = list("GLRT-Multiples Measures" = "GLRT-Multiple-Measures",
                                                     "Page" = "Page"),
                             "Bayesian" =  list("Multiple Measures" = "Bayesian-Multiple-Measures"))
      updated.selected <- switch(input$checkboxParadigm,
                                 "Non-Parametric" = "GLRT-Multiple-Measures",
                                 "Bayesian" =  "Bayesian-Multiple-Measures")
      updateSelectInput(session, inputId = 'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
    else{
      updated.list <- switch(input$checkboxParadigm,
                             "Parametric" = list("ANOVA" = "ANOVA", "t-test" = "t-test"),
                             "Non-Parametric" = {
                               list("Wilcoxon" = "Wilcoxon", 
                                    "Wilcoxon Rank-Sum" = "WilcoxonRS",
                                    "Friedman" = "Friedman", 
                                    "Friedman Aligned-Rank" = "FriedmanAR",
                                    "Iman-Davenport" = "Iman-Davenport", 
                                    "Quade" = "Quade")
                             },
                             "Bayesian" = {
                               list("Friedman" = "BayesFriedman", 
                                    "Sign test" = "Sign",
                                    "Signed-rank" = "Signed-Rank",
                                    "Correlated t-test" = "Corr-t-test",
                                    "IDP-Wilcoxon" = "IDP-Wilcoxon")
                             })
      updated.selected <- switch(input$checkboxParadigm,
                                 "Parametric" = "ANOVA",
                                 "Non-Parametric" = "Friedman",
                                 "Bayesian" = "BayesFriedman")
      updateSelectInput(session, inputId =  'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
  })
  
  # Update available post-hoc test
  observeEvent(input$posthoccomparison,{
      updated.list <- switch(input$posthoccomparison,
                             "One vs All" = list("Li" = "Li",
                                                 "Holland" = "Holland", 
                                                 "Rom" = "Rom"),
                             "All vs All" = list("Shaffer",
                                                 "Bergmann-Hommel",
                                                 "Li" = "Li", 
                                                 "Holland" = "Holland", 
                                                 "Rom" = "Rom"))
      updated.selected <- switch(input$posthoccomparison,
                                 "One vs All" = "Li",
                                 "All vs All" = "Shaffer")
      updateSelectInput(session, inputId =  'posthocmethod', label = "Post-hoc method",  
                        choices = updated.list, selected = updated.selected)
  })
  
  # See if it is possible to compute the test with current input
  check.possible <- reactive({
    df <- df()
    
    is.possible <-ncol(df >= 2) &&
      (input$checkwideformat ||
         (!input$checkwideformat && 
            (ncol(df) - 2 - ifelse(is.null(input$checkBlockingVariable), 
                                   0, length(input$checkBlockingVariable))) == 
            ifelse(long.format.vars$scenario != "None", 1, 0))
      )
    return(is.possible)
  })
  
  # See if there is an associated plot
  check.plot <- reactive({
    exists.plot <- (ncol(df()) >= 2 &&
                      !input$additional && 
                      input$checkboxParadigm == "Bayesian" && 
                      input$test %in% c("Sign", "Signed-Rank", "Corr-t-test", "IDP-Wilcoxon"))
    return(exists.plot)
  })
  
  # COMPUTE TEST
  reactive.test <- reactive({
    df <- df()
    df2 <- df2()
    
    first.comp.group <- comp.group$first
    second.comp.group <- comp.group$second
    
    
    if(input$additional){
      test.result <- apply.test.two.datasets(df, df2, 
                                             input$checkboxParadigm, 
                                             input$test)
    }
    else{
      # Filter dataset by scenario
      if(!input$checkwideformat && long.format.vars$scenario != "None" && input$selectScenarioValue != "All"){
        df <- dplyr::filter_(df, paste(long.format.vars$scenario, "==", input$selectScenarioValue))
        df <- dplyr::select_(df, paste0("-",long.format.vars$scenario))
      }
      
      test.result <- apply.test(df, input$checkboxParadigm, input$test, 
                                wide.format = input$checkwideformat,
                                comparison.var = long.format.vars$comp,
                                scenario.var = long.format.vars$scenario,
                                result.var = long.format.vars$result,
                                grouping.var = input$checkBlockingVariable,
                                firstCompGroup = first.comp.group, secondCompGroup = second.comp.group,
                                post.hoc = input$PostHoc, 
                                post.hoc.method = input$posthocmethod,
                                post.hoc.comparison = input$posthoccomparison,
                                control = input$controlalgorithm)
    }
    
    return(test.result)
  })
  
  # Plot Test
  reactive.plot <- reactive({
    check.possible <- check.possible()
    check.plot <- check.plot()
    
    if(check.possible && check.plot){
      
      test <- reactive.test()
      plot <- plot(test)
      
      if (input$test == "Corr-t-test"){
        plot <- plot + ggplot2::ggtitle(paste(input$textFirstAlgorithm,  "vs.",
                                              input$textSecondAlgorithm,
                                              "\nDataset:", input$textDataset))
        return(plot)
      }
      else if (input$test == "Sign"){
        plot <- plot + ggtitle("Sign test") +
          labs(z = input$textFirstAlgorithm, x = input$textSecondAlgorithm)
      }
      else if (input$test == "Signed-Rank"){
        plot <- plot +
          ggtitle("Signed-Rank test") + 
          labs(z = input$textFirstAlgorithm, x = input$textSecondAlgorithm)
        
      }
      else if (input$test == "IDP-Wilcoxon"){
        plot <- plot(test)
      }
      
      print(plot)
      return(NULL)
    }
  })
  # Print Test Output in Table
  reactive.table.output <- reactive({
    # Check if is possible to execute the test
    check.possible <- check.possible()
    
    if(check.possible){
      
      test <- reactive.test()
      
      if(input$checkboxParadigm ==  "Non-Parametric" && 
         input$test %in% c("Friedman", "FriedmanAR", "Quade") &&
         input$PostHoc){
        test
      }
      else{
        format.table(test)
      }
    }
    else{
      return("Test cannot be computed with current input")
    }
    
  })
  # Print Test Output in TeX Format
  reactive.text.output <- reactive({
    # Check if is possible to execute the test
    check.possible <- check.possible()
    
    if(check.possible){
      
      test <- reactive.test()
      
      if(input$checkboxParadigm ==  "Non-Parametric" && 
         input$test %in% c("Friedman", "FriedmanAR", "Quade") &&
         input$PostHoc){
        toString(xtable::xtable(reactive.test(), caption = "Post-hoc test",
                                label = "tab:post-hoc"))
      }
      else{
        rNPBST::htest2Tex(test)
      }
    }
    else{
      return("Test cannot be computed with current input")
    }
    
  })

  
  output$table.test.result <- renderDataTable({reactive.table.output()})
  output$tex.test.result <- renderText({reactive.text.output()})
  output$plot.test <- renderPlot({reactive.plot()})
  
  # UPDATE REFERENCES
  output$test.reference <- reactive({switch(input$checkboxParadigm,
    "Parametric" ={
      switch(input$test,
             "ANOVA" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.",
             "t-test" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.")
      },                              
    "Non-Parametric" = {
      switch (input$test,
              "Wilcoxon" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.", 
              "WilcoxonRS" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.",
              "Friedman" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.", 
              "FriedmanAR" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.",
              "Iman-Davenport" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.", 
              "Quade" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.",
              "Page" = "Sheskin, D. J., Handbook of parametric and nonparametric statistical procedures (2003), : crc Press.",
              "GLRT-Multiples-Measures" = "de Campos, C. P., & Benavoli, A., Joint Analysis of Multiple Algorithms and Performance Measures, New Generation Computing, 35(1), 69–86 (2016).  http://dx.doi.org/10.1007/s00354-016-0005-8")
    },
    "Bayesian" = {
      switch (input$test,
              "BayesFriedman" = "Benavoli, A., Corani, G., Mangili, F., & Zaffalon, M., A Bayesian nonparametric procedure for comparing algorithms, In , Proceedings of the 32nd International Conference on Machine Learning, ICML 2015, Lille, France, 6-11 July 2015 (pp. 1264–1272) (2015).", 
              "Sign" = "Benavoli, A., Corani, G., Dem\v sar, Janez, & Zaffalon, M., Time for a Change: a Tutorial for Comparing Multiple Classifiers Through Bayesian Analysis, Journal of Machine Learning Research, 18(77), 1–36 (2017). ",
              "Signed-Rank" = "Benavoli, A., Corani, G., Dem\v sar, Janez, & Zaffalon, M., Time for a Change: a Tutorial for Comparing Multiple Classifiers Through Bayesian Analysis, Journal of Machine Learning Research, 18(77), 1–36 (2017). ",
              "Corr-t-test" = "Corani, G., & Benavoli, A., A Bayesian approach for comparing cross-validated algorithms on multiple data sets, Machine Learning, 100(2-3), 285–304 (2015).",
              "Bayesian-Multiple-Measures" = "de Campos, C. P., & Benavoli, A., Joint Analysis of Multiple Algorithms and Performance Measures, New Generation Computing, 35(1), 69–86 (2016)",
              "IDP-Wilcoxon" = "Benavoli, A., Mangili, F., Ruggeri, F., & Zaffalon, M. (2015). Imprecise Dirichlet Process With Application to the Hypothesis Test on the Probability That X < Y. Journal of Statistical Theory and Practice, 9(3), 658–684. http://dx.doi.org/10.1080/15598608.2014.985997")
      })})
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('plot-', Sys.Date(), '.png', sep='')
  #   },
  #   content = function(con) {
  #     ggplot2::ggsave(plot = renderPlot(reactive.plot()), filename = con)
  #   }
  # )
}