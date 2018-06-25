require(shiny)
require(ggplot2)
require(reshape2)
require(devtools)
require(ggtern)

if("rNPBST" %in% rownames(installed.packages())){
  library(rNPBST)
} else {
  devtools::install_github("JacintoCC/rNPBST")
}

format.table <- function(l){
  if("sample" %in% names(l)){
    l$sample <- NULL
  }
  if("dist" %in% names(l)){
    l$dist <- NULL
  }
  names.l <- names(l)
  max.length <- max(sapply(l, length))
  which.vector <- sapply(l, length) > 1
  table <- matrix("", ncol = max.length + 1, nrow = length(l) + sum(which.vector))
  for(i in 1:length(l)){
    index.i <- i + ifelse(i == 1, 0, sum(which.vector[1:(i-1)]))
    table[index.i, 1] <- names.l[i]
    if(which.vector[i]){
      if(is.null(names(l[[i]]))){
        names.li <- rep("", times = length(l[[i]]))
      }
      else{
        names.li <- names(l[[i]])
      }
      table[index.i, seq(2, along.with = l[[i]])] <- names.li
      table[index.i + 1, seq(2, along.with = l[[i]])] <- unname(l[[i]])
    }
    else{
      table[index.i, 2] <- l[[i]]
    }
  }
  colnames(table) <- c("Attribute", paste("Value", 1:max.length))
  return(table)
}

apply.parametric.test <- function(df, test, columnfirst = 0, columnsecond = 0){
  results <- switch (test,
          "ANOVA" = {
            long.df <- reshape2::melt(df)
            results.anova <- unlist(summary(aov(value ~ variable, long.df)))
            results.anova <- matrix(results.anova, nrow = 2,
                                    dimnames = list( Variable = c("variable","residuals"),
                                                     Value = c("Df","SumSq","MeanSq", "Fvalue","Pr(>F)")))
            results.anova <- rNPBST::make.htest(method = "ANOVA test",
                                                p.value = results.anova[1,5],
                                                df = results.anova[,1],
                                                SumSq = results.anova[,2],
                                                MeanSq = results.anova[,3],
                                                Fvalue = results.anova[,4])
          },
          "t-test" = t.test(df[ ,columnfirst], df[ ,columnsecond])
          )
  return(results)
}

apply.non.parametric.test <- function(df, test, columnfirst = 0, columnsecond = 0){
  results <- switch (test,
                     "Wilcoxon" = rNPBST::wilcoxon.test(df[,c(columnfirst,columnsecond)]),
                     "WilcoxonRS" = rNPBST::wilcoxonRankSum.test(df[,c(columnfirst,columnsecond)]),
                     "Friedman" = rNPBST::friedman.test(df),
                     "FriedmanAR" = rNPBST::friedmanAR.test(df),
                     "Iman-Davenport" = rNPBST::imanDavenport.test(df),
                     "Quade" = quade.test(as.matrix(df,
                                                    dimnames = list(as.character(1:nrow(df)),
                                                                    LETTERS[1:ncol(df)]))))
  return(results)
}


apply.bayesian.test <- function(df, test, columnfirst = 0, columnsecond = 0){
  results <- switch (test,
          "BayesFriedman" = rNPBST::bayesianFriedman.test(df, imprecise = T),
          "Sign" = rNPBST::bayesianSign.test(df[ ,columnfirst], df[ ,columnsecond]),
          "Signed-Rank" = rNPBST::bayesianSignedRank.test(df[ ,columnfirst], df[ ,columnsecond]),
          "Corr-t-test" = rNPBST::bayesianCorrelatedT.test(df[ ,columnfirst], df[ ,columnsecond]))
  return(results)
}

apply.test <- function(df, paradigm, test, ...){
  switch (paradigm,
          "Parametric" = {
            results <- apply.parametric.test(df, test,...)
          },
          "Non-Parametric" = {
            results <- apply.non.parametric.test(df, test,...)
          },
          "Bayesian" = {
            results <- apply.bayesian.test(df, test,...)
          }
  )
  return(results)
}

apply.test.two.datasets <- function(df1, df2, paradigm, test, ...){
  results <- switch(paradigm,
                    "Non-Parametric" = {
                      switch(test,
                             "GLRT-Multiple-Measures" = 
                               rNPBST::multipleMeasuresGLRT(df1, df2))
                    },
                    "Bayesian" = {
                      switch(test,
                             "Bayesian-Multiple-Measures" = 
                               rNPBST::bayesianMultipleConditions.test(df1, df2))
                    })
}





####
#' Server
#' Define server logic to read selected file
####

server <- function(input, output,session) {
  # Read first dataset
  df.reactive <- function(){
    if(!input$defaultdataset){
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
      return(df)
    }
    else{
      return(switch(input$default.file1,
             "results" = rNPBST::results,
             "results.knn" = rNPBST::results.knn,
             "results.lr" = rNPBST::results.lr,
             "results.nb" = rNPBST::results.nb,
             "results.nnet" = rNPBST::results.nnet,
             "results.rf" = rNPBST::results.rf))
    }
  }
  # Read additional dataset
  df2.reactive <- function(){
    if(input$additional){
      req(input$file2)
      
      df2 <- read.csv(input$file2$datapath,
                      header = input$header2,
                      sep = input$sep2)
      return(df2)
    }
  }
  
  # Show input table
  output$contents <- renderDataTable(df.reactive()) 
  output$contents.table2 <- renderDataTable(df2.reactive()) 
  
  # Update tests
  observeEvent(input$additional,{
    updateNumericInput(session, "columnfirst", value = 1, min = 1, max = ncol(df.reactive()))
    updateNumericInput(session, "columnsecond", value =  2, min = 1, max = ncol(df.reactive()))
    # Update available tests if two files
    if(input$additional){
      updateRadioButtons(session, inputId =  'checkboxParadigm', 
                         label =  "Select Kind of test",
                         choices = c("Non-Parametric", "Bayesian"),
                         selected = "Non-Parametric")
      updated.list <- switch(input$checkboxParadigm,
                             "Non-Parametric" = list("GLRT-Multiples Measures" = "GLRT-Multiple-Measures"),
                             "Bayesian" =  list("Multiple Measures" = "Bayesian-Multiple-Measures"))
      updated.selected <- switch(input$checkboxParadigm,
                                 "Non-Parametric" = "GLRT-Multiple-Measures",
                                 "Bayesian" =  "Bayesian-Multiple-Measures")
      updateSelectInput(session, inputId = 'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
    else{
      updateRadioButtons(session, inputId =  'checkboxParadigm', 
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
                                    "Correlated t-test" = "Corr-t-test")
                             })
      updated.selected <- switch(input$checkboxParadigm,
                                 "Parametric" = "ANOVA",
                                 "Non-Parametric" = "Friedman",
                                 "Bayesian" = "BayesFriedman")
      updateSelectInput(session, inputId =  'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
  })
  
  observeEvent(input$checkboxParadigm,{
    updateNumericInput(session, "columnfirst", value = 1, min = 1, max = ncol(df.reactive()))
    updateNumericInput(session, "columnsecond", value =  2, min = 1, max = ncol(df.reactive()))
    # Update available tests if two files
    if(input$additional){
      updated.list <- switch(input$checkboxParadigm,
                             "Non-Parametric" = list("GLRT-Multiples Measures" = "GLRT-Multiple-Measures"),
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
                                    "Correlated t-test" = "Corr-t-test")
                             })
      updated.selected <- switch(input$checkboxParadigm,
                                 "Parametric" = "ANOVA",
                                 "Non-Parametric" = "Friedman",
                                 "Bayesian" = "BayesFriedman")
      updateSelectInput(session, inputId =  'test', label = "Test",  
                        choices = updated.list, selected = updated.selected)
    }
  })
  
  
  # Compute Test
  reactive.test <- reactive({
    if(ncol(df.reactive() >= 2)){
      if(input$additional)
        test.result <- apply.test.two.datasets(df.reactive(), df2.reactive(), 
                                               input$checkboxParadigm, 
                                               input$test)
      else{
        test.result <- apply.test(df.reactive(), input$checkboxParadigm, input$test, 
                                  columnfirst = input$columnfirst, columnsecond = input$columnsecond)
      }
    }
    return(test.result)
  })
  
  # Plot Test
  reactive.plot <- reactive({
    if(ncol(df.reactive()) >= 2 &&
       !input$additional && 
       input$checkboxParadigm == "Bayesian" && 
       input$test %in% c("Sign", "Signed-Rank", "Corr-t-test") &&
       input$checkboxPlot){
      
      if (input$test == "Corr-t-test"){
        
        plot <- rNPBST::plotPosterior(reactive.test(), 
                                      names = c(input$textFirstAlgorithm, 
                                                input$textSecondAlgorithm), 
                                      dataset = input$textDataset) 
        return(plot)
      }
      else if (input$test == "Sign"){
        plot <- rNPBST::plotSimplex(reactive.test()$sample) +
          ggtitle("Sign test") +
          labs(z = input$textFirstAlgorithm, x = input$textSecondAlgorithm)
        print(plot)
        return(NULL)
      }
      else if (input$test == "Signed-Rank"){
        plot <- rNPBST::plotSimplex(reactive.test()$sample) +
          ggtitle("Signed-Rank test") + 
          labs(z = input$textFirstAlgorithm, x = input$textSecondAlgorithm)
        print(plot)
        return(NULL)
      }
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('plot-', Sys.Date(), '.png', sep='')
    },
    content = function(con) {
      ggplot2::ggsave(plot = renderPlot(reactive.plot()), filename = con)
    }
  )

  output$tex.test.result <- renderText(rNPBST::htest2Tex(reactive.test()))
  output$table.test.result <- renderDataTable(format.table(reactive.test()))
  output$plot.test <- renderPlot(reactive.plot())
}