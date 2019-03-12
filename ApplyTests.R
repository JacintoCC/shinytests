

apply.parametric.test <- function(df, test, wide.format,
                                  result.var, comparison.var, scenario.var, 
                                  firstCompGroup = 0, secondCompGroup = 0, ...){
  results <- switch (test,
                     "ANOVA" = {
                       if(wide.format){
                         long.df <- reshape2::melt(df)
                         results.anova <- unlist(summary(aov(value ~ variable, long.df)))
                       }
                       else{
                         if(scenario.var != "None"){
                           df <- dplyr::select_(df, .dots=paste("-",scenario.var))
                         }
                         if(!is.null(grouping.var)){
                           df <- dplyr::select_(df, .dots=paste("-",grouping.var))
                         }
                         
                         form <- as.formula(paste(result.var,comparison.var, sep="~"))
                         results.anova <- unlist(summary(aov(form, df)))
                       }
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
                     "t-test" = {
                       if(wide.format){
                         t.test(df[ ,firstCompGroup], df[ ,secondCompGroup])
                       }
                       else{
                         group.1 <- dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", firstCompGroup, "'", sep = "")),
                                                   result.var)
                         group.2 <- dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", secondCompGroup, "'", sep = "")),
                                                   result.var)
                         t.test(group.1, group.2)
                       }
                     }
  )
  return(results)
}

apply.non.parametric.test <- function(df, test, wide.format, 
                                      result.var, comparison.var, scenario.var, grouping.var,
                                      firstCompGroup = 0, secondCompGroup = 0,
                                      post.hoc = FALSE, post.hoc.method, 
                                      post.hoc.comparison, ...){
  
  if(post.hoc & test %in% c("Friedman", "FriedmanAR", "Quade")){
    
    if(!wide.format){
      df <-  tidyr::spread(df, comparison.var, result.var)
      
      if(scenario.var != "None"){
        df <- dplyr::select_(df, .dots=paste("-",scenario.var))
      }
      if(!is.null(grouping.var)){
        df <- dplyr::select_(df, .dots=paste("-",grouping.var))
      }
    }
    
    if(post.hoc.comparison == "All vs All"){
      results <- switch (test,
                         "Friedman" = scmamp::friedmanPost(df),
                         "FriedmanAR" = scmamp::friedmanAlignedRanksPost(df),
                         "Quade" = scmamp::quadePost(df))
      if(post.hoc.method %in% c("Li","Holland","Rom")){
        results[lower.tri(results)] <- NA
      }
      results <- switch(post.hoc.method,
                        "Bergmann-Hommel" = scmamp::adjustBergmannHommel(results),
                        "Shaffer" = scmamp::adjustShaffer(results),
                        "Li" = scmamp::adjustLi(results), 
                        "Holland" = scmamp::adjustHolland(results), 
                        "Rom" = scmamp::adjustRom(results))
      # imguR::imguR()
      plotCD(df)
      plot <- recordPlot()
      # dev.off()
      return(list(results = results,
                  plot = plot))
    }
    else{
      results <- switch (test,
                         "Friedman" = scmamp::friedmanPost(df, ...),
                         "FriedmanAR" = scmamp::friedmanAlignedRanksPost(df, ...),
                         "Quade" = scmamp::quadePost(df, ...))
      results <- switch(post.hoc.method,
                        "Li" = scmamp::adjustLi(results), 
                        "Holland" = scmamp::adjustHolland(results), 
                        "Rom" = scmamp::adjustRom(results))
    }
  }
  else{
    if(wide.format){
      first.sample <- unlist(dplyr::select_(df,firstCompGroup))
      second.sample <- unlist(dplyr::select_(df,secondCompGroup))
      m = cbind(first.sample, second.sample)
    }
    else{
      first.sample <- dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", firstCompGroup, "'", sep = "")),
                                     result.var)
      second.sample <- dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", secondCompGroup, "'", sep = "")),
                                      result.var)
      m = cbind(unlist(first.sample), unlist(second.sample))
      
      df <- tidyr::spread_(df,  comparison.var, result.var)
      for (gv in grouping.var){
        df <- dplyr::select_(df, paste0("-",gv))
      }
    }
    
    results <- switch (test,
                       "Wilcoxon" = rNPBST::wilcoxon.test(m),
                       "WilcoxonRS" = rNPBST::wilcoxonRankSum.test(m),
                       "Friedman" = rNPBST::friedman.test(df),
                       "FriedmanAR" = rNPBST::friedmanAR.test(df),
                       "Iman-Davenport" = rNPBST::imanDavenport.test(df),
                       "Quade" = quade.test(as.matrix(df,
                                                      dimnames = list(as.character(1:nrow(df)),
                                                                      LETTERS[1:ncol(df)]))))
  }
  
  return(results)
}


apply.bayesian.test <- function(df, test, wide.format, 
                                result.var, comparison.var, scenario.var, grouping.var,
                                firstCompGroup = 0, secondCompGroup = 0, ...){
  
  if(wide.format){
    first.sample <- unname(unlist(dplyr::select_(df,firstCompGroup)))
    second.sample <- unname(unlist(dplyr::select_(df,secondCompGroup)))
    m = cbind(first.sample, second.sample)
  }
  else{
    if(scenario.var != "None"){
      df <- dplyr::select_(df, .dots=paste("-",scenario.var))
    }

    first.sample <- unname(unlist(dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", firstCompGroup, "'", sep = "")),
                                   result.var)))
    second.sample <- unname(unlist(dplyr::select_(dplyr::filter_(df, paste(comparison.var,"== '", secondCompGroup, "'", sep = "")),
                                    result.var)))
    m = cbind(first.sample,second.sample)
    
    df <- tidyr::spread_(df,  comparison.var, result.var)
    df <- dplyr::select_(df, .dots= paste0("-",grouping.var))
  }
  
  results <- switch (test,
                     "BayesFriedman" = rNPBST::bayesianFriedman.test(df, imprecise = T),
                     "Sign" = rNPBST::bayesianSign.test(first.sample, second.sample),
                     "Signed-Rank" = rNPBST::bayesianSignedRank.test(first.sample, second.sample),
                     "Corr-t-test" = rNPBST::bayesianCorrelatedT.test(first.sample, second.sample),
                     "IDP-Wilcoxon" = rNPBST::bayesian.imprecise(first.sample, second.sample))
  return(results)
}

apply.test <- function(df, paradigm, test, wide.format, ...){
  switch (paradigm,
          "Parametric" = {
            results <- apply.parametric.test(df, test, wide.format,...)
          },
          "Non-Parametric" = {
            results <- apply.non.parametric.test(df, test, wide.format,...)
          },
          "Bayesian" = {
            results <- apply.bayesian.test(df, test, wide.format,...)
          }
  )
  return(results)
}

apply.test.two.datasets <- function(df1, df2, paradigm, test, ...){
  results <- switch(paradigm,
                    "Non-Parametric" = {
                      switch(test,
                             "GLRT-Multiple-Measures" = 
                               rNPBST::multipleMeasuresGLRT(df1, df2),
                             "Page" = {
                               rNPBST::page.test(df1, df2)
                             })
                    },
                    "Bayesian" = {
                      switch(test,
                             "Bayesian-Multiple-Measures" = 
                               rNPBST::bayesianMultipleConditions.test(df1, df2))
                    })
}