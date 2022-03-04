#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, session) {
    
    # DONNEES ----
    
    df_fire <- read.csv("data/forestfires.csv")
    
    baseData <- reactiveValues(df = df_fire)

    # Charger les données
    
    observe({
      req(input$fileInput$datapath)
      oriData <- read.csv(file = input$fileInput$datapath,
                          sep = input$sepcol,
                          quote = input$quote,
                          dec = input$sepdec,
                          stringsAsFactor = FALSE,
                          check.names = FALSE)
      baseData$df <- oriData
    })
    
    # show table
    output$contentstable <- renderDataTable(options = list(pageLength = 10), expr = {
      baseData$df
    })
    
    # download data
    output$downloaddata <- downloadHandler(
      filename = "data.csv",
      content = function(file) {
        if (input$csvtype == "anglo")
          {
          write.csv(x = baseData$df, file = file, row.names = FALSE)
        } else {
          write.csv2(x = baseData$df, file = file, row.names = FALSE)}
      })
    
    
    
    observe({
      columnList <- c("", colnames(baseData$df))
      quantitative <- c("", colnames(Filter(is.numeric, baseData$df)))
      qualitative <-  c("", colnames(baseData$df[,!(names(baseData$df) %in% colnames(Filter(is.numeric, baseData$df)))]))
      
      updateSelectInput(session = session,
                        inputId = "idtab",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "uniquanti",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "uniquali",
                        choices = qualitative)
      updateSelectInput(session = session,
                        inputId = "qualidep",
                        choices = qualitative)
      updateSelectInput(session = session,
                        inputId = "qualiindep",
                        choices = qualitative)
      updateSelectInput(session = session,
                        inputId = "quantidep",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "quantiindep",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "quanlidep",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "quanliindep",
                        choices = qualitative)
      updateSelectInput(session = session,
                        inputId = "regmultdep",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "regmultindep",
                        choices = quantitative)
      updateSelectInput(session = session,
                        inputId = "cahvar",
                        choices = quantitative)
            
    })
    
    # FILTER ROWS ----
    
    observeEvent(input$addfilter, {
      tempTab <- try(baseData$df %>% filter_(input$filterrow))
      if(is.data.frame(tempTab)){
        baseData$df <- tempTab
      } else {
        baseData$df <- baseData$df
      }
    })
    
    observeEvent(input$delfilter, {
      if(colnames(baseData$df)[1] == "day"){
        baseData$df <- tabFinal
      } else {
        baseData$df <- oriData
      }
    })
    
    
    
    # UNIVARIE ----
    
    # summary
    output$unisummary <- renderText({
      if (input$uniquanti != "" & input$uniquali == ""){
        textResult <- paste("Nb. obs. = ", nrow(baseData$df), "<br/>",
                            "Valeurs manquantes = ", anyNA(baseData$df[, input$uniquanti]), "<br/>",
                            "Moyenne = ", round(mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                            "Médiane = ", round(median(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                            "Variance = ", round(var(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                            "Coef. de variation = ", round(sd(baseData$df[, input$uniquanti], na.rm = TRUE) / mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2),
                            sep = "")
        return(textResult)
        
      } else if (input$uniquanti == "" & input$uniquali != "") {
        textResult <- paste("Nb. obs. = ", nrow(baseData$df), "<br/>",
                            "Valeurs manquantes = ", anyNA(baseData$df[, input$uniquali]),
                            sep = "")
        return(textResult)
      } else {
        return()
      }
    })
    
    # plot univariate
    output$unitab <- renderTable(include.rownames = FALSE, expr = {
      if (input$uniquanti != "" & input$uniquali == ""){
        return()
      } else if (input$uniquanti == "" & input$uniquali != "") {
        if (length(unique(baseData$df[, input$uniquali])) > 30){
          stop("La variable sélectionnée n'est probablement pas qualitative")
        } else {
          tabFreq <- as.data.frame(table(baseData$df[, input$uniquali]))
          tabFreq$Perc <- round(100 * tabFreq$Freq / sum(tabFreq$Freq), digits = 2)
          colnames(tabFreq) <- c("Modalité", "Freq. absolue", "Freq. relative")
          return(tabFreq)
        }
      } else if (input$uniquanti == "" & input$uniquali == ""){
        return()
      } else {
        stop("Sélectionner une seule variable")
      }
    })
    
    # plot univariate
    output$uniplot <- renderPlot({
      if (input$uniquanti != "" & input$uniquali == ""){
        Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary)
      } else if (input$uniquanti == "" & input$uniquali != "") {
        if (length(unique(baseData$df[, input$uniquali])) > 30){
          stop("La variable sélectionnée n'est probablement pas qualitative")
        } else {
          Barplot(df = baseData$df, varquali = input$uniquali)
        }
      } else if (input$uniquanti == "" & input$uniquali == ""){
        return()
      } else {
        stop("Sélectionner une seule variable")
      }
    })
    
    # download plot
    output$downloaduniplot <- downloadHandler(
      filename = "uniplot.svg",
      content = function(file) {
        svg(file, width = input$widthuni / 2.54, height = input$heightuni / 2.54, pointsize = 8)
        if (input$uniquanti != "" & input$uniquali == ""){
          print(Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary))
        } else if (input$uniquanti == "" & input$uniquali != "") {
          print(Barplot(df = baseData$df, varquali = input$uniquali))
        } else {
          return()
        }
        dev.off()
      })
    
    # BIVARIE : QUALI-QUALI ----
    
    # Print mosaic plot
    output$mosaic <- renderPlot({
      req(input$qualiindep, input$qualidep)
      if((length(unique(baseData$df[, input$qualidep])) - 1) * (length(unique(baseData$df[, input$qualiindep])) - 1) > 150){
        stop("La ou les variables sélectionnées ne sont probablement pas qualitatives (ddl > 150)")
      } else {
        Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep)
      }
    })
    
    # contingency table
    output$contingtab <- renderTable({
      req(input$qualiindep, input$qualidep)
      chiResults <- chisq.test(baseData$df[, input$qualidep], baseData$df[, input$qualiindep])
      if(input$contcont == "obsfreq"){
        return(chiResults$observed)
      }
      else if(input$contcont == "rowpct"){
        return(100 * prop.table(table(baseData$df[, input$qualidep], baseData$df[, input$qualiindep]), margin = 1))
      }
      else if(input$contcont == "expfreq"){
        return(round(chiResults$expected, digits = 0))
      }
      else if(input$contcont == "rawresid"){
        return(chiResults$observed - chiResults$expected)
      }
      else if(input$contcont == "stdresid"){
        return(chiResults$residuals)
      }
    })
    
    # contingency text
    output$contingtext <- renderText({
      req(input$qualiindep, input$qualidep)
      chiResults <- chisq.test(baseData$df[, input$qualidep], baseData$df[, input$qualiindep])
      textResult <- paste("Chi2 = ", round(chiResults$statistic, 2), "<br/>",
                          "Phi = ", round(sqrt(chiResults$statistic / nrow(baseData$df)), 2), "<br/>",
                          "V de Cramer = ", round(sqrt(chiResults$statistic / (nrow(baseData$df) * min(dim(chiResults$observed)))), 2), 
                          sep = "")
      return(textResult)
    })
    
    # download plot
    output$downloadmosaicplot <- downloadHandler(
      filename = "mosaicplot.svg",
      content = function(file) {
        svg(file, width = input$widthmosaic / 2.54, height = input$heightmosaic / 2.54, pointsize = 8)
        req(input$qualiindep, input$qualidep)
        print(Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep))
        dev.off()
      })
    
    # BIVARIE : QUANTI-QUANTI ----
    
    # print scatter plot
    output$scatterplot <- renderPlot({
      req(input$quantiindep, input$quantidep)
      ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep)
    })
    
    # compute linear regression
    regMod <- reactive({
      req(input$quantiindep, input$quantidep)
      ComputeRegression(df = baseData$df, vardep = input$quantidep, varindep = input$quantiindep)
    })
    
    # print coefficients
    output$coefreg <- renderTable(include.rownames = FALSE, expr = {
      req(input$quantiindep, input$quantidep)
      regMod()$TABCOEF
    })
    
    # download plot
    output$downloadreg1 <- downloadHandler(
      filename = "regressionplot.svg",
      content = function(file) {
        svg(file, width = input$widthreg1 / 2.54, height = input$heightreg1 / 2.54, pointsize = 8)
        req(input$quantiindep, input$quantidep)
        print(ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep))
        dev.off()
      })
    
    
    # BIVARIE : QUALI-QUANTI ----
    
    # print boxplot
    output$boxes <- renderPlot({
      req(input$quanliindep, input$quanlidep)
      Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter)
    })
    
    # print aovplot
    output$aovplot <- renderPlot({
      req(input$quanliindep, input$quanlidep)
      AnovaPlot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
    })
    
    # compute linear regression
    aovMod <- reactive({
      req(input$quanliindep, input$quanlidep)
      ComputeRegression(df = baseData$df, vardep = input$quanlidep, varindep = input$quanliindep)
    })
    
    # print coefficients
    output$coefanova <- renderTable(include.rownames = FALSE, expr = {
      req(input$quanliindep, input$quanlidep)
      rbind(aovMod()$TABVAR, aovMod()$TABCOEF)
    })
    
    # print mean and variance
    output$tabanova <- renderTable(include.rownames = FALSE, expr = {
      req(input$quanliindep, input$quanlidep)
      AnovaTab(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
    })
    
    # download plot
    output$downloadanova <- downloadHandler(
      filename = "anovaplot.svg",
      content = function(file) {
        svg(file, width = input$widthanova1 / 2.54, height = input$heightanova1 / 2.54, pointsize = 8)
        req(input$quanliindep, input$quanlidep)
        print(grid.arrange(Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter),
                           AnovaPlot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep),
                           nrow = 2))
        dev.off()
      })
    
    
    
    
    # MULTIVARIE : REGRESSION ----
    
    # Compute linear regression
    
    regmultMod <- reactive({
      if (input$regmultdep != "" & !is.null(input$regmultindep)){
        ComputeRegressionMult(df = baseData$df, vardep = input$regmultdep, varindep = input$regmultindep)
      } else {
        return()
      }
    })
    
    # Print matrix
    output$matcor <- renderTable(include.rownames = TRUE, expr = {
      if (input$regmultdep != "" & !is.null(input$regmultindep)){
        regmultMod()$MATCOR
      } else {
        return()
      }
    })
    
    # Print coefficients
    
    output$coefregmult <- renderTable(include.rownames = FALSE, expr = {
      if (input$regmultdep != "" & !is.null(input$regmultindep)){
        regmultMod()$TABCOEF
      } else {
        return()
      }
    })

    # MULTIVARIE : CLASSIFICATION ----
    
    # Compute hierarchical clustering
    
    clusterComp <- eventReactive(input$buttoncah, {
      ComputeClassif(df = baseData$df, varquanti = input$cahvar, stand = input$cahstandardize, method = input$cahmethod)
    })
    
    # Plot dendrogram
    
    output$cahdendro <- renderPlot({
      req(clusterComp)
      withProgress(min = 0, message = "Calcul en cours", expr = {
        PlotDendro(classifobj = clusterComp())})
    })
    
    # Plot heigth
    
    output$cahheight <- renderPlot({
      req(clusterComp)
      PlotHeight(classifobj = clusterComp())
    })
    
    # Plot profile
    
    output$cahprofile <- renderPlot({
      req(clusterComp)
      PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE
    })
    
    
    # download plots
    output$downloadclus <- downloadHandler(
      filename = "clusplot.svg",
      content = function(file) {
        svg(file, width = input$widthclus / 2.54, height = input$heightclus / 2.54, pointsize = 8)
        req(clusterComp)
        print(grid.arrange(PlotDendro(classifobj = clusterComp()),
                           PlotHeight(classifobj = clusterComp()),
                           PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE,
                           nrow = 3))
        dev.off()
      })
    
    
    
    
    
    output$table <- DT::renderDataTable({
      DT::datatable(cars)
    })
    
    
    
  })
