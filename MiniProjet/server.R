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
    
    df_fire <- read.csv("data/forestfires.csv")
    
    baseData <- reactiveValues(df = df_fire)
    
    # DONNEES ----
    
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
        if (input$csvtype == "anglo"){
          write.csv(x = baseData$df, file = file, row.names = FALSE)
        } else {
          write.csv2(x = baseData$df, file = file, row.names = FALSE)
        }
      })
    
    
    
    observe({
      columnList <- c("", colnames(baseData$df))
      
      updateSelectInput(session = session,
                        inputId = "idtab",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "idshape",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "uniquanti",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "uniquali",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "qualidep",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "qualiindep",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "quantidep",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "quantiindep",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "quanlidep",
                        choices = columnList)
      updateSelectInput(session = session,
                        inputId = "quanliindep",
                        choices = columnList)
            
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
    
    output$summary <- renderPrint({
      summary(cars)
    })
    
    
    
    output$table <- DT::renderDataTable({
      DT::datatable(cars)
    })
    
    
    
  })
