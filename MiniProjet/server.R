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
    
    # show table
    output$contentstable <- renderDataTable(options = list(pageLength = 10), expr = {
      baseData$df
    })
    
    output$summary <- renderPrint({
      summary(cars)
    })
    
    
    
    output$table <- DT::renderDataTable({
      DT::datatable(cars)
    })
    
    
    
  })
