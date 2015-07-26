rm(list = ls())
library(shiny)
library(shinyAce)
library(rjobs)

init('config.yml')

shinyServer(function(input, output, session) {
  output$jobs_table <- renderDataTable({
    refreshOnClick()
    DT <- info()
    DT
  }, options = list(paging = F))
  
  refreshOnClick <- reactive({
    if (input$refresh == 0) return()
    message('refresh')
  })
  
  observe({
    if (input$add == 0) return()
    job_id <- create_job(input$conn, input$query, input$desc)
    message(sprintf('%s added ...', job_id))
  })
  
})