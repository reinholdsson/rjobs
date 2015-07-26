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
  },
    options = list(paging = F),
    callback = "function(table) {
      table.on('click.dt', 'tr', function() {
        $(this).toggleClass('selected');
        Shiny.onInputChange('rows',
        table.rows('.selected').indexes().toArray());
      });
    }"
  )
  
  refreshOnClick <- reactive({
    if (input$refresh == 0) return()
    message('refresh')
  })
  
  observe({
    if (input$add == 0) return()
    job_id <- create_job(input$conn, input$query, input$desc)
    message(sprintf('%s added ...', job_id))
  })
  
  output$tbl <- renderDataTable(
    mtcars,
    options = list(pageLength = 10),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').indexes().toArray());
    });
}"
  )
  output$rows_out <- renderText({
    paste(c('You selected these rows on the page:', input$rows),
          collapse = ' ')
  })
  
})