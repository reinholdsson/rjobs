
#' @export
job_app <- function(config = '~/.rjobs.yaml') {
  # rm(list = ls())
  require(shiny)
  require(shinyAce)
  require(rjobs)
  library(shinydashboard)
  
  init(config)
  conns <- names(suppressWarnings(yaml.load_file(config)$connections))
  
  js_click_callback <- function(input_var) {
    sprintf("function(table) {
      table.on('click.dt', 'tr', function() {
        if ($(this).hasClass('selected') ) {
            $(this).removeClass('selected');
        } else {
            $(this).addClass('selected');
        }
        Shiny.onInputChange('%s',
          table.rows('.selected').data()
        );
      });
    }", input_var)
  }
  
  swapply <- function(x, ...) lapply(as.character(x), function(i) switch(i, ...))
  
  shinyApp(
    ui = dashboardPage(skin = 'black',
      dashboardHeader(titleWidth = 400,
        title = 'R A D H O C'
      ),
      dashboardSidebar(width = 400,
        HTML('<center>'),
        selectInput('conn', 'Data Source:', conns),
        textInput('desc', 'Description:'),
        HTML('<label for = "query">Query:</label>'),
        HTML('</center>'),
        aceEditor('query', '', mode = 'sql', theme = 'github', fontSize = 12),
        HTML('<center>'),
        actionButton('add', ' Add', icon = icon('plus')),
        HTML('</center>')
      ),
      dashboardBody(
        fluidRow(
          box(title = 'Jobs', width = 12,
            dataTableOutput(outputId = 'jobs_table'),
            tags$style(type="text/css", '#jobs_table tfoot {display:none;}')
          ),
          box(title = 'Actions', width = 6,
            actionButton('delete', ' Delete', icon = icon('minus')),
            actionButton('start', ' Start', icon = icon('play')),
            actionButton('refresh', ' Refresh', icon = icon('refresh')),
            downloadButton('downloadData', ' Download'),
            br(), br(),
            textOutput('refresh_time')
          ),
          box(title = 'Query', width = 6,
            verbatimTextOutput('job_info')
          )
        )
      )
    ),
    server = function(input, output, session) {
      output$jobs_table <- renderDataTable({
        refreshOnClick()
        dt <- info()
        dt[, .(
          `Job Id` = job_id,
          `Added At` = as.character(as.Date(created_at)),
          `Data Source` = conn,
          `Description` = desc,
          `Status` = swapply(status,
            'ended' = as.character(icon('check')),
            'error' = as.character(icon('exclamation-circle')),
            'started' = as.character(icon('spinner')),
            'created' = as.character(icon('circle-thin')),
            ''
          )
        )]
      },
        options = list(
          paging = T,
          searching = T
        ),
        callback = js_click_callback('jobs')
      )
      
      output$refresh_time <- renderText({
        refreshOnClick()
        sprintf('Last refresh at %s', Sys.time())
      })
      
      output$job_info <- renderText({
        job_id <- head(selected_jobs(), 1)
        if (length(job_id) == 1) {
          job <- get_job_attr(job_id, c('query', 'message'))
          sprintf('%s%s', job$query, ifelse(is.null(job$message), '', paste0('\n\n', job$message)) )
        } else return()
      })
      
      refreshOnClick <- reactive({
        if (input$refresh == 0) return()
        message('refresh')
      })
      
      observe({
        print(input$add)
        if (input$add == 0) return()
        job_id <- create_job(isolate(input$conn), isolate(input$query), isolate(input$desc))
        message(sprintf('%s added ...', job_id))
      })
      
      observe({
        if (input$start == 0) return()
        jobs <- isolate(selected_jobs())
        if (length(jobs) > 0) {
          for (job_id in jobs) {
            start_job(job_id)
          }
        } else message('no jobs selected ...')
      })
      
      observe({
        if (input$delete == 0) return()
        jobs <- isolate(selected_jobs())
        if (length(jobs) > 0) {
          for (job_id in jobs) {
            delete_job(job_id)
          }
        } else message('no jobs selected ...')
      })
      
      selected_jobs <- reactive({
        j <- input$jobs
        sapply(j[str_is_num(names(j))], function(i) i[[1]])
      })
      
      output$downloadData <- downloadHandler(
        filename = function() { paste('myreport', '.xlsx', sep='') },
        content = function(file) {
          jobs <- isolate(selected_jobs())
          lst <- get_jobs_output(jobs)
          lst$SOURCE_INFO <- info()[job_id %in% names(lst)]
          df_to_xlsx(lst, file)
      })
    }
  )
}
  