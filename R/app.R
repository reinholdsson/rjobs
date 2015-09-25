get_table_info <- function(conn) {
  tryCatch({
    output <- switch(conn,
      'exasol' = {
        message('exasol!')
        mtcars
      },
      'postgres' = {
        message('postgresql!')
        require(rpsql)
        a <- psql()
        a$fetch('SELECT DISTINCT pgc.relname as table, a.attname as name, format_type(a.atttypid, a.atttypmod) as type, a.attnotnull as not_null, com.description as comment, coalesce(i.indisprimary,false) as primary_key, def.adsrc as default FROM pg_attribute a JOIN pg_class pgc ON pgc.oid = a.attrelid LEFT JOIN pg_index i ON (pgc.oid = i.indrelid AND i.indkey[0] = a.attnum) LEFT JOIN pg_description com on (pgc.oid = com.objoid AND a.attnum = com.objsubid) LEFT JOIN pg_attrdef def ON (a.attrelid = def.adrelid AND a.attnum = def.adnum) WHERE a.attnum > 0 AND pgc.oid = a.attrelid AND pg_table_is_visible(pgc.oid) AND NOT a.attisdropped ORDER BY 1, 2')
      },
      stop('Unsupported connection ...')
    )
  }, error = function(e) {
    message('ERROR table info', e)
  }, finally = {
    message('Table info ...')
  })
  
  return(output)
}

#' @export
job_app <- function(config = '~/.rjobs.yaml') {
  # rm(list = ls())
  require(shiny)
  require(shinyAce)
  require(rjobs)
  library(shinydashboard)
  
  init(config)
  config_lst <- suppressWarnings(yaml.load_file(config))
  conns <- names(config_lst$connections)
  
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
  
  space <- function(x, ...) { 
    format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
  }
  
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
        actionButton('add_run', ' Run', icon = icon('play')),
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
          ),
          box(title = 'Database Columns & Tables', width = 12,
            dataTableOutput(outputId = 'table_info')
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
          `Finished At` = ifelse(status == 'ended', ended_at, NA),
          `Query Time (s)` = ifelse(status == 'ended',
            round(as.numeric(difftime(as.POSIXct(ended_at), as.POSIXct(started_at), units = 'secs')), 1),
            NA
          ),
          ` ` = swapply(status,
            'ended' = as.character(icon('check')),
            'error' = as.character(icon('exclamation-circle')),
            'started' = as.character(icon('spinner')),
            'created' = as.character(icon('circle-thin')),
            NA
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
        print(input$add_run)
        if (input$add_run == 0) return()
        job_id <- create_job(isolate(input$conn), isolate(input$query), isolate(input$desc))
        message(sprintf('%s added ...', job_id))
        start_job(job_id)
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
      
      output$table_info <- renderDataTable({
        get_table_info(config_lst$connections[[input$conn]]$src)
      }, options = list(pageLength = 10))
      
      output$downloadData <- downloadHandler(
        filename = function() { paste('myreport', '.xlsx', sep='') },
        content = function(file) {
          jobs <- isolate(selected_jobs())
          lst <- get_jobs_output(jobs)
          lst$SOURCE_INFO <- info()[job_id %in% jobs]
          df_to_xlsx(lst, file)
      })
    }
  )
}
  