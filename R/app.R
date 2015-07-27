
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
  
  shinyApp(
    ui = dashboardPage(skin = 'black',
      dashboardHeader(titleWidth = 275,
        title = 'R A D H O C'
      ),
      dashboardSidebar(width = 275,
        selectInput('conn', 'Connection:', conns),
        textInput('desc', 'Job description:'),
        HTML('<label for = "query">Query:</label>'),
        aceEditor('query', '', mode = 'sql', theme = 'github', fontSize = 12),
        HTML('<center>'), actionButton('add', ' Add', icon = icon('plus')), HTML('</center>')
      ),
      dashboardBody(
        fluidRow(
          box(width = 12,
            actionButton('delete', ' Delete', icon = icon('minus')),
            actionButton('start', ' Start', icon = icon('play')),
            actionButton('refresh', ' Refresh', icon = icon('refresh')),
            downloadButton('downloadData', ' Download')
          ),
          box(width = 12,
            dataTableOutput(outputId = 'jobs_table'),
            tags$style(type="text/css", '#jobs_table tfoot {display:none;}'),
            textOutput('refresh_time')
          )
        )
      )
    ),
    server = function(input, output, session) {
      output$jobs_table <- renderDataTable({
        refreshOnClick()
        info()
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
          df_to_xlsx(get_jobs_output(isolate(selected_jobs())), file)
      })
    }
  )
}
  