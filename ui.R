rm(list = ls())
library(shiny)
library(shinyAce)
library(rjobs)
conns <- names(yaml.load_file('config.yml')$connections)

shinyUI(bootstrapPage(
  selectInput('conn', 'Connection:', conns),
  textInput('desc', 'Job Description:'),
  aceEditor('query', '', mode = 'sql', theme = 'github'),
  
  actionButton('add', 'add', icon = icon('plus')),
  actionButton('refresh', 'refresh', icon = icon('refresh')),

  dataTableOutput(outputId = 'jobs_table')
))