#!/usr/local/bin/Rscript
suppressMessages(require(rredis))
suppressMessages(require(yaml))

argv <- commandArgs(trailingOnly = TRUE)
config <- argv[1]
job_id <- argv[2]

cfg <- suppressWarnings(yaml.load_file(config))
do.call('redisConnect', cfg$redis)

job <- redisHGetAll(job_id)
job$status <- 'started'
job$started_at <- Sys.time()
job$process_id <- Sys.getpid()
redisHMSet(job_id, job)
redisHDel(job_id, 'ended_at')

#Sys.sleep(30)
tryCatch({
  job$output <- switch(cfg$connections[[job$conn]]$src,
    'exasol' = {
      message('exasol!')
      mtcars
    },
    'postgres' = {
      message('postgresql!')
      require(rpsql)
      a <- psql()
      a$fetch(job$query)
    },
    stop('Unsupported connection ...')
  )
  job$status <- 'ended'
  job$ended_at <- Sys.time()
  message(sprintf('Job %s finished ...', job_id))
}, error = function(e) {
  job$status <<- 'error'
  job$ended_at <<- Sys.time()
  job$message <<- as.character(e)
  message(sprintf('Job %s: %s', job_id, e))
}, finally = {
  redisHMSet(job_id, job)
  redisHDel(job_id, 'process_id')
  message(sprintf('Job %s - process finished ...', job_id))
})
