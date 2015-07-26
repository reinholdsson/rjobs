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

Sys.sleep(30)
output <- switch(cfg$connections[[job$conn]]$src,
  'exasol' = {
    message('exasol!')
    mtcars
  },
  'postgres' = {
    message('postgresql!')
    cars
  },
  warning('Unsupported connection ...')
)

## SWITCH DATABASES
# data <- sample(mtcars)
# output <- tempfile(fileext = '.rds')
# saveRDS(data, output)

# Finished
job$status <- 'ended'
job$output <- output
job$ended_at <- Sys.time()
redisHMSet(job_id, job)
redisHDel(job_id, 'process_id')

message(sprintf('Job %s finished ...', job_id))