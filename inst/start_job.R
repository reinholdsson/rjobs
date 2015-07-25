#!/usr/local/bin/r
suppressMessages(require(rredis))
redisConnect()
job_id <- argv[1]

job <- redisHGetAll(job_id)
job$status <- 'started'
job$started_at <- Sys.time()
job$process_id <- Sys.getpid()
redisHMSet(job_id, job)
redisHDel(job_id, 'ended_at')

Sys.sleep(60)
# data <- sample(mtcars)
# output <- tempfile(fileext = '.rds')
# saveRDS(data, output)
output <- mtcars

# Finished
job$status <- 'ended'
job$output <- output
job$ended_at <- Sys.time()
redisHMSet(job_id, job)
redisHDel(job_id, 'process_id')

message(sprintf('Job %s finished ...', job_id))