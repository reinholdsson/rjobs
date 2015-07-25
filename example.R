# Example
library(rjobs)
redisConnect()  # requires a running redis server
# redisFlushDB()

# Create new job
job1 <- create_job('myquery1')
job2 <- create_job('myquery2')
job3 <- create_job('myquery3')

start_job(job2)

info()

# Delete job (and kill ongoing process)
delete_job(job1)
