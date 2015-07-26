# Example
library(rjobs)
init('config.yml')
# redisFlushDB()

# Create new job
job1 <- create_job('exa', 'select * from a', 'job1')
job2 <- create_job('exa', 'select * from b', 'job2')
job3 <- create_job('psql', 'select * from c', 'job3')
info()

start_job(job2)
info()

# Delete job (and kill ongoing process)
delete_job(job1)
