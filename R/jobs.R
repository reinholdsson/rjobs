#' @export
create_job <- function(fun, input) {
  job_id <- basename(tempfile(pattern = ''))
  redisHMSet(job_id, list(fun = 'myfunction', created_at = Sys.time(), input = input, status = 'created'))
  return(job_id)
}

#' @export
start_job <- function(job_id) {
  status <- redisHGet(job_id, 'status')
  if (is.null(status)) stop("Job doesn't exist ...")
  else if (status == 'started') stop("Job is already running ...")
  system2('r', args = c(system.file('start_job.R', package = 'rjobs'), job_id), wait = F)
}

#' @export
info <- function() {
  # rbindlist(lapply(redisKeys(), function(i) data.table(job_id = i, t(redisHGetAll(i)))), use.names = T, fill = T)
  rbindlist(lapply(redisKeys(), function(i) c(job_id = i, redisHGetAll(i))), fill = T)
}

#' @export
delete_job <- function(job_id) {
  if (is.null(redisHGet(job_id, 'status'))) stop("job id doesn't exist")
  
  pid <- redisHGet(job_id, 'process_id')
  if (!is.null(pid)) pskill(pid, SIGKILL)  # why doesn't it return TRUE first time??
  
  redisDelete(job_id)
  return(T)
}
