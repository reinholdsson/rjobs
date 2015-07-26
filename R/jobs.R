#' @export
init <- function(config) {
  cfg <- suppressWarnings(yaml.load_file(config))
  .RJOBS_CONFIG <<- config
  do.call('redisConnect', cfg$redis)
  #redisConnect()  # requires a running redis server
}

#' @export
create_job <- function(conn, query, desc = NULL) {
  job_id <- basename(tempfile(pattern = ''))
  redisHMSet(job_id, list(
    created_at = Sys.time(),
    query = query,
    status = 'created',
    conn = conn,
    user = Sys.info()[['user']],
    desc = desc
  ))
  return(job_id)
}

#' @export
start_job <- function(job_id) {
  status <- redisHGet(job_id, 'status')
  if (is.null(status)) stop("Job doesn't exist ...")
  else if (status == 'started') stop("Job is already running ...")
  system2('r', args = c(system.file('start_job.R', package = 'rjobs'), c(.RJOBS_CONFIG, job_id)), wait = F)
}

#' @export
info <- function() {
  rbindlist(lapply(redisKeys(), function(i) data.table(job_id = i, t(redisHMGet(i, c('user', 'conn', 'desc', 'created_at', 'started_at', 'ended_at'))))), use.names = T, fill = T)
  # rbindlist(lapply(redisKeys(), function(i) c(job_id = i, redisHGetAll(i))), fill = T, use.names = T)
}

#' @export
delete_job <- function(job_id) {
  if (is.null(redisHGet(job_id, 'status'))) stop("job id doesn't exist")
  
  pid <- redisHGet(job_id, 'process_id')
  if (!is.null(pid)) pskill(pid, SIGKILL)  # why doesn't it return TRUE first time??
  
  redisDelete(job_id)
  return(T)
}
