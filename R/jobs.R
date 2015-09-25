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
  if (is.null(status)) {
    warning("Job doesn't exist ...")
    return(F)
  } else if (status == 'started') {
    warning("Job is already running ...")
    return(F)
  }
  system2('Rscript', args = c(system.file('start_job.R', package = 'rjobs'), c(.RJOBS_CONFIG, job_id)), wait = F)
  message(sprintf('%s started ...', job_id))
  return(T)
}

#' @export
info <- function() {
  DT <- rbindlist(lapply(redisKeys(), function(i) {
    cbind(job_id = i, as.data.table(lapply(redisHMGet(i, c('created_at', 'user', 'conn', 'desc', 'query', 'started_at', 'ended_at', 'status', 'message')), function(i) if (is.null(i)) NA_character_ else as.character(i))))
  }), use.names = T, fill = T)
#   DT[, c('created_at', 'started_at', 'ended_at') := list(
#     as.POSIXct(created_at, origin = '1970-01-01'),
#     as.POSIXct(started_at, origin = '1970-01-01'),
#     as.POSIXct(ended_at, origin = '1970-01-01')
#   )]
  return(DT)
}

#' @export
delete_job <- function(job_id) {
  if (is.null(redisHGet(job_id, 'status'))) {
    warning("job id doesn't exist")
    return(F)
  }
  
  pid <- redisHGet(job_id, 'process_id')
  if (!is.null(pid)) pskill(pid, SIGKILL)  # why doesn't it return TRUE first time??
  
  redisDelete(job_id)
  return(T)
}

#' @export
get_jobs_output <- function(jobs) {
  x <- lapply(jobs, function(i) {
    redisHGet(i, 'output')
  })
  names(x) <- sapply(jobs, function(i) { redisHGet(i, 'desc') })
  # remove empty
  x <- x[sapply(x, function(i) !is.null(i[[1]]))]
  
  return(x)
}

get_job_attr <- function(job_id, attr) {
  redisHMGet(job_id, attr)
}

# replace_null <- function(DT, val = NA) sapply(DT, function(x) ifelse(x == "NULL", val, x))

