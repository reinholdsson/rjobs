
# XLSX Functions
fix_dates <- function(df) {
  fun <- function(i) if (is.Date(i)) as.character(i) else i
  mutate_each(df, funs(fun))
}

add_sheet <- function(wb, name, df) {
  df <- data.frame(fix_dates(df))
  createSheet(wb, name = name)
  writeWorksheet(wb, df, sheet = name)
}

#' @export
df_to_xlsx <- function(input, output = tempfile(pattern = 'data_', fileext = '.xlsx')) {
  require(lubridate)
  require(XLConnect)

  wb <- loadWorkbook(output, create = T)
  
  # Allow input as several arguments
  # ("- test: !expr mtcars\n- test2: !expr cars") or as a list ("- !expr mylist")
  if (!(is.list(input) && length(input) == 1L && is.null(names(input)))) {
    input <- sapply(input, function(x) x)
  } else {
    input <- input[[1]]
  }
  
  for(i in 1:length(input)) {
    nm <- names(input)[[i]]
    sheet_name <- if (!is.null(nm) && nchar(nm) > 0) nm else basename(tempfile())
    add_sheet(wb, sheet_name, input[[i]])
  }
  saveWorkbook(wb)
  return(output)
}

create_temp_file <- function(file_name, ...) {
    temp_dir <- tempfile(...)
    dir.create(temp_dir)
    file.path(temp_dir, file_name)
}

# df_to_xlsx(list(list(data = cars), list(data2 = mtcars)))
