#' @export
df_to_xlsx <- function(input, output = tempfile(pattern = 'data_', fileext = '.xlsx')) {
  options(
    xlsx.date.format = 'yyyy-mm-dd',
    xlsx.datetime.format = 'yyyy-mm-dd hh:mm:ss'
  )
  
  # Allow input as several arguments
  # e.g. df_to_xlsx(list(list(data = cars), list(data2 = mtcars)))
  if (!(is.list(input) && length(input) == 1L && is.null(names(input)))) {
    input <- sapply(input, function(x) x)
  } else {
    input <- input[[1]]
  }
  
  for(i in 1:length(input)) {
    nm <- names(input)[[i]]
    sheet_name <- if (!is.null(nm) && nchar(nm) > 0) nm else basename(tempfile())
    write.xlsx2(x = input[[i]], file = output, sheetName = sheet_name, row.names = F, append = T, showNA = F)
  }
  
  return(output)
}
