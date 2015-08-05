#' @export
df_to_xlsx <- function(input, output = tempfile(pattern = 'data_', fileext = '.xlsx')) {
  options(
    openxlsx.dateFormat = 'yyyy-mm-dd',
    openxlsx.datetimeFormat = 'yyyy-mm-dd hh:mm:ss'
  )
  
  # Allow input as several arguments
  # e.g. df_to_xlsx(list(list(data = cars), list(data2 = mtcars)))
  if (!(is.list(input) && length(input) == 1L && is.null(names(input)))) {
    input <- sapply(input, function(x) x)
  } else {
    input <- input[[1]]
  }
  
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(input)) {
    nm <- names(input)[[i]]
    sheet_name <- if (!is.null(nm) && nchar(nm) > 0) nm else basename(tempfile())
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, input[[i]], rowNames = F)
  }
  
  openxlsx::saveWorkbook(wb, output, overwrite = T)
  return(output)
}
