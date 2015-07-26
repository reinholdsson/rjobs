
str_is_num <- function(x) suppressWarnings(sapply(x, function(i) !is.na(as.numeric(i))))
