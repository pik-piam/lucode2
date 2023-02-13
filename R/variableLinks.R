variableLinks <- function(x, name) {
  tmp <- sapply(x, function(x, y) return(y %in% x), name) # nolint
  return(names(tmp)[tmp])
}
