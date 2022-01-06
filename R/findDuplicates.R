findDuplicates <- function(lists) {
  if (is.null(names(lists)) || "" %in% names(lists) || length(unique(names(lists))) < length(lists)) {
    stop("All arguments given to findDuplicates must have a unique name.")
  }
  allElements <- unique(do.call(c, lists))
  elementLocations <- lapply(allElements, function(element) {
    return(Filter(function(listName) element %in% lists[[listName]], names(lists)))
  })
  names(elementLocations) <- allElements
  duplicates <- Filter(function(x) length(x) > 1, elementLocations)
  return(duplicates)
}
