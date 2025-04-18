citationDoi <- function(meta) {
  cit <- citation(auto = meta)
  class(cit) <- "list"
  attr(cit[[1]], "textVersion") <- NULL
  class(cit) <- "bibentry"
  cit$title <- sub(" *\\*$", "", cit$title)
  if (!is.null(cit$note)) {
    tmp <- strsplit(cit$note, ",[ \n]*")[[1]]
    urls <- grep("http", tmp, value = TRUE)
    cit$note <- paste0(grep("http", tmp, value = TRUE, invert = TRUE), collapse = ", ")
    cit$doi <- gsub("\n", "", gsub("https://doi.org/", "",
                                   grep("doi.org", urls[1], fixed = TRUE, value = TRUE), fixed = TRUE))
    cit$url <- c(cit$url, grep("doi.org", urls, fixed = TRUE, value = TRUE, invert = TRUE))
    if (cit$note == "") cit$note <- paste("R package version", meta$Version)
    if (!length(cit$url)) cit$url <- NULL
    if (!length(cit$doi)) cit$doi <- NULL
  }
  return(cit)
}
