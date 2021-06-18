#' githubMetadataExtract
#'
#' A function to extract metadata information from a GitHub organization, summarizing
#' information about the repositories belonging to that respective organization
#'
#' @param org name of the organization from which the metadata should be extracted
#' @author Jan Philipp Dietrich
#' @export
githubMetadataExtract <- function(org) {
 filter <- c("name", "description", "html_url", "default_branch", "created_at", "updated_at",
             "language", "stargazers_count", "watchers_count", "forks")
 xraw <- suppressWarnings(read_yaml(url(paste0("https://api.github.com/orgs/", org, "/repos?per_page=100"))))
 x <- sapply(xraw, function(x, filter) return(x[filter]), filter = filter)
 x <- as.data.frame(t(x))
 x$language <- as.character(x$language)
 x$created_at <- format(as.POSIXct(unlist(x$created_at)), "%Y-%m-%d")
 x$updated_at <- format(as.POSIXct(unlist(x$updated_at)), "%Y-%m-%d")
 x$type <- ""
 x$title <- ""
 x$license <- NA
 x$published <- TRUE
 x$opensource <- NA
 x$doi <- ""
 for (i in seq_len(nrow(x))) {
   if (is.list(x[i, "license"]) && !is.null(x[i, "license"]$license$key)) x[i, "license"] <- x[i, "license"]$license$key
   if (x[i, "language"] == "R") {
     descUrl <- paste0("https://raw.githubusercontent.com/", org, "/",
                        x[i, "name"], "/", x[i, "default_branch"], "/DESCRIPTION")
     desc <- try(readLines(url(descUrl)), silent = TRUE)
     if (class(desc) != "try-error") {
       x[i, "type"] <- "package"
       x[i, "license"] <- sub("^.*: *", "", grep("License:", desc, value = TRUE))
       x[i, "title"]   <- sub("^.*: *", "", grep("Title:", desc, value = TRUE))
       doi <- sub("^.*https://doi.org", "https://doi.org", grep("doi.org", desc, value = TRUE))
       if (length(doi) > 0) x[i, "doi"] <- doi
     }
   }
 }
 # semi-manual clean up
 .clean <- function(x, map) {
   for (m in names(map)) x[grepl(m, x)] <- map[m]
   return(x)
 }
 x$license <- .clean(x$license, c("LGPL-3" = "lgpl-3",
                                  "GPL-3" = "gpl-3",
                                  "GPL-2" = "gpl-2",
                                  "BSD_2" = "bsd-2",
                                  "MIT" = "mit"))
 x$license[grepl("GPL-3", x$license)]  <- "gpl-3"
 x$license[grepl("BSD_2", x$license)] <- "bsd-2"
 x$opensource[x$license %in% c("lgpl-3", "gpl-3", "gpl-2", "bsd-2", "mit")] <- TRUE
 x$url <- x$html_url
 x$description[x$title != ""] <- x$title[x$title != ""]
 x <- x[c("name", "type", "language", "created_at", "updated_at", "published",
          "opensource", "license", "url", "doi", "description")]
 for (i in seq_len(ncol(x))) {
    x[[i]] <- unlist(x[[i]])
 }
 out <- list()
 for (j in seq_len(nrow(x))) {
    out[[x$name[j]]] <- x[j, ]
 }
 return(out)
}
