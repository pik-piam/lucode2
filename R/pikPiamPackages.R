#' pikPiamPackages
#'
#' Fetches the names of packages available on https://pik-piam.r-universe.dev/ui#builds
#'
#' @return A character vector of names of packages available on https://pik-piam.r-universe.dev/ui#builds
#'
#' @export
pikPiamPackages <- function() {
  packagesUrl <- "https://pik-piam.r-universe.dev/src/contrib/PACKAGES"
  return(sub("^Package: ", "", grep("^Package: ", readLines(packagesUrl), value = TRUE)))
}
