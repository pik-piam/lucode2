#' Test package
#' 
#' Installs a package in a temporary library and loads that library on top of the existing one
#' 
#' 
#' @param repo GitHub repository to install the package from
#' @param tmpLib temporary library directory where the package should be installed
#' @param ... additional arguments forwarded to \code{devtools::install_github}
#' @author Jan Philipp Dietrich
#' @export
#' @examples
#' 
#' \dontrun{
#' testPackage("git@github.com:pik-piam/lucode2")
#' }
#' 
testPackage <- function(repo, tmpLib=tempdir(), ...) {
  tmpLib <- normalizePath(tmpLib)
  .libPaths(c(tmpLib,.libPaths()))
  devtools::install_github(repo,...)
}

