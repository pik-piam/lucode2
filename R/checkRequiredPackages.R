#' checkRequiredPackages
#'
#' Check if one or more packages are available and try to install the missing packages. Throw an error if at least one
#' of the required packages is still missing after the installation attempt.
#'
#' @param requiredPackages One or more names of packages that are checked using requireNamespace.
#' @param requiredFor Optional single string. What the packages are required for, usually the name of a function.
#' @param installFunction Optional function, defaults to install.packages. Will be called during
#' checkRequiredPackages like this: installFunction(missingPackages, libPaths[[1]]). Only needed if the required
#' packages are not available on CRAN or the configured repos (getOption("repos")). In that case you might want to use
#' something like this: function(...) remotes::install_github("USDA-ERS/MTED-HARr").
#' @param readlineFunction This argument was added for testing. A function to get an answer from the user.
#' @param libPaths This argument was added for testing. Where to look for and install the required packages.
#' @author Pascal Sauer
#' @seealso \code{\link{requireNamespace}}
#' @importFrom utils install.packages
#' @examples
#' \dontrun{
#' checkRequiredPackages(c("ggplot2", "lusweave"), "lucode2::readRuntime(..., plot = TRUE)")
#' }
#' @export
checkRequiredPackages <- function(requiredPackages, requiredFor = "", installFunction = install.packages,
                                  readlineFunction = readline,
                                  libPaths = .libPaths()) { # nolint
  stopifnot(length(requiredFor) == 1)
  requiredFor <- if (requiredFor != "") paste(" for", requiredFor) else ""

  missingPackages <- Filter(function(package) !requireNamespace(package, lib.loc = libPaths, quietly = TRUE),
                            requiredPackages)

  question <- paste0("The following currently not installed package",
                     if (length(missingPackages) == 1) " is" else "s are",
                     " required", requiredFor, ":\n",
                     paste("-", missingPackages, collapse = "\n"),
                     "\nDo you want to install ",
                     if (length(missingPackages) == 1) "it" else "them",
                     " now? (y/N)")
  if (length(missingPackages) > 0 && tolower(readlineFunction(question)) %in% c("y", "yes")) {
    installFunction(missingPackages, libPaths[[1]])
  }

  stillMissingPackages <- Filter(function(package) !requireNamespace(package, lib.loc = libPaths, quietly = TRUE),
                                 requiredPackages)
  if (length(stillMissingPackages) > 0) {
    stop(paste0("The following package",
                if (length(stillMissingPackages) == 1) " is" else "s are",
                " required", requiredFor, ", but ",
                if (length(missingPackages) == 1) "it is" else "they are",
                " still not available:\n",
                paste("-", stillMissingPackages, collapse = "\n")))
  }
}
