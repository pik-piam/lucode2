#' buildLibrary
#'
#' Builds R libraries. Includes checks for consistency.
#'
#' This function is designed to help building R libraries. It performs the
#' following steps: \itemize{
#' \item Version: Determination of a new version number (Can also be defined by the user).
#' \item Date: Determination of a the date of the build (Can also be defined by the user).
#' \item Linter: Check for code style problems.
#' \item R check: Check whether the library is consistent and can be built.
#' \item Package building: Builds the .zip and .tar.gz packages under windows.
#' Under linux, only the .tar.gz package is built. }
#' The commit has to be performed by the user still.
#'
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param gitpush If a git commit should happen automatically
#' @param commitmessage Your commit message
#' @param updateType Either an integer or character string:
#'
#'   | **number**  | **string**    | **description**                          |
#'   |-------------|---------------|------------------------------------------|
#'   | 1           | `major`       | for major rewrite of the whole package   |
#'   | 2 (default) | `minor`       | for new features or improvements         |
#'   | 3           | `patch`       | for bugfixes and corrections             |
#'   | 4           | `development` | only for packages in development stage   |
#'
#' @md
#'
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal Führlich
#' @seealso \code{\link{package2readme}}, \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom citation package2zenodo
#' @importFrom yaml write_yaml
#' @examples
#' \dontrun{
#' buildLibrary()
#' }
#' @export
buildLibrary <- function(lib = ".", cran = TRUE, updateType = NULL, gitpush = FALSE, commitmessage = NULL) {
  getLine <- function() {
    # gets characters (line) from the terminal or from a connection
    # and returns it
    if (interactive()) {
      s <- readline()
    } else {
      con <- file("stdin")
      s <- readLines(con, 1, warn = FALSE)
      on.exit(close(con))
    }
    return(s)
  }

  askYesNo <- function(question) {
    cat(paste(question, "(yes/no)"))
    return(isTRUE(tolower(getLine()) %in% c("y", "yes")))
  }

  # did user pull?
  if (!askYesNo("Is your repository up-to-date? Did you pull immediately before running this check?")) {
    stop("Please update your repository first, before you proceed!")
  }

  ####################################################################
  # Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$", list.dirs(path = lib, full.names = FALSE, recursive = FALSE), value = TRUE)
  unlink(rcheckfolders, recursive = TRUE)

  ####################################################################
  # Check if roxygen is used and run roxygenize if required
  ###################################################################
  descfile <- readLines(file.path(lib, "DESCRIPTION"))
  if (any(grepl("RoxygenNote", descfile))) {
    devtools::document(pkg = lib, roclets = c("rd", "collate", "namespace", "vignette"))
  }

  ############################################################
  # load/create .buildLibrary file
  ############################################################
  cfg <- loadBuildLibraryConfig(lib)

  ############################################################
  # linter
  ############################################################
  linterResult <- lint()
  if (length(linterResult) > 0) {
    warning(paste(
      "There were linter warnings. It is not mandatory to fix them, they do not prevent buildLibrary from finishing",
      "normally. Still, please fix all linter warnings in new code and ideally also some in old code. Running",
      "lucode2::autoFormat() might fix some warnings. If really needed, see ?lintr::exclude on how to disable the",
      "linter for some lines."
    ))
    if (isFALSE(cfg$allowLinterWarnings)) {
      return(linterResult)
    }
  }

  ############################################################
  # GitHub actions
  ############################################################
  if (isTRUE(cfg$UseGithubActions)) {
    addGitHubActions(lib)
  }

  ############################################################
  # check the library
  ############################################################

  ck <- devtools::check(lib, cran = cran)

  # Filter warnings and notes which are accepted
  for (aw in cfg$AcceptedWarnings) {
    ck$warnings <- grep(aw, ck$warnings, value = TRUE, invert = TRUE)
  }
  for (aw in cfg$AcceptedNotes) {
    ck$notes <- grep(aw, ck$notes, value = TRUE, invert = TRUE)
  }
  print(ck)

  if (length(ck$errors) > 0) {
    stop("The package check showed errors. You need to fix these errors first before submission!")
  }

  if (length(ck$warnings) > 0) {
    stop("The package check showed warnings. You need to take care of these warnings first before submission!")
  }

  if (length(ck$notes) > 0) {
    stop("The package check showed notes. You need to take care of these notes first before submission!")
  }

  ##########################################################
  # Check for version numbers
  ##########################################################
  # Version number in the man file
  # Version number in the description file
  descfile <- readLines(file.path(lib, "DESCRIPTION"))
  descfileVersion <- sub(
    pattern = "[^(0-9)]*$", replacement = "", perl = TRUE,
    x = sub(
      pattern = "Version:[^(0-9)]*", replacement = "", perl = TRUE,
      x = grep(pattern = "Version", x = descfile, value = TRUE)
    )
  )

  version <- descfileVersion

  chooseModule <- function(title = "Package check successful! Please choose an update type") {
    updateType <- c(
      "major revision (for major rewrite of the whole package)",
      "minor revision (for new features or improvements)",
      "patch (for bugfixes and corrections)",
      "only for packages in development stage",
      "no version increment (only to use if version is already incremented!)"
    )
    cat(title, ":\n")
    updateTypeNumber <- c(1:(length(updateType) - 1), 0)
    cat(paste(updateTypeNumber, updateType, sep = ": "), sep = "\n")
    cat("\nNumber: ")
    identifier <- getLine()
    identifier <- as.numeric(strsplit(identifier, ",")[[1]])
    if (any(!(identifier %in% updateTypeNumber))) {
      stop(
        "This choice (", identifier, ") is not possible. ",
        "Please type in a number between 0 and ", length(updateType) - 1
      )
    }
    return(identifier)
  }

  if (is.null(updateType)) {
    updateType <- chooseModule()
  } else {
    # convert character updateType parameters to numbers
    updateType <- switch(as.character(updateType),
      "major" = 1,
      "1" = 1,
      "minor" = 2,
      "2" = 2,
      "patch" = 3,
      "3" = 3,
      "development" = 4,
      "4" = 4,
      # default
      chooseModule()
    )
  }
  version <- incrementVersion(version, updateType)

  # Change the version in descfile
  descfile[grep("Version", descfile)] <- sub(descfileVersion, version, descfile[grep("Version", descfile)])

  ############################################################
  # Check for the date
  ############################################################
  dateToday <- Sys.Date()
  if (any(grepl("Date:", descfile))) {
    descfile[grep("Date:", descfile)] <- paste("Date:", dateToday)
  } else {
    descfile <- append(descfile, paste("Date:", dateToday))
  }

  ############################################################
  # Update validation key
  ############################################################
  cfg$ValidationKey <- as.character(if (cran) validationkey(version, dateToday) else 0) # nolint

  if (any(grepl("ValidationKey:", descfile))) {
    descfile <- descfile[!grepl("ValidationKey:", descfile)]
  }

  ##################################################################
  # Write the modified description files, update metadata and readme
  ##################################################################
  writeLines(descfile, file.path(lib, "DESCRIPTION"))
  write_yaml(cfg, file.path(lib, ".buildlibrary"))
  package2zenodo(lib)
  if (isTRUE(cfg$AutocreateReadme)) {
    package2readme(lib)
  }

  ############################################################
  # Verbosity for version information and git commands
  ############################################################

  if (updateType != 0) {
    cat(paste0("* updating from version"), descfileVersion, "to version", toString(version), "... OK\n")
    if (file.exists(file.path(lib, ".git"))) {
      if (gitpush) {
        system(paste0('cd "', lib, '" && git add . && git commit -m "', commitmessage, " and type ", updateType,
                      ' upgrade" && git push && cd -'))
      } else {
        cat("* git repository detected... OK\n")
        cat("* command suggestions for updating your git repository:\n")
        cat(rep("=", getOption("width")), "\n", sep = "")
        cat(paste0('change into the package dir: $ cd "', lib, '"\n'))
        cat(paste0('adding and committing: $ git add . && git commit -m "type ', updateType, ' upgrade"\n'))
        cat(paste0("version tagging: $ git tag ", version, "\n"))
        cat("push commits to github: $ git push <remote> <branch>\n")
        cat("push new tag to github: $ git push --tags\n")
        cat(rep("=", getOption("width")), "\n", sep = "")
      }
    }
  }
  cat("done\n")
  return(linterResult)
}
