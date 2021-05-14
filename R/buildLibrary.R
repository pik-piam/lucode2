#' buildLibrary
#'
#' Builds R libraries. Includes checks for consistency.
#'
#' This function is designed to help building R libraries. It performs the
#' following steps: \itemize{ \item Version: Determination
#' of a new version number (Can also be defined by the user). \item Date:
#' Determination of a the date of the build (Can also be defined by the user).
#' \item R check: Check whether the library is consistent and can be built.
#' \item Package building Builds the .zip and .tar.gz packages under windows.
#' Under linux, only the .tar.gz package is built. } The commit has to be performed by the user still.
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
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch
#' @seealso \code{\link{package2readme}}, \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom citation package2zenodo
#' @importFrom yaml read_yaml write_yaml
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
    s <- getLine()
    if (!(tolower(s) %in% c("y", "yes"))) {
      return(FALSE)
    }
    return(TRUE)
  }

  didYouPull <- function() {
    if (!askYesNo("Is your repository up-to-date? Did you pull immediately before running this check?")) {
      stop("Please update your repository first, before you proceed!")
    }
  }
  didYouPull()

  thisdir <- getwd()
  if (lib != ".") setwd(lib)
  on.exit(setwd(thisdir))

  ####################################################################
  # Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$", base::list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)
  unlink(rcheckfolders, recursive = TRUE)

  ####################################################################
  # Check if roxygen is used and run roxygenize if required
  ###################################################################
  descfile <- readLines("DESCRIPTION")
  if (any(grepl("RoxygenNote", descfile))) {
    devtools::document(pkg = ".", roclets = c("rd", "collate", "namespace", "vignette"))
    roxygen <- TRUE  # nolint
  } else {
    roxygen <- FALSE  # nolint
  }

  ############################################################
  # linter
  ############################################################
  if (any(sapply(lint(), length) > 0)) {
    stop(paste(
      "There were linter warnings, run lucode2::lint() to see them. You need to address these before submission!",
      "Running lucode2::autoFormat() might fix some warnings.",
      "In exceptional cases disabling the linter for some lines might be okay, see ?lintr::exclude on how to do that.",
      sep = "\n"))
  }

  ############################################################
  # check the library
  ############################################################

  if (!file.exists(".buildlibrary")) {
    # if not yet available, add .buildlibrary and add to .Rbuildignore
    cfg <- list(
      ValidationKey = 0,
      AutocreateReadme = TRUE,
      AcceptedWarnings = c(
        "Warning: package '.*' was built under R version",
        "Warning: namespace '.*' is not available and has been replaced"
      ),
      AcceptedNotes = NULL
    )
    write_yaml(cfg, ".buildlibrary")
    message("Created .buildlibrary config file and added it to .Rbuildignore. Please add it to your next commit!")
    if (file.exists(".Rbuildignore")) {
      a <- c(readLines(".Rbuildignore"), "^\\.buildlibrary$")
      if (anyDuplicated(a)) a <- a[!duplicated(a)]
    } else {
      a <- "^\\.buildlibrary$"
    }
    writeLines(a, ".Rbuildignore")
  }

  cfg <- read_yaml(".buildlibrary")

  if (is.null(cfg$AutocreateReadme)) {
    cfg$AutocreateReadme <- TRUE  # nolint
    if (askYesNo("Do you want to use GitHub Actions for package testing?")) {
      cfg$UseGithubActions <- TRUE  # nolint
    }
  }

  if (isTRUE(cfg$UseGithubActions)) {
    addGitHubActions(lib)
    # remove travis related parts
    travisfile <- paste0(lib, "/.travis.yml")
    if (file.exists(travisfile)) {
      file.remove(travisfile)
      rbuildignore <- paste0(lib, "/.Rbuildignore")
      if (file.exists(rbuildignore)) {
        a <- readLines(rbuildignore)
        writeLines(grep("travis", a, value = TRUE, invert = TRUE), rbuildignore)
      }
      testfolder <- paste0(lib, "/tests/testthat")
      if (!file.exists(testfolder)) dir.create(testfolder, recursive = TRUE)
      travistest <- paste0(lib, "/tests/testthat/test-travisCI.R")
      if (file.exists(travistest)) file.remove(travistest)
      if (length(dir(testfolder) == 0)) writeLines('skip("dummy test")', paste0(testfolder, "/test-dummy.R"))
    }
  }

  ck <- devtools::check(".", cran = cran)

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
  descfile <- readLines("DESCRIPTION")
  descfileVersion <- sub(pattern = "[^(0-9)]*$", replacement = "", perl = TRUE,
                          x = sub(pattern = "Version:[^(0-9)]*", replacement = "", perl = TRUE,
                                  x = grep(pattern = "Version", x = descfile, value = TRUE)))

  version <- descfileVersion

  autoversion <- function(oldVersion, upt, defLengths = 3) {
    oldVersion <- numeric_version(oldVersion)
    if (upt == 0) {
      return(oldVersion)
    }
    for (i in 1:upt) if (is.na(oldVersion[1, i])) oldVersion[1, i] <- 0
    if (oldVersion[1, upt] == 0 & upt == 4) oldVersion[1, upt] <- 9000
    oldVersion[1, upt] <- as.numeric(oldVersion[1, i]) + 1
    if (defLengths > upt) {
      for (i in (upt + 1):defLengths) {
        oldVersion[1, i] <- 0
      }
    }
    oldVersion <- oldVersion[1, 1:max(upt, defLengths)]
    return(oldVersion)
  }


  chooseModule <- function(title = "Package check successful! Please choose an update type") {
    updateType <- c(
      "major revision (for major rewrite of the whole package)",
      "minor revision (for new features or improvements)",
      "patch (for bugfixes and corrections)",
      "only for packages in development stage",
      "no version increment (only to use if version is already incremented!)"
    )
    cat(title, ":\n")
    cat(paste(c(1:(length(updateType) - 1), 0), updateType, sep = ": "), sep = "\n")
    cat("\nNumber: ")
    identifier <- getLine()
    identifier <- as.numeric(strsplit(identifier, ",")[[1]])
    if (any(!(identifier %in% (1:(length(updateType) - 1))))) {
      stop("This choice (", identifier, ") is not possible. ",
           "Please type in a number between 0 and ", length(updateType) - 1)
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
  version <- autoversion(version, updateType)

  # Change the version in descfile
  descfile[grep("Version", descfile)] <- sub(descfileVersion, version, descfile[grep("Version", descfile)])

  ############################################################
  # Check for the date
  ############################################################
  if (any(grepl("Date:", descfile))) {
    descfileDate <- sub(pattern = "[^(0-9)]*$", replacement = "", perl = TRUE,
                         x = sub(pattern = "Date:[^(0-9)]*", replacement = "", perl = TRUE,
                                 x = grep(pattern = "Date:", x = descfile, value = TRUE)))
    date <- Sys.Date()
    # Change the date in descfile
    descfile[grep("Date:", descfile)] <- sub(descfileDate, date, descfile[grep("Date:", descfile)])
  }

  ############################################################
  # Update validation key
  ############################################################
  if (cran) {
    cfg$ValidationKey <- as.character(validationkey(version, date))  # nolint
  } else {
    cfg$ValidationKey <- as.character(0)  # nolint
  }
  if (any(grepl("ValidationKey:", descfile))) {
    descfile <- descfile[!grepl("ValidationKey:", descfile)]
  }

  ##################################################################
  # Write the modified description files, update metadata and readme
  ##################################################################
  writeLines(descfile, "DESCRIPTION")
  write_yaml(cfg, ".buildlibrary")
  package2zenodo(".")
  if (isTRUE(cfg$AutocreateReadme)) package2readme(".")

  ############################################################
  # Verbosity for version information and git commands
  ############################################################

  if (updateType != 0) {
    cat(paste0("* updating from version"), descfileVersion, "to version", toString(version), "... OK\n")
    if (file.exists(".git")) {
      if (gitpush) {
        system(paste0('git add . && git commit -m "', commitmessage, " and type ", updateType, ' upgrade" && git push'))
      } else {
        cat("* git repository detected... OK\n")
        cat("* command suggestions for updating your git repository:\n")
        cat(rep("=", options()$width), "\n", sep = "")
        cat(paste0('adding and committing: $ git add . && git commit -m "type ', updateType, ' upgrade"\n'))
        cat(paste0("version tagging: $ git tag ", version, "\n"))
        cat("push commits to github: $ git push <remote> <branch>\n")
        cat("push new tag to github: $ git push --tags\n")
        cat(rep("=", options()$width), "\n", sep = "")
      }
    }
  }
  cat("done")
}
