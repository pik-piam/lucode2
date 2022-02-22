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
#' @param checkForUpdates Check for lucode2 updates (default TRUE). Set FALSE in case of problems with the new version.
#' @param autoCheckRepoUpToDate Automatically check if your repository is up to date. If FALSE the user is simply asked.
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
#' @note The behavior of buildLibrary can be configured via the `.buildLibrary` file in the
#' main folder of the package. It uses YAML format and can contain the following entries:
#'
#' * **ValidationKey**: This entry always exists and is written automatically by `buildLibrary`
#'                      It confirms that the package has been successfully build via the function.
#' * **AutocreateReadme** (optional): yes/no - decides whether `buildLibrary` automatically updates
#'                                    the README.md file or not (default: yes)
#' * **AddInReadme** (optional): Additional entries to be added to the autocreated README. Provided either
#'                               in markdown format or as paths to RMarkdown (Rmd) or Markdown (md) files
#' * **AcceptedWarnings** (optional): a list of Warnings which should be ignored by `buildLibrary`
#'                                    (autocompletion via asterisks allowed)
#' * **AcceptedNotes** (optional): a list of Notes which should be ignored by `buildLibrary`
#'                                    (autocompletion via asterisks allowed)
#' * **allowLinterWarnings**: yes/no - If set to "no" linter warnings will stop the build process.
#'                            (default: yes)
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal Führlich
#' @seealso \code{\link{package2readme}}, \code{\link{lint}}, \code{\link{autoFormat}}
#' @importFrom citation package2zenodo
#' @importFrom desc desc desc_get_deps
#' @importFrom yaml write_yaml
#' @importFrom utils old.packages update.packages packageVersion
#' @importFrom withr defer local_connection local_dir
#' @examples
#' \dontrun{
#' buildLibrary()
#' }
#' @export
buildLibrary <- function(lib = ".", cran = TRUE, updateType = NULL, gitpush = FALSE, commitmessage = NULL, # nolint
                         checkForUpdates = TRUE, autoCheckRepoUpToDate = TRUE) {
  local_dir(lib)
  if (checkForUpdates) {
    pleaseRestartSession <- paste("Please restart your R session (in RStudio: Ctrl+Shift+F10) to make sure that the",
                                  "newest lucode2 version is loaded. Then try again:\nlucode2::buildLibrary()")
    if (.__NAMESPACE__.$spec[[2]] != packageVersion("lucode2")) {
      # the loaded lucode2 version (.__NAMESPACE__.$spec[[2]]) is different from the version available on disk
      cat(pleaseRestartSession)
      return(invisible(NULL))
    }
    cat("Checking for lucode2 update... ")
    if (!is.null(old.packages(instPkgs = installed.packages()["lucode2", , drop = FALSE]))) {
      cat("A new version of lucode2 is available, please update.\n")
      update.packages(oldPkgs = "lucode2")
      cat(pleaseRestartSession)
      return(invisible(NULL))
    } else {
      cat("You're running the newest lucode2 version.\n")
    }
  }

  getLine <- function() {
    # gets characters (line) from the terminal or from a connection
    # and returns it
    if (interactive()) {
      s <- readline()
    } else {
      con <- file("stdin")
      defer({
        close(con)
      })
      s <- readLines(con, 1, warn = FALSE)
    }
    return(s)
  }

  checkRepoUpToDate(".", autoCheckRepoUpToDate)

  ############################################################
  # load/create .buildLibrary file
  ############################################################
  cfg <- loadBuildLibraryConfig()

  ############################################################
  # import Depends packages
  ############################################################
  dependsPackages <- setdiff(desc_get_deps()[desc_get_deps()["type"] == "Depends", "package"], "R")
  if (length(dependsPackages) > 0) {
    # All packages defined as "Depends" in DESCRIPTION must be imported, otherwise a check fails.
    writeLines(c("# Generated by lucode2: do not edit by hand\n",
                 paste("#' @import", paste(dependsPackages, collapse = " ")),
                 "NULL"),
               file.path("R", "imports.R"))
  }

  ####################################################################
  # Run checks, tests and linter
  ###################################################################
  testfolder <- file.path("tests", "testthat")
  if (!file.exists(testfolder)) {
    dir.create(testfolder, recursive = TRUE)
  }
  if (length(dir(testfolder)) == 0) {
    writeLines('skip("dummy test")', file.path(testfolder, "test-dummy.R"))
  }
  check(cran = cran, config = cfg)

  ####################################################################
  # Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$", list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)
  unlink(rcheckfolders, recursive = TRUE)

  ############################################################
  # add GitHub actions and pre-commit-config
  ############################################################
  tryCatch(addGitHubActions(), error = function(error) {
    message("Could not add GitHub Actions:", error)
  })

  if (!file.exists(file.path("DESCRIPTION")) ||
      desc(file = file.path("DESCRIPTION"))[["get"]]("Package") != "lucode2") {
    fileUrl <- "https://raw.githubusercontent.com/pik-piam/lucode2/master/.pre-commit-config.yaml"
    urlConnection <- local_connection(url(fileUrl))
    preCommitConfig <- sub("autoupdate_schedule: weekly", "autoupdate_schedule: quarterly", readLines(urlConnection))
    writeLines(preCommitConfig, file.path(".pre-commit-config.yaml"))
  }

  ##########################################################
  # Check for version numbers
  ##########################################################
  # Version number in the man file
  # Version number in the description file
  descfile <- readLines(file.path("DESCRIPTION"))
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
  writeLines(descfile, file.path("DESCRIPTION"))
  write_yaml(cfg, file.path(".buildlibrary"))
  package2zenodo()
  if (isTRUE(cfg$AutocreateReadme)) {
    package2readme(add = cfg$AddInReadme)
  }

  ####################################################################
  # Make sure man/*.Rd files are not ignored
  ###################################################################
  gitignore <- readLines(file.path(".gitignore"))
  if ("*.Rd" %in% gitignore || file.exists(file.path("man", ".gitignore"))) {
    message("*.Rd files are currently ignored, but they should be commited.")

    if ("*.Rd" %in% gitignore) {
      message('removing "*.Rd" from .gitignore')
      writeLines(gitignore[gitignore != "*.Rd" &
                             gitignore != "# Help files (because they will be created automatically by roxygen)"],
                 ".gitignore")
    }

    if (file.exists(file.path("man", ".gitignore"))) {
      message("removing man/.gitignore")
      file.remove(file.path("man", ".gitignore"))
    }
  }

  ############################################################
  # Verbosity for version information and git commands
  ############################################################

  if (updateType != 0) {
    cat(paste0("* updating from version"), descfileVersion, "to version", toString(version), "... OK\n")
    if (file.exists(file.path(".git"))) {
      if (gitpush) {
        system(paste0('git add . && git commit -m "', commitmessage, " and type ", updateType,
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
}
