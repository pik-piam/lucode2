#' Update package repository
#'
#' Function to update an package repository. Run this function on a folder which contains
#' packages sources as subfolders. Packages should be managed via git in order to be updated properly.
#' To add a new package to the repo just checkout/clone it into this folder. The
#' function will automatically detect the new package and add it.
#'
#' @param path Path to the repository
#' @param check Boolean deciding whether package must have been checked or not in order to be distributed
#' @param forceRebuild Option to rebuild all packages from source
#' @param clean Option to clean repos before updating/pulling to avoid merge conflicts
#' @param skipFolders Which folders/packages should not be built.
#' @param repoUrl Url of the package repo. Will be added to DESCRIPTION files in the Repository field.
#' @author Jan Philipp Dietrich, Pascal Führlich
#' @seealso \code{\link{buildLibrary}}
#' @importFrom devtools document build install_deps
#' @importFrom tools write_PACKAGES
#' @importFrom withr local_dir local_envvar with_dir
#' @export
updateRepo <- function(path = ".", check = TRUE, forceRebuild = FALSE, clean = TRUE, # nolint
                       skipFolders = c("Archive", "gdxrrw", "HARr", "SPARQL"),
                       repoUrl = "https://rse.pik-potsdam.de/r/packages") {
  checkRequiredPackages("gert", "interacting with git repos")
  path <- normalizePath(path)
  message(date(), "\n")

  if (!file.exists(file.path(path, "PACKAGES"))) {
    write_PACKAGES(path, unpacked = TRUE)
  }
  availablePackages <- suppressWarnings(available.packages(paste0("file:", path)))
  availablePackagesOnCran <- suppressWarnings(available.packages("https://cloud.r-project.org/src/contrib",
                                                                 filters = "duplicates"))

  # pull git repos
  repoPaths <- list.dirs(path, recursive = FALSE)
  packageNames <- basename(repoPaths)
  repoPaths <- repoPaths[!startsWith(packageNames, ".") &
                           !(packageNames %in% skipFolders) &
                           dir.exists(file.path(repoPaths, ".git"))]
  repoPaths <- repoPaths[order(tolower(repoPaths))]

  collectedErrors <- lapply(repoPaths, function(repoPath) {
    return(tryCatch({
      packageName <- basename(repoPath)
      if (clean) {
        gert::git_reset_hard(repo = repoPath)
        # emulate git clean using stash
        try({ # creates error if there's nothing to stash, ignore it
          gert::git_stash_save(include_untracked = TRUE, repo = repoPath)
          gert::git_stash_drop(repo = repoPath)
        }, silent = TRUE)
      }
      suppressMessages({
        gert::git_pull(repo = repoPath, verbose = FALSE)
      })
      currentVersion <- numeric_version(tryCatch(availablePackages[packageName, "Version"], error = function(e) "0"))

      messageStart <- paste0(".:: ", format(packageName, width = max(nchar(packageNames))),
                             " ", format(as.character(currentVersion), width = 10))

      validationKey <- validkey(repoPath)
      newVersion <- numeric_version(validationKey$version)

      if (!(validationKey$valid || !check || forceRebuild)) {
        message(messageStart, " -> ", newVersion, " invalid validation key ::.")
        str(gert::git_commit_info(repo = repoPath)[c("id", "author", "message", "time")])
        stop("invalid validation key")
      }

      targzPattern <- paste0("^", packageName, "_(.*)\\.tar\\.gz")
      targzBasename <- list.files(path, pattern = targzPattern)
      if (length(targzBasename) >= 2) {
        stop("for ", packageName, " there are multiple tgz files: ", paste(targzBasename, collapse = ", "))
      }

      buildVersion <- numeric_version(sub(targzPattern, "\\1", targzBasename))
      if (length(buildVersion) == 0) {
        buildVersion <- numeric_version("0")
      }

      if (forceRebuild || newVersion > currentVersion || newVersion > buildVersion) {
        devtools::install_deps(repoPath, upgrade = "always")

        if (validationKey$roxygen) {
          devtools::document(repoPath, roclets = c("rd", "collate", "namespace", "vignette"))
        }

        # add metadata such as remote url to DESCRIPTION e.g. for renv
        remoteUrl <- sub("git@github\\.com:(.+)\\.git",
                         "https://github.com/\\1",
                         gert::git_remote_info(repo = repoPath)$url)
        cat("Repository: ", repoUrl, "\n",
            "RemoteUrl: ", remoteUrl, "\n",
            "RemoteRef: HEAD\n",
            "RemoteSha: ", gert::git_commit_id(repo = repoPath), "\n",
            file = file.path(repoPath, "DESCRIPTION"), sep = "", append = TRUE)

        error <- try({
          devtools::build(repoPath)
        })

        gert::git_reset_hard(repo = repoPath) # reset DESCRIPTION changes to prevent merge conflict

        if (inherits(error, "try-error")) {
          message(messageStart, " -> ", newVersion, " build failed ::.")
          system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
          stop(error) # will be caught by tryCatch
        }

        message(messageStart, " -> ", newVersion, " build success ::.", appendLF = FALSE)

        # move old tgz file into Archive for renv
        if (length(list.files(path, pattern = targzPattern)) > 1) {
          archivePath <- file.path(path, "Archive", packageName)
          dir.create(archivePath, recursive = TRUE, showWarnings = !dir.exists(archivePath))
          file.rename(file.path(path, targzBasename), file.path(archivePath, targzBasename))
        }
      } else {
        message(messageStart, " ok ::.", appendLF = FALSE)
      }

      # check if newer package version is available on CRAN
      if (packageName %in% rownames(availablePackagesOnCran)) {
        cranversion <- numeric_version(availablePackagesOnCran[packageName, "Version"])
        if (cranversion > currentVersion && cranversion > newVersion) {
          message(" .::WARNING! CRAN: ", availablePackagesOnCran[packageName, "Version"], " !WARNING::.")
          stop("Package version of package \"", packageName, "\" is newer on CRAN (", cranversion,
               ") compared to PIK-CRAN (", currentVersion, ")! Check version on CRAN immediatly!")
        } else {
          message(" .::CRAN: ", availablePackagesOnCran[packageName, "Version"], "::.")
        }
      } else {
        message()
      }
      NULL
    }, error = function(error) error))
  })

  names(collectedErrors) <- basename(repoPaths)
  collectedErrors <- Filter(Negate(is.null), collectedErrors)

  write_PACKAGES(path, unpacked = TRUE)

  if (length(collectedErrors) >= 1) {
    cat("\nerrors:\n")
    print(collectedErrors)

    stop("There were errors, see log (on RSE server with `journalctl -r -u update-repo.service`)")
  }
  message("done.")
}
