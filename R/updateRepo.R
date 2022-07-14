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
updateRepo <- function(path = ".", check = TRUE, forceRebuild = FALSE, clean = FALSE, # nolint
                       skipFolders = c("Archive", "gdxrrw", "HARr"),
                       repoUrl = "https://rse.pik-potsdam.de/r/packages") {
  checkRequiredPackages("gert", "get git remote url and commit hash")
  path <- normalizePath(path)
  local_dir(path)
  message(date(), "\n")

  # pull git repos
  dirs <- grep("^\\.", list.dirs(recursive = FALSE, full.names = FALSE), value = TRUE, invert = TRUE)
  for (d in dirs) {
    if (d %in% skipFolders) {
      next
    }
    with_dir(d, {
      if (dir.exists(".git")) {
        if (clean) {
          system("git reset --hard HEAD -q; git clean -fxq; git pull -q")
        } else {
          system("git pull -q")
        }
      }
    })
  }

  # build new versions of packages including documentation
  updatePACKAGES <- FALSE
  availablePackages <- suppressWarnings(available.packages(paste0("file:", path), filters = "duplicates"))
  availablePackagesOnCran <- suppressWarnings(available.packages("https://cloud.r-project.org/src/contrib",
                                                                 filters = "duplicates"))
  dirs <- dirs[order(tolower(dirs))]
  for (d in dirs) {
    if (d %in% skipFolders) {
      next
    }
    fd <- format(d, width = max(nchar(dirs)))
    curversion <- tryCatch(availablePackages[d, "Version"], error = function(e) 0)
    with_dir(d, {
      vkey <- validkey()
      pattern <- paste0("^", d, "_(.*)\\.tar\\.gz")
      buildVersion <- max(as.numeric_version(sub(pattern, "\\1", dir("..", pattern = pattern))))
      if (length(buildVersion) == 0) {
        buildVersion <- as.numeric_version(0)
      }

      if (as.numeric_version(curversion) < as.numeric_version(vkey$version) || forceRebuild) {
        if (vkey$valid || !check || forceRebuild) {
          error <- try(devtools::install_deps(upgrade = "always"))
          if (vkey$roxygen && !("try-error" %in% class(error))) {
            error <- try(devtools::document(pkg = ".", roclets = c("rd", "collate", "namespace", "vignette")))
          }
          if (!("try-error" %in% class(error))) {
            remoteUrl <- gert::git_remote_info()$url
            if (startsWith("git@github.com:", remoteUrl)) {
              remoteUrl <- sub("^git@github\\.com:", "https://github.com/", remoteUrl)
              remoteUrl <- sub("\\.git$", "", remoteUrl)
            }

            writeLines(paste0("Repository: ", repoUrl, "\n",
                              "RemoteUrl: ", remoteUrl, "\n",
                              "RemoteRef: HEAD\n",
                              "RemoteSha: ", gert::git_commit_id(), "\n"),
                       withr::local_connection(file("DESCRIPTION", "a"))) # open DESCRIPTION in append mode
            error <- try(devtools::build())
            gert::git_reset_hard()
          }
          if ("try-error" %in% class(error)) {
            message(".:: ", fd, " ", curversion, " -> ", vkey$version, " build failed ::.")
            if (dir.exists(".git")) {
              system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
            }
          } else {
            updatePACKAGES <- TRUE
            message(".:: ", fd, " ", curversion, " -> ", vkey$version, " build success ::.")
          }
        } else {
          message(".:: ", fd, " ", curversion, " -> ", vkey$version, " invalid commit ::.")
          if (dir.exists(".git")) {
            system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
          }
        }
      } else if (as.numeric_version(curversion) < buildVersion) {
        message(".:: ", fd, " ", curversion, " -> not build as newer version (",
                buildVersion, ") is already part of the repo ::.")
      } else if (as.numeric_version(curversion) == as.numeric_version(vkey$version) &&
                 as.numeric_version(curversion) > buildVersion) {
        error <- try(devtools::install_deps(upgrade = "always"))
        if (!("try-error" %in% class(error))) {
          error <- try(devtools::build())
        }
        if ("try-error" %in% class(error)) {
          message(".:: ", fd, " ", curversion, " -> package build failed ::.")
          if (dir.exists(".git")) {
            system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
          }
        } else {
          message(".:: ", fd, " ", curversion, " -> package build success ::.")
        }
      } else {
        craninfo <- ""
        if (d %in% rownames(availablePackagesOnCran)) {
          cranversion <- availablePackagesOnCran[d, "Version"]
          if (as.numeric_version(cranversion) > as.numeric_version(curversion)) {
            warning("Package version of package \"", d, "\" is newer on CRAN (", cranversion,
                    ") compared to PIK-CRAN (", curversion, ")! Check version on CRAN immediatly!")
            craninfo <- paste0(" .::WARNING! CRAN: ", availablePackagesOnCran[d, "Version"], " !WARNING::.")
          } else {
            craninfo <- paste0(" .::CRAN: ", availablePackagesOnCran[d, "Version"], "::.")
          }
        }
        message(".:: ", fd, " ", format(curversion, width = 10), " ok ::.", craninfo)
      }
    })
  }
  if (updatePACKAGES) {
    write_PACKAGES(unpacked = TRUE)
    for (d in dirs) {
      if (d %in% skipFolders) {
        next
      }
      targz <- grep(paste0("^", d, "_.*.tar.gz"), dir(), value = TRUE)
      if (length(targz) > 1) {
        newest <- max(numeric_version(sub("^.*_", "", sub(".tar.gz$", "", targz))))
        targz <- targz[-grep(newest, targz)]
        if (file.exists(file.path("Archive", d))) {
          file.rename(targz, file.path("Archive", d, targz))
        } else {
          dir.create(file.path("Archive", d))
          file.rename(targz, file.path("Archive", d, targz))
        }
      }
    }
  }
  message("done.")
}
