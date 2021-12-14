#' Renames the scenariofolder and the scenario contained in it
#'
#' Use this function to change the name of a run after it has finished.
#' This function renames the run folder, change the run title in the cfg and in the reporting.
#' This can be useful if the initial name of a run was not meaningful. However, inconsistencies will
#' remain, since the function will NOT rename the scenario in the list file, the gdx, and the results database.
#'
#' @param map Named vector, containing the new scenario names as elements and the corresponding old folder names as the
#'            emelents' names.
#' @param keep_time_stamp Logical indicating whether timestamp of old folder name should be transferred to new folder
#'                        name or not (default = FALSE).
#' @author David Klein
#' @export

rename_scenario <- function(map, keep_time_stamp = FALSE) { # nolint
  if (any(duplicated(map))) {
    stop("The list of new names contains duplicates which would cause overwrites, aborting.")
  }

  for (f in seq_along(map)) {
    cat("\nRenaming\n")
    oldFolder <- names(map[f])

    if (dir.exists(oldFolder)) {
      # extract date string of the form "_yyyy-mm-dd_hh.mm.ss"
      d <- gsub(".*(_\\d{4}-\\d{2}-\\d{2}_\\d{2}.\\d{2}.\\d{2}$)", "\\1", oldFolder)
      # if no timestamp was found set it to empty string (so it will have no effect in gsub below)
      if (identical(d, oldFolder)) d <- ""
      # remove date
      oldScenario <- gsub(d, "", oldFolder)
      # if timestamp should not be kept set it to empty string (so it will have no effect in paste0 below)
      if (!keep_time_stamp) d <- ""
      newFolder <- paste0(map[f], d)
      if (dir.exists(newFolder)) {
        warning("Folder with the name ", newFolder, " already exists! Skipped this one!")
        next
      }

      newScenario <- map[f]

      cat("scenario from:", oldScenario, "\n           -> ", newScenario, "\n")
      # rename title
      cfgName <- paste0(oldFolder, "/config.Rdata")
      if (file.exists(cfgName)) {
        load(cfgName)
        cfg$title <- newScenario
        save(cfg, file = cfgName)
      } else {
        warning("Could not find", cfgName)
      }

      # rename scenario in mif: first read REMIND report, if not existing read MAgPIE report
      mifName <- paste0(oldFolder, "/report.mif")
      newMifName <- mifName

      if (!file.exists(mifName)) {
        mifName    <- paste0(oldFolder, "/REMIND_generic_", oldScenario, ".mif")
        newMifName <- paste0(oldFolder, "/REMIND_generic_", newScenario, ".mif")
      }

      if (file.exists(mifName)) {
        checkRequiredPackages("magclass", "lucode2::rename_scenario()")
        cat("mif      from:", mifName, "\n           -> ", newMifName, "\n")
        a <- magclass::read.report(mifName, as.list = FALSE)
        magclass::getItems(a, dim = 3.1) <- unname(gsub("\\.", "", newScenario))
        magclass::write.report(a, newMifName)
      } else {
        warning("Could not find", mifName, " nor ", paste0(oldFolder, "/report.mif"))
      }

      # rename folder
      cat("folder   from:", oldFolder, "\n           -> ", newFolder, "\n")
      system(paste("mv", oldFolder, newFolder))
    } else {
      cat("Could not find", oldFolder, "!\n")
    }
  }
}
