#' readRuntime
#'
#' Reads all runtime information from given experiments. The runtime is given
#' in hours and is the runtime of GAMS.
#'
#' @param path Path to a run or a vector of paths.
#' @param plot Logical indicating whether the output should be plotted to a pdf
#' with the name runtime.pdf.
#' @param types A vector of names of different types of runs which should be
#' distinguished by colors. The names have to be part of the folder name in
#' order to allow the function to map the given types to the runs.
#' @param coupled Logical indicating if comparison plots should be added for
#' coupled REMIND and MAgPIE runs. \code{TRUE} automatically sets \code{types}
#' to \code{c("-rem-","-mag-")} and overwrites user defined \code{types}
#' @param outfname Optional name of the pdf. If nothing is given the default
#' "runtime" will be used.
#' @return A data frame containing the run names and runtime information in
#' hours.
#' @author David Klein
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by summarize arrange rename mutate
#' @importFrom utils install.packages
#' @importFrom withr local_dir
#' @export
readRuntime <- function(path, plot = FALSE, types = NULL, coupled = FALSE, outfname = NULL) { # nolint
  if (plot) {
    checkRequiredPackages(c("ggplot2", "lusweave"), "lucode2::readRuntime(..., plot = TRUE)")
  }
  runtime <- NULL
  maindir <- getwd()

  # ---- Read runtime data ----

  cat("\nReading runtime for", length(path), "runs\n")
  for (d in path) {
    splittedpath <- strsplit(d, "/")[[1]]
    runfolder <- splittedpath[length(splittedpath)]
    datafile <- paste0(d, "/runstatistics.rda")

    # try to read runtime data from runstatistics.rda
    tmp     <- NULL
    start   <- NULL
    end     <- NULL
    timePrepareStart <- NULL
    timePrepareEnd   <- NULL
    timeGAMSStart    <- NULL
    timeGAMSEnd      <- NULL
    timeOutputStart  <- NULL
    timeOutputEnd    <- NULL

    if (!file.exists(datafile)) {
      cat("No file found ", datafile, "\n")
    } else if (file.info(datafile)$size == 0) {
      cat("Empty file ", datafile, "\n")
    } else {
      # if file exists and it's file size is >0: load it
      stats <- NULL
      load(datafile)
      # try to load detailed runtime information
      if (!is.null(stats) && !is.null(stats$timePrepareStart)) {
        timePrepareStart <- stats$timePrepareStart
        timePrepareEnd   <- stats$timePrepareEnd
        timeGAMSStart    <- stats$timeGAMSStart
        timeGAMSEnd      <- stats$timeGAMSEnd
        timeOutputStart  <- stats$timeOutputStart
        timeOutputEnd    <- stats$timeOutputEnd
      } else if (!is.null(stats) && !is.null(stats$starttime)) {
        # if no detailed information is available load the old one (it's only the gams runtime)
        start <- stats$starttime
        end   <- stats$endtime
      }
    }

    # if no start and end was extractable from runstatistics.rda
    # conclude it from timestamps of the files in the results folder
    if (is.null(end) && is.null(timePrepareEnd) && is.null(timeGAMSEnd) && is.null(timeOutputEnd)) {
      local_dir(d)
      # find all files
      info <- file.info(dir())
      # sort files in info by mtime
      info <- info[order(info$mtime), ]
      # save time of first file in the list (oldest)
      start <- info[1, ]$mtime
      # save time if last file in the list (newest)

      if ("report.rds" %in% rownames(info)) {
        # if run has finished normally the report.rds file should exist. In this case take the newest file
        cat("Using the newest file in", runfolder, "as end\n")
        end <- tail(info$mtime, n = 1)
      } else {
        # if report.rds does not exist, this indicates that the run did not finish properly and the mif file has been
        # generated manually later without also producing the report.rds
        # In this case do not take the newest file (which is the manually and belated produced mif file) but take the
        # full.lst which is the newest file before the mif file
        cat("Using", runfolder, "full.lst as end\n")
        end <- info["full.lst", ]$mtime
      }
      local_dir(maindir)
    }

    # if (total) runtime data was found
    if (all(c(!is.null(start), !is.null(end)))) {
      # need to be transformed to NA otherwise rbind would not work if one of them is NULL
      tmp <- end - start
      units(tmp) <- "hours"
      if (is.null(start)) start <- NA
      if (is.null(end)) end   <- NA
      new <- data.frame(run = runfolder, type = "NA", section = "total", value = tmp, start = start, end = end,
                        stringsAsFactors = FALSE)
      runtime <- rbind(runtime, new)
    }

    # if detailed runtime data was found append it
    if (!is.null(timePrepareEnd)) {
      tmp <- timePrepareEnd - timePrepareStart
      units(tmp) <- "hours"
      new <- data.frame(run = runfolder, type = "NA", section = "prep", value = tmp, start = timePrepareStart,
                        end = timePrepareEnd, stringsAsFactors = FALSE)
      runtime <- rbind(runtime, new)
    }

    if (!is.null(timeGAMSEnd)) {
      tmp <- timeGAMSEnd - timeGAMSStart
      units(tmp) <- "hours"
      new <- data.frame(run = runfolder, type = "NA", section = "GAMS", value = tmp, start = timeGAMSStart,
                        end = timeGAMSEnd, stringsAsFactors = FALSE)
      runtime <- rbind(runtime, new)
    }

    if (!is.null(timeOutputEnd)) {
      tmp <- timeOutputEnd - timeOutputStart
      units(tmp) <- "hours"
      new <- data.frame(run = runfolder, type = "NA", section = "output", value = tmp, start = timeOutputStart,
                        end = timeOutputEnd, stringsAsFactors = FALSE)
      runtime <- rbind(runtime, new)
    }
  }

  if (coupled) {
    types <- c("-rem-", "-mag-")
  }

  # define "types"-column
  if (!is.null(types)) {
    for (tt in types) {
      runtime$type[grep(tt, runtime$run)] <- tt
    }
  }

  # cosmetics: order levels for better order in plots (starting with remind) and remove "-"
  if (identical(types, c("-rem-", "-mag-"))) {
    runtime$type <- sub("(-)(rem|mag)(-)", "\\2", runtime$type)
    runtime$type <- ordered(factor(runtime$type), levels = c("rem", "mag"))
  }

  if (is.null(runtime)) {
    warning("No runtime information found for all runs!")
  }
  res <- runtime # save runtime for returning it before it is modified below

  saveRDS(runtime, file = "runtime.rds")

  # ----  Generate plots ----
  if (plot) {
    cat("\nPreparing pdf with runtime plots.\n")
    out <- lusweave::swopen(template = "david")
    if (coupled) {
      # change runnames
      itnumber <- sub(".*-([0-9]{1,2}$)", "\\1", runtime$run) # replace everything with pattern contained in brackets
      runname  <- sub("-(rem|mag)(-[0-9]{1,2}$)", "", runtime$run) # replace pattern contained in brackets with nothing

      # replace runnames
      runtime$run <- runname

      # add column with iteration number, convert to numeric for sorting on x-axis in plot
      runtime$it <- itnumber
      runtime$it <- as.numeric(runtime$it)

      # plot runtime over iterations of all runs
      # Calculate total runtime from sub sections
      tot <- runtime %>% group_by(.data$run, .data$type, .data$it) %>% summarize(total = sum(.data$value))

      pIteration <- ggplot2::ggplot(data = tot, ggplot2::aes_string(x = "it", y = "total", fill = "type")) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
        ggplot2::scale_x_continuous(breaks = runtime$it) +
        ggplot2::facet_wrap(~run) +
        ggplot2::scale_y_continuous() +
        ggplot2::xlab("Coupling iteration") +
        ggplot2::ylab("Hours")
      lusweave::swfigure(out, print, pIteration, sw_option = "height=9,width=16")
    }

    # Order runs descending by runtime
    # step 1: calculate total runtime for each run and order ascending (in new data frame)
    tot <- runtime %>% group_by(.data$run) %>% summarize(total = sum(.data$value)) %>% arrange(.data$total)
    # step 2: use the order of runs in this new data frame to order levels of "run" in runtime accordingly
    runtime$run <- ordered(factor(runtime$run), levels = tot$run)

    # Plot: compare runs of all scenarios
    # Convert hours to days if total runtime is longer than 3 days
    if (max(tot$total, na.rm = TRUE) > 24 * 3) {
      yUnit <- "days"
      runtime$value <- runtime$value / 24
    } else {
      yUnit <- "hours"
    }

    pSorted <- ggplot2::ggplot(data = runtime,
                               ggplot2::aes_string(x = "run",
                                                   y = "value",
                                                   fill = ifelse(is.null(types), "NULL", "type"))) +
      ggplot2::geom_bar(colour = "black", stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::ylab(yUnit) +
      ggplot2::ggtitle("Ordered by runtime") +
      ggplot2::scale_y_continuous() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    lusweave::swfigure(out, print, pSorted, sw_option = "height=9,width=16")

    # sort runs and levels by starttime (to have the right order in the plots)
    dat <- runtime %>%
      arrange(.data$start) %>%
      mutate(run = factor(.data$run, levels = rev(unique(.data$run)), ordered = TRUE),
             section = factor(.data$section, levels = c("total", "prep", "GAMS", "output"), ordered = TRUE))

    pTimeline <- ggplot2::ggplot(dat, ggplot2::aes_string(color = ifelse(is.null(types), "NULL", "type"),
                                                          alpha = "section")) +
      ggplot2::geom_segment(ggplot2::aes_string(x = "start", xend = "end", y = "run", yend = "run"), size = 6) +
      ggplot2::scale_alpha_manual(values = c("total" = 1, "prep" = 0.5, "GAMS" = 1, "output" = 0.5)) +
      ggplot2::scale_color_manual(values = c("rem" = "royalblue3", "mag" = "seagreen")) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::theme(legend.justification = c(1, 1),
                     legend.position = c(0.99, 0.99),
                     legend.title = ggplot2::element_blank())

    lusweave::swfigure(out, print, pTimeline, sw_option = "height=9,width=16")

    # Calculate and display statistics
    dat <- dat %>% rename(duration = .data$value, model = .data$type)
    x <- as.numeric(max(dat$end) - min(dat$start), units = "hours")
    lusweave::swlatex(out, paste0("From start to end:\\newline  ", round(x), " hours\\newline\\newline"))

    lusweave::swlatex(out, "Average runtime (including preparation and output):\\newline")
    x <- dat %>%
      group_by(.data$model, .data$run, .data$it) %>%
      # sum duration over section (prep, GAMS, output) for each model, run, and iteration
      summarize(duration_single = sum(.data$duration)) %>%
      # remove groups of run and iteration
      # average duration across iteration and runs for each model
      group_by(.data$model) %>%
      summarize(single_mean = mean(.data$duration_single))

    for (m in x$model) {
      lusweave::swlatex(out, paste0(" ", m, " ", round(x[x$model == m, ]$single_mean, 1), " hours\\newline"))
    }
    lusweave::swlatex(out, paste0("\\newline"))

    lusweave::swlatex(out, "Average total runtime (sum over iterations, average over scenarios):\\newline")
    x <- dat %>%
      group_by(.data$model, .data$run) %>%
      summarize(total = sum(.data$duration)) %>%
      summarize(duration_model_mean = mean(.data$total))
    for (m in x$model) {
      lusweave::swlatex(out, paste0(" ", m, " ", round(x[x$model == m, ]$duration_model_mean, 1), " hours\\newline"))
    }

    x <- dat %>%
      group_by(.data$run) %>%
      summarize(duration_coupled = sum(.data$duration)) %>%
      summarize(duration_mean = mean(.data$duration_coupled))
    lusweave::swlatex(out, paste0("coupled ",
                                  round(as.numeric(x$duration_mean, units = "hours"), 1),
                                  " hours\\newline\\newline"))

    lusweave::swlatex(out, "Number of runs:\\newline")
    x <- dat %>%
      group_by(.data$model, .data$section) %>%
      summarize(no_of_runs = length(.data$model)) %>%
      summarize(no_of_runs = mean(.data$no_of_runs))
    for (m in x$model) {
      lusweave::swlatex(out, paste0(m, " ", x[x$model == m, ]$no_of_runs, "\\newline"))
    }
    lusweave::swlatex(out, "\\newline")

    lusweave::swlatex(out, "Total runtime:\\newline")
    x <- dat %>% group_by(.data$model) %>% summarize(total_time = sum(.data$duration))
    for (m in x$model) {
      lusweave::swlatex(out, paste0(m, " ", round(as.numeric(x[x$model == m, ]$total_time, units = "days"), 1),
                                    " days\\newline"))
    }
    lusweave::swlatex(out, paste0(" Total ", round(sum(as.numeric(x$total_time, units = "days")), 1), " days\\newline"))

    if (is.null(outfname)) outfname <- "runtime"
    lusweave::swclose(out, outfile = paste0(outfname, ".pdf"), clean_output = TRUE)
  }
  invisible(res)
}
