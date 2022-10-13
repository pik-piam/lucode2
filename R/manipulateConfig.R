#' Replace in File
#'
#' Function to set configuration parameters in configuration files (e.g.
#' default.cfg and magpie.gms). This replacement is useful, when using R to
#' manage different model runs at once.  Please check your results after
#' replacement!
#'
#' @param configFile a character string containing the name of the
#' configuration file, that should be manipulated. Supported file formats are
#' at the moment "gms", "inc", "cfg" (R-syntax), "php", "opt" and "cmd". Other
#' formats are currently not supported
#' @param ... Variables, that should be set to new values, e.g. title="test"
#' for default.cfg or s_max_timesteps=10 for magpie.gms
#' @author Jan Philipp Dietrich, Markus Bonsch, David Klein
#' @export
#' @seealso \code{\link{manipulateFile}}
#' @examples
#' \dontrun{
#' manipulateConfig("config/default.cfg", input = "test_new_yields", title = "yihaa", revision = 4.2)
#' manipulateConfig("magpie.gms", s_max_timesteps = 4, s_use_gdx = -1)
#' }
#'
manipulateConfig <- function(configFile, ...) {
  maxchar <- 60
  arguments <- list(...)
  if (length(arguments) == 1) {
    if (is.list(arguments[[1]])) {
      arguments <- arguments[[1]]
    }
  }
  manipulations <- list()
  type <- substring(configFile, nchar(configFile) - 2)
  if (type %in% c("gms", "inc")) {
    for (key in names(arguments)) {
      value <- arguments[[key]]
      if (length(value) > 1) {
        value <- capture.output(cat(paste0(value, ", "), fill = maxchar))
        value <- paste(value, collapse = "\n\t\t")
        value <- substr(value, 1, nchar(value) - 2)
      }

      # definitions in the form
      # > $setglobal key value
      manipulations[[paste0(key, "_pattern1")]][1] <- paste0(
        "((\\n|^)[\\t ]*\\$[sS][eE][tT][gG][lL][oO][bB][aA][lL][\\t ]*", key, "[\\t ]).*?( *!!|\\n|$)")
      manipulations[[paste0(key, "_pattern1")]][2] <- paste0("\\1 ", value, "\\2")

      # definitions in the form
      # > parameter key "documentation" / value /;
      manipulations[[paste0(key, "_pattern2")]][1] <- paste0(
        "((\\n|^)[\\t ]*(scalar|parameter|set|)s?[\\t ]*",
        key,
        "(|\\([^\\)]*\\))(/|[\\t ]+(\"[^\"]*\"|)[^\"/;]*/))[^/]*")
      manipulations[[paste0(key, "_pattern2")]][2] <- paste0("\\1 ", value, " ")

      # definitions in the form
      # > key = value;
      # > key = "value";
      manipulations[[paste0(key, "_pattern3")]][1] <- paste0(
        "((^|[\\n\\t ])", key, "[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*")
      manipulations[[paste0(key, "_pattern3")]][2] <- paste0("\\1", value)
    }
  } else if (type %in% c("cfg", "R", "r")) {
    for (key in names(arguments)) {
      if (is.character(arguments[[key]])) arguments[[key]] <- paste0("\"", arguments[[key]], "\"")
      manipulations[[key]][1] <- paste0("((^|[\\t \\$])", key, "[ \\t]*<-[ \\t]*)[\"\']?[^\"\']*[\"\']?")
      manipulations[[key]][2] <- paste0("\\1", arguments[[key]])
    }
  } else if (type %in% c("cmd", ".sh")) {
    for (key in names(arguments)) {
      manipulations[[key]][1] <- paste0("((^|[\\t ])", key, "[ \\t]*=[ \\t]*[\"\']?)[^\"\'\\n]*")
      manipulations[[key]][2] <- paste0("\\1", arguments[[key]])
    }
  } else if (type == "php") {
    for (key in names(arguments)) {
      manipulations[[key]][1] <- paste0("((^|[\\t ])\\$", key, "[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*")
      manipulations[[key]][2] <- paste0("\\1", arguments[[key]])
    }
  } else if (type == "opt") {
    for (key in names(arguments)) {
      manipulations[[key]][1] <- paste0("((^|[\\t ])", key, "[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*")
      manipulations[[key]][2] <- paste0("\\1", arguments[[key]])
    }
  } else {
    stop(paste("Unknown file type", type))
  }
  manipulateFile(configFile, manipulations)
}
