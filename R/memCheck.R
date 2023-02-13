#' Memory usage Check
#'
#' Function checks memory usage and shows the biggest objects in the given
#' environment
#'
#' This function is based on an idea posted at stack overflow:
#' http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'
#' @param order.by Column based on which the data should be sorted
#' @param decreasing Determines whether the values should be in an increasing
#' or decreasing order
#' @param n Limit of number of elements that should be shown. NULL means no
#' limit
#' @param envir Environment which should be analyzed, but default the parent
#' environment relative to this function.
#' @param gc Determines whether the garbage collector should be executed at the
#' end of the function for additional information
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' memCheck()
#' }
#' @importFrom utils capture.output object.size head
#' @export
memCheck <- function(order.by = "Size", # nolint: object_name_linter.
                     decreasing = TRUE, n = NULL, envir = parent.frame(), gc = TRUE) {
  napply <- function(names, fn, pos) {
    sapply(names, function(x) fn(get(x, pos = pos))) # nolint: undesirable_function_linter.
  }
  names <- ls(envir)
  if (length(names) == 0) {
    cat("\nEnvironment is empty!\n")
    return(NULL)
  }
  objClass <- napply(names, function(x) as.character(class(x))[1], envir)
  objMode <- napply(names, mode, envir)
  objType <- ifelse(is.na(objClass), objMode, objClass)
  objPrettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto"))
  }, envir)
  objSize <- napply(names, object.size, envir)
  objDim <- t(napply(names, function(x) as.numeric(dim(x))[1:2], envir))
  vec <- is.na(objDim)[, 1] & (objType != "function")
  objDim[vec, 1] <- napply(names, length, envir)[vec]
  out <- data.frame(objType, objSize, objPrettysize, objDim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!is.null(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (!is.null(n))
    out <- head(out, n)
  cat("\n")
  print(out)
  if (gc) {
    cat("\n")
    print(gc(reset = FALSE))
  }
  cat("\n")
}
