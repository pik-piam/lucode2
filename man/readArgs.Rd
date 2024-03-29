% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readArgs.R
\name{readArgs}
\alias{readArgs}
\title{Read Arguments from command line}
\usage{
readArgs(
  ...,
  .argv = commandArgs(trailingOnly = TRUE),
  .envir = parent.frame(),
  .flags = NULL,
  .silent = FALSE
)
}
\arguments{
\item{\dots}{arguments allowed to be read from command line (other values
are ignored). Value is set if found on command line input, nothing is done,
if value is not found.}

\item{.argv}{command line arguments, usually read with commandArgs, can be specified
for testing purposes}

\item{.envir}{environment in which the variables should be written (by
default the environment from which the function is called)}

\item{.flags}{named vector with possible command line switches. Element names
are short flags used with one dash, corresponding elements the long form
including two dashes: c(t = "--test") will interpret "-t" in command line as "--test"}

\item{.silent}{boolean which allows to suppress status messages}
}
\value{
vector of activated flags, if any
}
\description{
Function reads arguments from command line of the structure value=content
and transforms them to R-Values, if they are called as allowed arguments.
}
\examples{

# Create an R-file "test.R" with following code:
value1 <- "old"
value2 <- 2
value3 <- "willstaythesame"
flags <- readArgs("value1", "value2", "value4", .flags = c(t = "--test", p = "--parallel"))
message(value1)
message(value2)
message(value3)
if ("--test" \%in\% flags) {
  message("You are in test mode")
}
if ("--parallel" \%in\% flags) {
  message("You are in parallel mode")
}

# Open the command line and execute the following code:
# Rscript test.R -t --parallel value1=new value2=3 value3=isnotallowed

# Output:
#
# ### READ COMMAND LINE - ASSIGNED CONFIGURATION ###
# value1 <- new
# value2 <- 3
# value4 not defined
# Flags: --parallel, --test
# ### READ COMMAND LINE - CONFIGURATION END ###
#
# new
# 3
# willstaythesame
# You are in test mode
# You are in parallel mode


### function that reads all allowed arguments from command line ###
}
\seealso{
\code{\link{manipulateConfig}}
}
\author{
Jan Philipp Dietrich, Oliver Richters
}
