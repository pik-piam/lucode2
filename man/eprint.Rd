% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eprint.R
\name{eprint}
\alias{eprint}
\title{extended Print}
\usage{
eprint(var_name, envir = parent.frame())
}
\arguments{
\item{var_name}{name of the variable that should be printed as string}

\item{envir}{environment from which the variable should be read (by default
the environment from which the function is called)}
}
\description{
An extended print command which formats information in a way that it is good
to use for a log-file
}
\examples{
\dontrun{
a <- 1:3
eprint("a")
}

### print additional information concerning loaded configuration###
### ePrint (extended Print) offers an extended output functionality which
### allows to create easily log-files with all relevant information
}
\seealso{
\code{\link{eprint_list}}
}
\author{
Jan Philipp Dietrich, Oliver Richters
}
