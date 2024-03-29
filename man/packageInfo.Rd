% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packageInfo.R
\name{packageInfo}
\alias{packageInfo}
\title{packageInfo}
\usage{
packageInfo(
  package,
  repos = c("https://cran.rstudio.com/", "https://rse.pik-potsdam.de/r/packages/",
    "https://pik-piam.r-universe.dev")
)
}
\arguments{
\item{package}{Package name}

\item{repos}{vector of package repositories in which availability of the package should
be checked}
}
\description{
Function to print version number and time since last update formatted to standard output.
Considers CRAN, the RSE server, and r-universe.
}
\author{
Jan Philipp Dietrich
}
