# packageInfo

Function to print version number and time since last update formatted to
standard output. Considers CRAN, the RSE server, and r-universe.

## Usage

``` r
packageInfo(
  package,
  repos = c("https://cran.rstudio.com/", "https://rse.pik-potsdam.de/r/packages/",
    "https://pik-piam.r-universe.dev")
)
```

## Arguments

- package:

  Package name

- repos:

  vector of package repositories in which availability of the package
  should be checked

## Author

Jan Philipp Dietrich
