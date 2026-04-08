# checkRequiredPackages

Check if one or more packages are available and try to install the
missing packages. Throw an error if at least one of the required
packages is still missing after the installation attempt.

## Usage

``` r
checkRequiredPackages(
  requiredPackages,
  requiredFor = "",
  installFunction = install.packages,
  readlineFunction = readline,
  libPaths = .libPaths()
)
```

## Arguments

- requiredPackages:

  One or more names of packages that are checked using requireNamespace.

- requiredFor:

  Optional single string. What the packages are required for, usually
  the name of a function.

- installFunction:

  Optional function, defaults to install.packages. Will be called during
  checkRequiredPackages like this: installFunction(missingPackages,
  libPaths\[\[1\]\]). Only needed if the required packages are not
  available on CRAN or the configured repos (getOption("repos")). In
  that case you might want to use something like this: function(...)
  remotes::install_github("USDA-ERS/MTED-HARr").

- readlineFunction:

  This argument was added for testing. A function to get an answer from
  the user.

- libPaths:

  This argument was added for testing. Where to look for and install the
  required packages.

## See also

[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
checkRequiredPackages(c("ggplot2", "lusweave"), "lucode2::readRuntime(..., plot = TRUE)")
} # }
```
