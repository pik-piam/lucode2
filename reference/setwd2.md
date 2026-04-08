# setwd2

Mini function that allows you to set a directory based on a readline
input. Very useful for Windows users, as it replaces backslashes by
slashes.

## Usage

``` r
setwd2(return_only = FALSE)
```

## Arguments

- return_only:

  if TRUE, the path is not changed, but the clipboard path is returned
  as string.

## Value

if return_only=FALSE: Nothing, but the working directory is set to.
Otherwise: no working directory returned, but path transformed.

## Author

Benjamin Leon Bodirsky
