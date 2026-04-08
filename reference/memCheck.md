# Memory usage Check

Function checks memory usage and shows the biggest objects in the given
environment

## Usage

``` r
memCheck(
  order.by = "Size",
  decreasing = TRUE,
  n = NULL,
  envir = parent.frame(),
  gc = TRUE
)
```

## Arguments

- order.by:

  Column based on which the data should be sorted

- decreasing:

  Determines whether the values should be in an increasing or decreasing
  order

- n:

  Limit of number of elements that should be shown. NULL means no limit

- envir:

  Environment which should be analyzed, but default the parent
  environment relative to this function.

- gc:

  Determines whether the garbage collector should be executed at the end
  of the function for additional information

## Details

This function is based on an idea posted at stack overflow:
http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
memCheck()
} # }
```
