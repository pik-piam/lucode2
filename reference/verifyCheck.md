# verifyCheck

Run R CMD check completely without stopping. Then stop on errors, or
unaccepted warnings and notes.

## Usage

``` r
verifyCheck(cran, acceptedWarnings, acceptedNotes)
```

## Arguments

- cran:

  Passed to devtools::check

- acceptedWarnings:

  A character vector of regular expressions. A warning will result in an
  error unless it matches one of these regular expressions.

- acceptedNotes:

  A character vector of regular expressions. A note will result in an
  error unless it matches one of these regular expressions.

## Author

Pascal Sauer
