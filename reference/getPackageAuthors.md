# getPackageAuthors

Creates a suggestion for an Authors entry for the DESCRIPTION of a
package. Suggestion is based on author information in roxygen headers
and authors are ranked based on number of mentionings.

## Usage

``` r
getPackageAuthors(folder = "R")
```

## Arguments

- folder:

  R folder of the package

## Details

Please be aware that the output will most likely require some manual
processing before it can be used in the DESCRIPTION!

## Author

Jan Philipp Dietrich
