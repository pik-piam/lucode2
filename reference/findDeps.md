# findDeps

Find all dependencies of an R project.

## Usage

``` r
findDeps(devDeps = TRUE)
```

## Arguments

- devDeps:

  Whether development dependencies should also be checked.

## Value

A dataframe documenting which dependency is needed where.

## Details

This is a wrapper around \`renv::dependencies()\` that does not report
dependencies on core R packages, because these are always available.

## See also

[`dependencies`](https://rstudio.github.io/renv/reference/dependencies.html)

## Author

Pascal Sauer
