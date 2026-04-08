# Merge Statistics

Support function to merge run statistics which have been derived with
[`runstatistics`](runstatistics.md)

## Usage

``` r
mergestatistics(
  dir = ".",
  file = NULL,
  renew = FALSE,
  quickcheck = FALSE,
  pattern = "*\\.[rR]da",
  removeCols = NULL,
  keepCols = NULL
)
```

## Arguments

- dir:

  Path to the run statistics repository

- file:

  path to an rds-file the data should be written to and from which (if
  existing) already merged data can be read from

- renew:

  if set to TRUE the full data.table will be created again from scratch,
  if set to FALSE merging will start with the existing file (if it
  exists) and just add missing entries

- quickcheck:

  If active, the function compares last modification dates of repository
  data and and merged statistics and cancels execution in case that
  there is no newer file in the data repository (assuming that merge
  statistics are already complete). This is useful if this function is
  run frequently and execution time plays a role, but might lead to
  cases in which the function is not run even if the merge statistics
  are incomplete.

- pattern:

  detection pattern for rda files that should be merged

- removeCols:

  vector of columns that will be filtered out

- keepCols:

  only these columns will be kept, if NULL all columns will be kept

## Value

A data table containing the merged run statistics or NULL in case the
data was not recalculated

## Author

Jan Philipp Dietrich
