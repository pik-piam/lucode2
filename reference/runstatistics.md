# Run Statistics

Support function to collect run statistics.

## Usage

``` r
runstatistics(file = "runstatistics.Rda", overwrite = TRUE, submit = NULL, ...)
```

## Arguments

- file:

  file name the statistics are/should be stored

- overwrite:

  boolean deciding whether entries should be overwritten, if already
  existing in the file. If set to FALSE an error will be thrown in case
  that an overwrite is attempted.

- submit:

  path to a folder the run statistics should be submitted to. As soon as
  the path is set the data will be submitted, so please only set the
  path as soon as the run statistics are complete

- ...:

  entries that should be added to the run statistics file. Standard
  entries are: model, config, runtime, user, date, modelstat,
  version_management, revision and status.

## Value

An invisible list containing run statistics as stored in the given file

## Author

Jan Philipp Dietrich

## Examples

``` r
f <- tempfile()
 runstatistics(file = f, user = Sys.info()[["user"]])
 print(runstatistics(file = f))
#> $user
#> [1] "runner"
#> 
 runstatistics(file = f, submit = tempdir())
#> Submitted run statistics to /tmp/Rtmp9N3giD 
```
