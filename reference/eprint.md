# extended Print

An extended print command which formats information in a way that it is
good to use for a log-file

## Usage

``` r
eprint(var_name, envir = parent.frame())
```

## Arguments

- var_name:

  name of the variable that should be printed as string

- envir:

  environment from which the variable should be read (by default the
  environment from which the function is called)

## See also

[`eprint_list`](eprint_list.md)

## Author

Jan Philipp Dietrich, Oliver Richters

## Examples

``` r
if (FALSE) { # \dontrun{
a <- 1:3
eprint("a")
} # }

### print additional information concerning loaded configuration###
### ePrint (extended Print) offers an extended output functionality which
### allows to create easily log-files with all relevant information
```
