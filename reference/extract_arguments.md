# Extract arguments

Extracts the value (right-hand-side) of a string of the structure
"name=value" and converts it to an appropriate format. This file also
reads arguments from command line. To use this script you have to
include it by typing source("readArgs.R") in your script and call
readArgs(allowed_args) including all arguments that can be read from
command line.

## Usage

``` r
extract_arguments(inputArg)
```

## Arguments

- inputArg:

  string of the structure "name=value"

## Value

- value :

  the value (right-hand-side) of the string converted into an
  appropriate format

## See also

[`readArgs`](readArgs.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
extract_arguments("bla=1:9")
# [1] 1 2 3 4 5 6 7 8 9

extract_arguments("blub=3,5,7")
# [1] 3 5 7

extract_arguments("ble=hallo")
# [1] "hallo"
} # }
```
