# validkey

Support function which validates a key out of a version date combination

## Usage

``` r
validkey(package = ".", stopIfInvalid = FALSE)
```

## Arguments

- package:

  Path to the package

- stopIfInvalid:

  logical; whether to stop if the key is invalid.

## Value

list with version, date and result of validation test

## Details

This function is used to check whether [`buildLibrary`](buildLibrary.md)
has been run properly and without problems or not

## See also

[`buildLibrary`](buildLibrary.md)

## Author

Jan Philipp Dietrich
