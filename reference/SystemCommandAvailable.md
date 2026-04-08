# System Command Available

Checks whether a system command is available (does not return an error),
or not

## Usage

``` r
SystemCommandAvailable(command)
```

## Arguments

- command:

  System command to be tested

## Value

Boolean indicating whether the command is available, or not

## Author

Jan Philipp Dietrich

## Examples

``` r
SystemCommandAvailable("ls")
#> [1] TRUE
```
