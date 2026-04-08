# rmAllbut

Removes all objects except specified ones from the workspace

## Usage

``` r
rmAllbut(..., list = character(), clean = TRUE)
```

## Arguments

- ...:

  Objects that should be kept

- list:

  List specifying the objects to be kept. Same as in
  [`rm`](https://rdrr.io/r/base/rm.html).

- clean:

  Boolean to specify if a [`gc`](https://rdrr.io/r/base/gc.html) shall
  be executed

## Details

Helps to clean the workspace. Only objects specified in `...` survive.
Specify clean =TRUE to really free the memory.

## See also

[`rm`](https://rdrr.io/r/base/rm.html),[`ls`](https://rdrr.io/r/base/ls.html)

## Author

Markus Bonsch

## Examples

``` r
# Create some objects
a <- 1
b <- 2
c <- 3
# show them
ls()
#> [1] "a" "b" "c"
# delete all but b and c
rmAllbut(b, c)
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1383156 73.9    2219965 118.6  2219965 118.6
#> Vcells 2579598 19.7    8388608  64.0  6343076  48.4
ls()
#> [1] "b" "c"
# delete all but b
test <- "b"
rmAllbut(list = test)
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1383107 73.9    2219965 118.6  2219965 118.6
#> Vcells 2579511 19.7    8388608  64.0  6343076  48.4
ls()
#> [1] "b"
```
