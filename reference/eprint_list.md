# Extended list print

Same as [`eprint`](eprint.md), but expecting a vector with variable
names

## Usage

``` r
eprint_list(var_list, envir = parent.frame())
```

## Arguments

- var_list:

  Vector containing names of several variables that should be printed

- envir:

  environment from which the variable should be read (by default the
  environment from which the function is called)

## See also

[`eprint`](eprint.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- 1:3
b <- "blub"
lucode2:::eprint_list(c("a", "b"))
#> a <- 1, 2, 3
#> b <- blub
```
