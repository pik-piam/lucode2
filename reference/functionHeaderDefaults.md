# functionHeaderDefaults

used to quickly read in the default values of a function

## Usage

``` r
functionHeaderDefaults(...)
```

## Arguments

- ...:

  parameters that shall be assigned to the global environment of R

## Value

no direct return, values are assigned to .GlobalEnv

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
test <- function(a = "klk", b = "kjlkv", kk = 3) {
paste(a, b, kk)
}
functionHeaderDefaults(a = "klk", b = "kjlkv", kk = 3)
print(a)
paste(a, b, kk)
} # }
```
