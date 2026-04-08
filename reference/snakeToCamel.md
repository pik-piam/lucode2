# snakeToCamel

Convenience function to rename variables in an R file from snake to
camel case.

## Usage

``` r
snakeToCamel(pathToFile, ask = TRUE)
```

## Arguments

- pathToFile:

  Path to the R source file where variables should be renamed.

- ask:

  If TRUE (default) ask before renaming a variable, otherwise always
  assume "yes" as answer.
