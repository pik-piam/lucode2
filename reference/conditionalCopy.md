# conditionalCopy

Copy a file from lucode2 into the current package.

## Usage

``` r
conditionalCopy(relativePath, nameInInstExtdata = basename(relativePath))
```

## Arguments

- relativePath:

  The destination to copy to.

- nameInInstExtdata:

  The source file name in lucode2's extdata, if it differs from
  relativePath's basename.

## Details

For normal packages, it will simply overwrite the given file from the
corresponding file in lucode2's extdata. For lucode2 itself, it instead
checks if the file in extdata matches the file in the main folder. If
not, it asks if the file in extdata should be updated.
