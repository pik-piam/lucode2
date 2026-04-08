# validationkey

Support function which creates a key out of a version date combination

## Usage

``` r
validationkey(version, date)
```

## Arguments

- version:

  Version number of the package

- date:

  Date of the package

## Details

This function is used in [`buildLibrary`](buildLibrary.md) to offer the
package publication server an option to check whether the package has
been properly and successfully (no errors/warnings/notes) checked before
its commit.

The calculated key is not safe and can easily be reproduced, but should
be complicated enough to encourage users running the
[`buildLibrary`](buildLibrary.md) check properly.

## See also

[`buildLibrary`](buildLibrary.md)

## Author

Jan Philipp Dietrich
