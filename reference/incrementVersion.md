# incrementVersion

Increment a version number at the specified position.

## Usage

``` r
incrementVersion(currentVersion, position, defLengths = 3)
```

## Arguments

- currentVersion:

  The current package version as a string like "1.23.456"

- position:

  An integer defining which part of the version number will be
  increased. Use 1 for major version, 2 for minor, 3 for patch/bugfix, 4
  for development.

- defLengths:

  An integer defining how many parts make up the resulting version
  number.

## Value

The new version string.

## See also

[`buildLibrary`](buildLibrary.md)

## Author

Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal
Sauer

## Examples

``` r
lucode2:::incrementVersion("1.23.45", 3)
#> [1] ‘1.23.46’
```
