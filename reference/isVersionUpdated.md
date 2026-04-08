# isVersionUpdated

Checks if the version number in the DESCRIPTION file of a given package
has been updated (may not be a version number for development stage
packages).

## Usage

``` r
isVersionUpdated(
  repo = "https://rse.pik-potsdam.de/r/packages/",
  config = loadBuildLibraryConfig()
)
```

## Arguments

- repo:

  package repository to determine latest version

- config:

  A configuration defining enforceVersionUpdate. By default the
  .buildLibrary file is read.

## Author

Falk Benke
