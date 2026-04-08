# addGitHubActions

This function adds a standard Github action workflow called "check.yaml"
to the project which runs lucode2::check(), checks the validation key,
and creates a coverage report using codecov. This file is overwritten
automatically each time this function is run and should not be edited by
hand.

## Usage

``` r
addGitHubActions(lib = ".", config = NULL)
```

## Arguments

- lib:

  Path to the package

- config:

  The build configuration loaded from .buildlibrary. If NULL, loads it
  automatically.

## Details

\#' If the config option \`UsePkgDown\` is set to TRUE in the
\`.buildlibrary\` file, this function also adds a pkgdown.yaml workflow
that builds and deploys package documentation to GitHub Pages, and
creates a default \`\_pkgdown.yml\` configuration file if it doesn't
exist.

In addition, this function adds a codecov.yml to the repository, if not
already existing. This file is only created if missing and can be edited
manually.

## See also

[`buildLibrary`](buildLibrary.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
addGitHubActions()
} # }
```
