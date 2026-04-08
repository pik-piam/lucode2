# buildLibrary

Builds R libraries. Includes checks for consistency. Find solutions to
common problems at
https://github.com/pik-piam/discussions/discussions/18

## Usage

``` r
buildLibrary(
  lib = ".",
  cran = TRUE,
  updateType = NULL,
  updateLucode2 = TRUE,
  autoCheckRepoUpToDate = TRUE
)
```

## Arguments

- lib:

  Path to the package

- cran:

  If cran-like test is needed

- updateType:

  Either an integer or character string:

  |             |               |                                        |
  |-------------|---------------|----------------------------------------|
  | **number**  | **string**    | **description**                        |
  | 1           | `major`       | for API breaking changes               |
  | 2 (default) | `minor`       | for new features or improvements       |
  | 3           | `patch`       | for bugfixes and corrections           |
  | 4           | `development` | only for packages in development stage |
  | 0           | `none`        | version has already been incremented   |

- updateLucode2:

  Update lucode2 if possible and run buildLibrary with new version
  instead.

- autoCheckRepoUpToDate:

  Automatically check if your repository is up to date. If FALSE the
  user is asked.

## Details

This function is designed to help building and checking R libraries. It
performs the following steps:

- Version: Determination of a new version number (Can also be defined by
  the user).

- Date: Determination of a the date of the build (Can also be defined by
  the user).

- Linter: Check for code style problems.

- R check: Check whether the library is consistent and can be built.

- Package building: Builds the .zip and .tar.gz packages under windows.
  Under linux, only the .tar.gz package is built.

## Note

The behavior of buildLibrary can be configured via the `.buildLibrary`
file in the main folder of the package. It uses YAML format and can
contain the following entries:

- **ValidationKey**: This entry always exists and is written
  automatically by `buildLibrary` It confirms that the package has been
  successfully build via the function.

- **AutocreateReadme** (optional): yes/no - decides whether
  `buildLibrary` automatically updates the README.md file or not
  (default: yes)

- **AutocreateCITATION** (optional): yes/no - decides whether
  `buildLibrary` automatically creates the CITATION.cff file or not
  (default: yes)

- **AddInReadme** (optional): Additional entries to be added to the
  autocreated README. Provided either in markdown format or as paths to
  RMarkdown (Rmd) or Markdown (md) files

- **AddLogoReadme** (optional): Additional logo to be added to the
  autocreated README. Provided as path to logo in PNG format

- **LogoHeightReadme** (optional): Height of the logo in README in px

- **AcceptedWarnings** (optional): a list of Warnings which should be
  ignored by `buildLibrary` (autocompletion via asterisks allowed)

- **AcceptedNotes** (optional): a list of Notes which should be ignored
  by `buildLibrary` (autocompletion via asterisks allowed)

- **allowLinterWarnings**: yes/no - If set to "no" linter warnings will
  stop the build process. (default: yes)

- **enforceVersionUpdate**: yes/no - If set to "yes", the version number
  must be incremented to a non-dev version number for the build process
  to succeed. (default: no)

- **skipCoverage**: yes/no - If set to "yes", running code coverage
  using covr as part of the GitHub workflow will be skipped. (default:
  no)

- **UsePkgDown**: yes/no - If set to "yes", a GitHub Action workflow
  will be added that builds and deploys pkgdown documentation to GitHub
  Pages. (default: no)

## See also

[`package2readme`](package2readme.md), [`lint`](lint.md),
[`autoFormat`](autoFormat.md)

## Author

Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch, Pascal
Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
buildLibrary()
} # }
```
