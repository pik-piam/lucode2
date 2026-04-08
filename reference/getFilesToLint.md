# getFilesToLint

Get the R files the current git user is responsible for to pass them to
the auto-formatter and/or linter.

## Usage

``` r
getFilesToLint(pathToGitRepo = ".")
```

## Arguments

- pathToGitRepo:

  path to a git repository

## Details

The files of interest are identified using git via system() (and shell()
for windows). All currently untracked files and changed files (both
staged and unstaged) are collected, as well as files that were changed
in non-merge commits authored by the current git user since the last
version commit (a commit where .buildLibrary was changed, presumably to
increase the version number). Of those the absolute paths to .R, .Rmd
and .Rnw files are returned as a character vector.

## See also

[`lint`](lint.md), [`autoFormat`](autoFormat.md)

## Author

Pascal Sauer

## Examples

``` r
lucode2:::getFilesToLint()
#> Warning: running command 'git config user.email' had status 1
#> character(0)
```
