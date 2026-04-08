# Check if repo is up to date with upstream

Checks whether the local repo is up-to-date with the remote tracking
branch and the default branch of the upstream remote. Will throw an
error if not up-to-date and prints a git command to update.

## Usage

``` r
checkRepoUpToDate(pathToRepo = ".", autoCheckRepoUpToDate = TRUE)
```

## Arguments

- pathToRepo:

  The path to the git repo.

- autoCheckRepoUpToDate:

  If FALSE do not check automatically and instead just ask the user.

## Author

Pascal Sauer
