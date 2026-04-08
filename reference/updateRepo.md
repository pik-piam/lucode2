# Update package repository

Function to update a package repository. Run this function on a folder
which contains packages sources as subfolders. Packages should be
managed via git in order to be updated properly. To add a new package to
the repo just checkout/clone it into this folder. The function will
automatically detect the new package and add it.

## Usage

``` r
updateRepo(
  path = ".",
  check = TRUE,
  forceRebuild = FALSE,
  clean = TRUE,
  skipFolders = "Archive",
  repoUrl = "https://rse.pik-potsdam.de/r/packages"
)
```

## Arguments

- path:

  Path to the repository

- check:

  Boolean deciding whether package must have been checked or not in
  order to be distributed

- forceRebuild:

  Option to rebuild all packages from source

- clean:

  Option to clean repos before updating/pulling to avoid merge conflicts

- skipFolders:

  Which folders/packages should not be built.

- repoUrl:

  Url of the package repo. Will be added to DESCRIPTION files in the
  Repository field.

## See also

[`buildLibrary`](buildLibrary.md)

## Author

Jan Philipp Dietrich, Pascal Sauer
