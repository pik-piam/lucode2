# fixBuildLibraryMergeConflict

Fix merge conflicts in files auto-edited by buildLibrary (.buildlibrary,
.zenodo.json, DESCRIPTION, README.md).

## Usage

``` r
fixBuildLibraryMergeConflict(lib = ".")
```

## Arguments

- lib:

  The path to the project with a merge conflict.

## Details

The ValidationKey in .buildlibrary will be set to an empty string and
the higher version number in DESCRIPTION is used. buildLibrary needs to
be run after this function to deal with the ValidationKey and the merge
markers in .zenodo and README.md.
