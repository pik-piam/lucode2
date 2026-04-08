# Produces the reporting mif files where they are missing

For coupled runs: Searches the output folder for all existing run
folders, checks which of them are currently running on the cluster,
ignores them, checks for the remaining runs whether there is a
reporting. Produces the reporting if it is missing. \#'

## Usage

``` r
produce_missing_reports(modeldir = "./")
```

## Arguments

- modeldir:

  Path to the main folder of REMIND or MAgPIE.

## Author

David Klein
