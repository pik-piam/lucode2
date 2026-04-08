# Package version check tool

Checks if there are CRAN-packages with the same name as those in our
PIK-CRAN whose version is newer

## Usage

``` r
check_versions(mail = TRUE, test = FALSE, gitpath = NULL)
```

## Arguments

- mail:

  whether an email notification is sent to RSE

- test:

  to test whether auto email sending works

- gitpath:

  if an email notification has to be sent out, the path to the git repo

## Author

Anastasis Giannousakis
