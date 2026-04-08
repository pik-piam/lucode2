# sendmail

A function that sends an automatic email with each push to a gitlab repo

## Usage

``` r
sendmail(
  path = NULL,
  gitrepo,
  file,
  commitmessage,
  remote = FALSE,
  reset = FALSE
)
```

## Arguments

- path:

  path to the clone of the gitlab repo (can be NULL)

- gitrepo:

  if no path is given, the gitlab repo is needed

- file:

  absolute path to the file to be committed

- commitmessage:

  the commit message (appears in the subject line of the email that will
  be sent)

- remote:

  whether communication with a remote is needed

- reset:

  whether a reset of a local copy is wanted

## Author

Anastasis Giannousakis
