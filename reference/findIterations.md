# findIterations

Collects paths to all coupled runs (iterations) in `modelpath` that
contain `runname`. For each entry in `runname` the paths are sorted by
the modification time of the respective fulldata.gdx

## Usage

``` r
findIterations(runname, modelpath = ".", latest = FALSE)
```

## Arguments

- runname:

  Scenarioname or vector of scenarionames.

- modelpath:

  Path or vector of paths where iterations are searched for.

- latest:

  Logical indicating if only the latest iteration of a runname is
  returned.

## Value

A vector containing the paths to the iterations of coupled runs.

## Author

David Klein
