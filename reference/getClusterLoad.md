# getClusterLoad

Returns information about cluster load in case that the command "sclass"
is available. Otherwise, it returns NULL

## Usage

``` r
getClusterLoad()
```

## Value

NULL, if command "sclass" is not available, otherwise returns a named
vector with current load on available partitions

## Author

Jan Philipp Dietrich
