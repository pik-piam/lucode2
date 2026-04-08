# Replace in File

Function to replace a specific text string in a text file. Useful to
manipulate GAMS sourcecode files.

## Usage

``` r
manipulateFile(file, manipulations, perl = TRUE, ...)
```

## Arguments

- file:

  a connection object or a character string describing the file, that
  should be manipulated.

- manipulations:

  A list of 2 element vectors, containing the search phrase as first
  element and the replace term as second element.

- perl:

  usually set to TRUE so regular expressions in perl syntax (including
  backreferencing) can be used. If fixed = TRUE is specified in ...,
  perl is set to FALSE

- ...:

  Further options passed to gsub

## See also

[`manipulateConfig`](manipulateConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# manipulateFile("example.txt",list(c("bla","blub"),c("a","b")))
```
