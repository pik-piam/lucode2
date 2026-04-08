# Replace in File

Function to set configuration parameters in configuration files (e.g.
default.cfg and magpie.gms). This replacement is useful, when using R to
manage different model runs at once. Please check your results after
replacement!

## Usage

``` r
manipulateConfig(configFile, ...)
```

## Arguments

- configFile:

  a character string containing the name of the configuration file, that
  should be manipulated. Supported file formats are at the moment "gms",
  "inc", "cfg" (R-syntax), "php", "opt" and "cmd". Other formats are
  currently not supported

- ...:

  Variables, that should be set to new values, e.g. title="test" for
  default.cfg or s_max_timesteps=10 for magpie.gms

## See also

[`manipulateFile`](manipulateFile.md)

## Author

Jan Philipp Dietrich, Markus Bonsch, David Klein

## Examples

``` r
if (FALSE) { # \dontrun{
manipulateConfig("config/default.cfg", input = "test_new_yields", title = "yihaa", revision = 4.2)
manipulateConfig("magpie.gms", s_max_timesteps = 4, s_use_gdx = -1)
} # }
```
