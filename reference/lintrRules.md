# lintrRules

This function defines the rules to be used by the linter called lintr.
[`check`](check.md) creates ".lintr" config files that use this
function.

## Usage

``` r
lintrRules(allowUndesirable = FALSE, modification = list())
```

## Arguments

- allowUndesirable:

  If true it is okay to use undesirable operators (such as "\<\<-") and
  undesirable (but not deprecated) functions (such as "setwd").

- modification:

  A named list mapping linter names to NULL (removes that linter) or a
  corresponding linter function. Will be applied to the result with
  utils::modifyList.

## Value

A named list mapping linter names to linter functions.

## Details

To change which linters are applied for a package edit the .lintr file
to use the modification argument, see example.

## See also

[`check`](check.md), [`lint`](lint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lintr::lint_dir(linters = lucode2::lintrRules())

# return usual linters with a different object_name_linter and without the todo_comment_linter
snakeCaseLinter <- lintr::object_name_linter(styles = "snake_case")
lucode2::lintrRules(modification = list(object_name_linter = snakeCaseLinter,
                                        todo_comment_linter = NULL))
} # }
```
