# How to Contribute to a mr* Package

Thanks for considering contributing to a mr* package!

For our coding style, see the CODE_STYLE.md file.

## Environment Setup
- By installing the full `lucode2` package, you get all needed tools:
```R
  pak::pak("lucode2", repos = c('https://pik-piam.r-universe.dev', 'https://cloud.r-project.org'))
  # OR
  install.packages("lucode2", repos = c('https://pik-piam.r-universe.dev', 'https://cloud.r-project.org'), dependencies = TRUE)
```

## Interactive Evaluation of Functions
To evaluate your changes interactively in an R session:
- Use `devtools::load_all()` from within the package folder to load the current version
- Use `madrat::setConfig(ignorecache = c("calcMyFunction", "readMyData"))` to force the re-execution of your function
- Execute your function through `calcOutput`/`readSource`/`retrieveData`

## Testing
- Tests are in tests/testthat
- Fixtures should go into tests/testthat/fixtures
- Running tests
  - All tests: `devtools::test()`
  - Single test file: `devtools::test_file("tests/testthat/test_myFunction.R")`

## Overall Development Process
- Before pushing your changes, you need to run `lucode2::buildLibrary()`, which will check the new version and take care of updating metadata
- Every published change needs a new version (we follow semantic versioning)
- For publishing your change, in general: open a PR, then merge it
- If your change is very small and the repository allows it: push directly

## Attribution
- Add yourself to authors list in DESCRIPTION <!-- what do you need to have done to warrant being added? -->
<!-- no bots -->
