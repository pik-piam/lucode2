## Coding Conventions
- General coding style is documented through linter rules (lucode2::lintrRules). Basic rules:
  - Camel case
  - Line length: 120 characters

- No non-standard evaluation
- :: is recommended over @importFrom, if the package is in depends also no ::
- Mapping files need to be put into inst/extdata in packages
- `magpiecell` cellular resolution is deprecated and should not be added anymore
- Use `terra` instead of `raster`
- Use `withr` calls instead of base tempdir and deferred function executions

- Aim to stay with magclass objects, only switch to data.frames or alike when external packages need it or operations can be expressed much more concisely
- Packages follow the basic structure from [R Packages](https://r-pkgs.org/structure.html)
