# Code Manipulation and Analysis Tools

R package **lucode2**, version **0.20.3**

[![CRAN status](https://www.r-pkg.org/badges/version/lucode2)](https://cran.r-project.org/package=lucode2) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4389418.svg)](https://doi.org/10.5281/zenodo.4389418) [![R build status](https://github.com/pik-piam/lucode2/workflows/check/badge.svg)](https://github.com/pik-piam/lucode2/actions) [![codecov](https://codecov.io/gh/pik-piam/lucode2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/lucode2) [![r-universe](https://pik-piam.r-universe.dev/badges/lucode2)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

A collection of tools which allow to manipulate and analyze code.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lucode2")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **lucode2** in publications use:

Dietrich J, Klein D, Giannousakis A, Bonsch M, Bodirsky B, Baumstark L, Führlich P (2022). _lucode2: Code Manipulation and Analysis Tools_. doi: 10.5281/zenodo.4389418 (URL: https://doi.org/10.5281/zenodo.4389418), R package version 0.20.3, <URL: https://github.com/pik-piam/lucode2>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lucode2: Code Manipulation and Analysis Tools},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Markus Bonsch and Benjamin Leon Bodirsky and Lavinia Baumstark and Pascal Führlich},
  year = {2022},
  note = {R package version 0.20.3},
  doi = {10.5281/zenodo.4389418},
  url = {https://github.com/pik-piam/lucode2},
}
```
