# Code Manipulation and Analysis Tools

R package **lucode2**, version **0.5.0**

[![Travis build status](https://travis-ci.com/pik-piam/lucode2.svg?branch=master)](https://travis-ci.com/pik-piam/lucode2)  [![codecov](https://codecov.io/gh/pik-piam/lucode2/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/lucode2)

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

Dietrich J, Klein D, Giannousakis A, Bonsch M, Bodirsky B, Baumstark L (2020). _lucode2: Code Manipulation
and Analysis Tools_. R package version 0.5.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lucode2: Code Manipulation and Analysis Tools},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Markus Bonsch and Benjamin Leon Bodirsky and Lavinia Baumstark},
  year = {2020},
  note = {R package version 0.5.0},
}
```

