# Setup Info

Returns a list with information about the current session (session info,
used library paths and installed libraries)

## Usage

``` r
setup_info()
```

## Value

A list with information about the current session and the currently used
R setup.

## Author

Jan Philipp Dietrich

## Examples

``` r
setup_info()
#> $sysinfo
#>                                               sysname 
#>                                               "Linux" 
#>                                               release 
#>                                   "6.17.0-1010-azure" 
#>                                               version 
#> "#10~24.04.1-Ubuntu SMP Fri Mar  6 22:00:57 UTC 2026" 
#>                                              nodename 
#>                                       "runnervmeorf1" 
#>                                               machine 
#>                                              "x86_64" 
#>                                                 login 
#>                                             "unknown" 
#>                                                  user 
#>                                              "runner" 
#>                                        effective_user 
#>                                              "runner" 
#> 
#> $sessionInfo
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C          
#>  [3] LC_TIME=C.UTF-8        LC_COLLATE=C          
#>  [5] LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C             
#>  [9] LC_ADDRESS=C           LC_TELEPHONE=C        
#> [11] LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods  
#> [7] base     
#> 
#> other attached packages:
#> [1] lucode2_0.54.5
#> 
#> loaded via a namespace (and not attached):
#>  [1] generics_0.1.4      rappdirs_0.3.4      sass_0.4.10        
#>  [4] renv_1.2.2          xml2_1.5.2          digest_0.6.39      
#>  [7] magrittr_2.0.5      evaluate_1.0.5      pkgload_1.5.2      
#> [10] fastmap_1.2.0       jsonlite_2.0.0      processx_3.9.0     
#> [13] pkgbuild_1.4.8      sessioninfo_1.2.3   whisker_0.4.1      
#> [16] backports_1.5.1     ps_1.9.3            purrr_1.2.2        
#> [19] fansi_1.0.7         lintr_3.3.0-1       textshaping_1.0.5  
#> [22] httr2_1.2.2         jquerylib_0.1.4     cli_3.6.6          
#> [25] rlang_1.2.0         tidytemplate_1.0.0  ellipsis_0.3.3     
#> [28] withr_3.0.2         cachem_1.1.0        yaml_2.3.12        
#> [31] devtools_2.5.1      otel_0.2.0          tools_4.6.0        
#> [34] memoise_2.0.1       dplyr_1.2.1         curl_7.1.0         
#> [37] vctrs_0.7.3         R6_2.6.1            lifecycle_1.0.5    
#> [40] htmlwidgets_1.6.4   fs_2.1.0            usethis_3.2.1      
#> [43] ragg_1.5.2          fontawesome_0.5.3   pkgconfig_2.0.3    
#> [46] desc_1.4.3          callr_3.7.6         rex_1.2.2          
#> [49] pkgdown_2.2.0       pillar_1.11.1       bslib_0.10.0       
#> [52] glue_1.8.1          data.table_1.18.2.1 systemfonts_1.3.2  
#> [55] tidyselect_1.2.1    xfun_0.57           tibble_3.3.1       
#> [58] rstudioapi_0.18.0   knitr_1.51          htmltools_0.5.9    
#> [61] rmarkdown_2.31      compiler_4.6.0      downlit_0.4.5      
#> [64] askpass_1.2.1       openssl_2.4.0      
#> 
#> $libPaths
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.6.0/lib/R/site-library"
#> [3] "/opt/R/4.6.0/lib/R/library"     
#> 
#> $installedpackages
#>              Package        LibPath                          
#> R.cache      "R.cache"      "/home/runner/work/_temp/Library"
#> R.methodsS3  "R.methodsS3"  "/home/runner/work/_temp/Library"
#> R.oo         "R.oo"         "/home/runner/work/_temp/Library"
#> R.utils      "R.utils"      "/home/runner/work/_temp/Library"
#> R6           "R6"           "/home/runner/work/_temp/Library"
#> RColorBrewer "RColorBrewer" "/home/runner/work/_temp/Library"
#> Rcpp         "Rcpp"         "/home/runner/work/_temp/Library"
#> S7           "S7"           "/home/runner/work/_temp/Library"
#> abind        "abind"        "/home/runner/work/_temp/Library"
#> askpass      "askpass"      "/home/runner/work/_temp/Library"
#> backports    "backports"    "/home/runner/work/_temp/Library"
#> base64enc    "base64enc"    "/home/runner/work/_temp/Library"
#> brew         "brew"         "/home/runner/work/_temp/Library"
#> brio         "brio"         "/home/runner/work/_temp/Library"
#> bslib        "bslib"        "/home/runner/work/_temp/Library"
#> cachem       "cachem"       "/home/runner/work/_temp/Library"
#> callr        "callr"        "/home/runner/work/_temp/Library"
#> citation     "citation"     "/home/runner/work/_temp/Library"
#> cli          "cli"          "/home/runner/work/_temp/Library"
#> clipr        "clipr"        "/home/runner/work/_temp/Library"
#> commonmark   "commonmark"   "/home/runner/work/_temp/Library"
#> covr         "covr"         "/home/runner/work/_temp/Library"
#> crayon       "crayon"       "/home/runner/work/_temp/Library"
#> credentials  "credentials"  "/home/runner/work/_temp/Library"
#> curl         "curl"         "/home/runner/work/_temp/Library"
#> data.table   "data.table"   "/home/runner/work/_temp/Library"
#> desc         "desc"         "/home/runner/work/_temp/Library"
#> devtools     "devtools"     "/home/runner/work/_temp/Library"
#> diffobj      "diffobj"      "/home/runner/work/_temp/Library"
#> digest       "digest"       "/home/runner/work/_temp/Library"
#> downlit      "downlit"      "/home/runner/work/_temp/Library"
#> dplyr        "dplyr"        "/home/runner/work/_temp/Library"
#> ellipsis     "ellipsis"     "/home/runner/work/_temp/Library"
#> evaluate     "evaluate"     "/home/runner/work/_temp/Library"
#> fansi        "fansi"        "/home/runner/work/_temp/Library"
#> farver       "farver"       "/home/runner/work/_temp/Library"
#> fastmap      "fastmap"      "/home/runner/work/_temp/Library"
#> fontawesome  "fontawesome"  "/home/runner/work/_temp/Library"
#> fs           "fs"           "/home/runner/work/_temp/Library"
#> gdx          "gdx"          "/home/runner/work/_temp/Library"
#> gdxrrw       "gdxrrw"       "/home/runner/work/_temp/Library"
#> generics     "generics"     "/home/runner/work/_temp/Library"
#> gert         "gert"         "/home/runner/work/_temp/Library"
#> ggplot2      "ggplot2"      "/home/runner/work/_temp/Library"
#> gh           "gh"           "/home/runner/work/_temp/Library"
#> gitcreds     "gitcreds"     "/home/runner/work/_temp/Library"
#> glue         "glue"         "/home/runner/work/_temp/Library"
#> gtable       "gtable"       "/home/runner/work/_temp/Library"
#> highr        "highr"        "/home/runner/work/_temp/Library"
#> htmltools    "htmltools"    "/home/runner/work/_temp/Library"
#> htmlwidgets  "htmlwidgets"  "/home/runner/work/_temp/Library"
#> httpuv       "httpuv"       "/home/runner/work/_temp/Library"
#> httr         "httr"         "/home/runner/work/_temp/Library"
#> httr2        "httr2"        "/home/runner/work/_temp/Library"
#> ini          "ini"          "/home/runner/work/_temp/Library"
#> isoband      "isoband"      "/home/runner/work/_temp/Library"
#> jquerylib    "jquerylib"    "/home/runner/work/_temp/Library"
#> jsonlite     "jsonlite"     "/home/runner/work/_temp/Library"
#> knitr        "knitr"        "/home/runner/work/_temp/Library"
#> labeling     "labeling"     "/home/runner/work/_temp/Library"
#> later        "later"        "/home/runner/work/_temp/Library"
#> lifecycle    "lifecycle"    "/home/runner/work/_temp/Library"
#> lintr        "lintr"        "/home/runner/work/_temp/Library"
#> lucode2      "lucode2"      "/home/runner/work/_temp/Library"
#> lusweave     "lusweave"     "/home/runner/work/_temp/Library"
#> magclass     "magclass"     "/home/runner/work/_temp/Library"
#> magrittr     "magrittr"     "/home/runner/work/_temp/Library"
#> memoise      "memoise"      "/home/runner/work/_temp/Library"
#> mime         "mime"         "/home/runner/work/_temp/Library"
#> miniUI       "miniUI"       "/home/runner/work/_temp/Library"
#> openssl      "openssl"      "/home/runner/work/_temp/Library"
#> otel         "otel"         "/home/runner/work/_temp/Library"
#> pak          "pak"          "/home/runner/work/_temp/Library"
#> pillar       "pillar"       "/home/runner/work/_temp/Library"
#> pkgbuild     "pkgbuild"     "/home/runner/work/_temp/Library"
#> pkgconfig    "pkgconfig"    "/home/runner/work/_temp/Library"
#> pkgdown      "pkgdown"      "/home/runner/work/_temp/Library"
#> pkgload      "pkgload"      "/home/runner/work/_temp/Library"
#> plyr         "plyr"         "/home/runner/work/_temp/Library"
#> poorman      "poorman"      "/home/runner/work/_temp/Library"
#> praise       "praise"       "/home/runner/work/_temp/Library"
#> prettyunits  "prettyunits"  "/home/runner/work/_temp/Library"
#> processx     "processx"     "/home/runner/work/_temp/Library"
#> profvis      "profvis"      "/home/runner/work/_temp/Library"
#> promises     "promises"     "/home/runner/work/_temp/Library"
#> ps           "ps"           "/home/runner/work/_temp/Library"
#> purrr        "purrr"        "/home/runner/work/_temp/Library"
#> ragg         "ragg"         "/home/runner/work/_temp/Library"
#> rappdirs     "rappdirs"     "/home/runner/work/_temp/Library"
#> rcmdcheck    "rcmdcheck"    "/home/runner/work/_temp/Library"
#> renv         "renv"         "/home/runner/work/_temp/Library"
#> reshape2     "reshape2"     "/home/runner/work/_temp/Library"
#> rex          "rex"          "/home/runner/work/_temp/Library"
#> rlang        "rlang"        "/home/runner/work/_temp/Library"
#> rmarkdown    "rmarkdown"    "/home/runner/work/_temp/Library"
#> roxygen2     "roxygen2"     "/home/runner/work/_temp/Library"
#> rprojroot    "rprojroot"    "/home/runner/work/_temp/Library"
#> rstudioapi   "rstudioapi"   "/home/runner/work/_temp/Library"
#> rversions    "rversions"    "/home/runner/work/_temp/Library"
#> sass         "sass"         "/home/runner/work/_temp/Library"
#> scales       "scales"       "/home/runner/work/_temp/Library"
#> sessioninfo  "sessioninfo"  "/home/runner/work/_temp/Library"
#> shiny        "shiny"        "/home/runner/work/_temp/Library"
#> sourcetools  "sourcetools"  "/home/runner/work/_temp/Library"
#> stringi      "stringi"      "/home/runner/work/_temp/Library"
#> stringr      "stringr"      "/home/runner/work/_temp/Library"
#> styler       "styler"       "/home/runner/work/_temp/Library"
#> sys          "sys"          "/home/runner/work/_temp/Library"
#> systemfonts  "systemfonts"  "/home/runner/work/_temp/Library"
#> testthat     "testthat"     "/home/runner/work/_temp/Library"
#> textshaping  "textshaping"  "/home/runner/work/_temp/Library"
#> tibble       "tibble"       "/home/runner/work/_temp/Library"
#> tidyselect   "tidyselect"   "/home/runner/work/_temp/Library"
#> tidytemplate "tidytemplate" "/home/runner/work/_temp/Library"
#> tinytex      "tinytex"      "/home/runner/work/_temp/Library"
#> urlchecker   "urlchecker"   "/home/runner/work/_temp/Library"
#> usethis      "usethis"      "/home/runner/work/_temp/Library"
#> utf8         "utf8"         "/home/runner/work/_temp/Library"
#> vctrs        "vctrs"        "/home/runner/work/_temp/Library"
#> viridisLite  "viridisLite"  "/home/runner/work/_temp/Library"
#> waldo        "waldo"        "/home/runner/work/_temp/Library"
#> whisker      "whisker"      "/home/runner/work/_temp/Library"
#> withr        "withr"        "/home/runner/work/_temp/Library"
#> xfun         "xfun"         "/home/runner/work/_temp/Library"
#> xml2         "xml2"         "/home/runner/work/_temp/Library"
#> xmlparsedata "xmlparsedata" "/home/runner/work/_temp/Library"
#> xopen        "xopen"        "/home/runner/work/_temp/Library"
#> xtable       "xtable"       "/home/runner/work/_temp/Library"
#> yaml         "yaml"         "/home/runner/work/_temp/Library"
#> zip          "zip"          "/home/runner/work/_temp/Library"
#> pak          "pak"          "/opt/R/4.6.0/lib/R/site-library"
#> KernSmooth   "KernSmooth"   "/opt/R/4.6.0/lib/R/library"     
#> MASS         "MASS"         "/opt/R/4.6.0/lib/R/library"     
#> Matrix       "Matrix"       "/opt/R/4.6.0/lib/R/library"     
#> base         "base"         "/opt/R/4.6.0/lib/R/library"     
#> boot         "boot"         "/opt/R/4.6.0/lib/R/library"     
#> class        "class"        "/opt/R/4.6.0/lib/R/library"     
#> cluster      "cluster"      "/opt/R/4.6.0/lib/R/library"     
#> codetools    "codetools"    "/opt/R/4.6.0/lib/R/library"     
#> compiler     "compiler"     "/opt/R/4.6.0/lib/R/library"     
#> datasets     "datasets"     "/opt/R/4.6.0/lib/R/library"     
#> foreign      "foreign"      "/opt/R/4.6.0/lib/R/library"     
#> grDevices    "grDevices"    "/opt/R/4.6.0/lib/R/library"     
#> graphics     "graphics"     "/opt/R/4.6.0/lib/R/library"     
#> grid         "grid"         "/opt/R/4.6.0/lib/R/library"     
#> lattice      "lattice"      "/opt/R/4.6.0/lib/R/library"     
#> methods      "methods"      "/opt/R/4.6.0/lib/R/library"     
#> mgcv         "mgcv"         "/opt/R/4.6.0/lib/R/library"     
#> nlme         "nlme"         "/opt/R/4.6.0/lib/R/library"     
#> nnet         "nnet"         "/opt/R/4.6.0/lib/R/library"     
#> parallel     "parallel"     "/opt/R/4.6.0/lib/R/library"     
#> rpart        "rpart"        "/opt/R/4.6.0/lib/R/library"     
#> spatial      "spatial"      "/opt/R/4.6.0/lib/R/library"     
#> splines      "splines"      "/opt/R/4.6.0/lib/R/library"     
#> stats        "stats"        "/opt/R/4.6.0/lib/R/library"     
#> stats4       "stats4"       "/opt/R/4.6.0/lib/R/library"     
#> survival     "survival"     "/opt/R/4.6.0/lib/R/library"     
#> tcltk        "tcltk"        "/opt/R/4.6.0/lib/R/library"     
#> tools        "tools"        "/opt/R/4.6.0/lib/R/library"     
#> utils        "utils"        "/opt/R/4.6.0/lib/R/library"     
#>              Version     Priority     
#> R.cache      "0.17.0"    NA           
#> R.methodsS3  "1.8.2"     NA           
#> R.oo         "1.27.1"    NA           
#> R.utils      "2.13.0"    NA           
#> R6           "2.6.1"     NA           
#> RColorBrewer "1.1-3"     NA           
#> Rcpp         "1.1.1-1.1" NA           
#> S7           "0.2.2"     NA           
#> abind        "1.4-8"     NA           
#> askpass      "1.2.1"     NA           
#> backports    "1.5.1"     NA           
#> base64enc    "0.1-6"     NA           
#> brew         "1.0-10"    NA           
#> brio         "1.1.5"     NA           
#> bslib        "0.10.0"    NA           
#> cachem       "1.1.0"     NA           
#> callr        "3.7.6"     NA           
#> citation     "0.12.2"    NA           
#> cli          "3.6.6"     NA           
#> clipr        "0.8.0"     NA           
#> commonmark   "2.0.0"     NA           
#> covr         "3.6.5"     NA           
#> crayon       "1.5.3"     NA           
#> credentials  "2.0.3"     NA           
#> curl         "7.1.0"     NA           
#> data.table   "1.18.2.1"  NA           
#> desc         "1.4.3"     NA           
#> devtools     "2.5.1"     NA           
#> diffobj      "0.3.6"     NA           
#> digest       "0.6.39"    NA           
#> downlit      "0.4.5"     NA           
#> dplyr        "1.2.1"     NA           
#> ellipsis     "0.3.3"     NA           
#> evaluate     "1.0.5"     NA           
#> fansi        "1.0.7"     NA           
#> farver       "2.1.2"     NA           
#> fastmap      "1.2.0"     NA           
#> fontawesome  "0.5.3"     NA           
#> fs           "2.1.0"     NA           
#> gdx          "1.53.1"    NA           
#> gdxrrw       "1.0.10"    NA           
#> generics     "0.1.4"     NA           
#> gert         "2.3.1"     NA           
#> ggplot2      "4.0.3"     NA           
#> gh           "1.5.0"     NA           
#> gitcreds     "0.1.2"     NA           
#> glue         "1.8.1"     NA           
#> gtable       "0.3.6"     NA           
#> highr        "0.12"      NA           
#> htmltools    "0.5.9"     NA           
#> htmlwidgets  "1.6.4"     NA           
#> httpuv       "1.6.17"    NA           
#> httr         "1.4.8"     NA           
#> httr2        "1.2.2"     NA           
#> ini          "0.3.1"     NA           
#> isoband      "0.3.0"     NA           
#> jquerylib    "0.1.4"     NA           
#> jsonlite     "2.0.0"     NA           
#> knitr        "1.51"      NA           
#> labeling     "0.4.3"     NA           
#> later        "1.4.8"     NA           
#> lifecycle    "1.0.5"     NA           
#> lintr        "3.3.0-1"   NA           
#> lucode2      "0.54.5"    NA           
#> lusweave     "1.46.6"    NA           
#> magclass     "7.4.3"     NA           
#> magrittr     "2.0.5"     NA           
#> memoise      "2.0.1"     NA           
#> mime         "0.13"      NA           
#> miniUI       "0.1.2"     NA           
#> openssl      "2.4.0"     NA           
#> otel         "0.2.0"     NA           
#> pak          "0.9.5"     NA           
#> pillar       "1.11.1"    NA           
#> pkgbuild     "1.4.8"     NA           
#> pkgconfig    "2.0.3"     NA           
#> pkgdown      "2.2.0"     NA           
#> pkgload      "1.5.2"     NA           
#> plyr         "1.8.9"     NA           
#> poorman      "0.2.7"     NA           
#> praise       "1.0.0"     NA           
#> prettyunits  "1.2.0"     NA           
#> processx     "3.9.0"     NA           
#> profvis      "0.4.0"     NA           
#> promises     "1.5.0"     NA           
#> ps           "1.9.3"     NA           
#> purrr        "1.2.2"     NA           
#> ragg         "1.5.2"     NA           
#> rappdirs     "0.3.4"     NA           
#> rcmdcheck    "1.4.0"     NA           
#> renv         "1.2.2"     NA           
#> reshape2     "1.4.5"     NA           
#> rex          "1.2.2"     NA           
#> rlang        "1.2.0"     NA           
#> rmarkdown    "2.31"      NA           
#> roxygen2     "7.3.3"     NA           
#> rprojroot    "2.1.1"     NA           
#> rstudioapi   "0.18.0"    NA           
#> rversions    "3.0.0"     NA           
#> sass         "0.4.10"    NA           
#> scales       "1.4.0"     NA           
#> sessioninfo  "1.2.3"     NA           
#> shiny        "1.13.0"    NA           
#> sourcetools  "0.1.7-2"   NA           
#> stringi      "1.8.7"     NA           
#> stringr      "1.6.0"     NA           
#> styler       "1.11.0"    NA           
#> sys          "3.4.3"     NA           
#> systemfonts  "1.3.2"     NA           
#> testthat     "3.3.2"     NA           
#> textshaping  "1.0.5"     NA           
#> tibble       "3.3.1"     NA           
#> tidyselect   "1.2.1"     NA           
#> tidytemplate "1.0.0"     NA           
#> tinytex      "0.59"      NA           
#> urlchecker   "1.0.1"     NA           
#> usethis      "3.2.1"     NA           
#> utf8         "1.2.6"     NA           
#> vctrs        "0.7.3"     NA           
#> viridisLite  "0.4.3"     NA           
#> waldo        "0.6.2"     NA           
#> whisker      "0.4.1"     NA           
#> withr        "3.0.2"     NA           
#> xfun         "0.57"      NA           
#> xml2         "1.5.2"     NA           
#> xmlparsedata "1.0.5"     NA           
#> xopen        "1.0.1"     NA           
#> xtable       "1.8-8"     NA           
#> yaml         "2.3.12"    NA           
#> zip          "2.3.3"     NA           
#> pak          "0.9.5"     NA           
#> KernSmooth   "2.23-26"   "recommended"
#> MASS         "7.3-65"    "recommended"
#> Matrix       "1.7-5"     "recommended"
#> base         "4.6.0"     "base"       
#> boot         "1.3-32"    "recommended"
#> class        "7.3-23"    "recommended"
#> cluster      "2.1.8.2"   "recommended"
#> codetools    "0.2-20"    "recommended"
#> compiler     "4.6.0"     "base"       
#> datasets     "4.6.0"     "base"       
#> foreign      "0.8-91"    "recommended"
#> grDevices    "4.6.0"     "base"       
#> graphics     "4.6.0"     "base"       
#> grid         "4.6.0"     "base"       
#> lattice      "0.22-9"    "recommended"
#> methods      "4.6.0"     "base"       
#> mgcv         "1.9-4"     "recommended"
#> nlme         "3.1-169"   "recommended"
#> nnet         "7.3-20"    "recommended"
#> parallel     "4.6.0"     "base"       
#> rpart        "4.1.27"    "recommended"
#> spatial      "7.3-18"    "recommended"
#> splines      "4.6.0"     "base"       
#> stats        "4.6.0"     "base"       
#> stats4       "4.6.0"     "base"       
#> survival     "3.8-6"     "recommended"
#> tcltk        "4.6.0"     "base"       
#> tools        "4.6.0"     "base"       
#> utils        "4.6.0"     "base"       
#>              Depends                                          
#> R.cache      "R (>= 2.14.0)"                                  
#> R.methodsS3  "R (>= 2.13.0)"                                  
#> R.oo         "R (>= 2.13.0), R.methodsS3 (>= 1.8.2)"          
#> R.utils      "R (>= 2.14.0), R.oo"                            
#> R6           "R (>= 3.6)"                                     
#> RColorBrewer "R (>= 2.0.0)"                                   
#> Rcpp         "R (>= 3.5.0)"                                   
#> S7           "R (>= 3.5.0)"                                   
#> abind        "R (>= 1.5.0)"                                   
#> askpass      NA                                               
#> backports    "R (>= 3.0.0)"                                   
#> base64enc    "R (>= 2.9.0)"                                   
#> brew         NA                                               
#> brio         "R (>= 3.6)"                                     
#> bslib        "R (>= 2.10)"                                    
#> cachem       NA                                               
#> callr        "R (>= 3.4)"                                     
#> citation     NA                                               
#> cli          "R (>= 3.4)"                                     
#> clipr        NA                                               
#> commonmark   NA                                               
#> covr         "R (>= 3.1.0), methods"                          
#> crayon       NA                                               
#> credentials  NA                                               
#> curl         "R (>= 3.0.0)"                                   
#> data.table   "R (>= 3.4.0)"                                   
#> desc         "R (>= 3.4)"                                     
#> devtools     "R (>= 4.1), usethis (>= 3.2.1)"                 
#> diffobj      "R (>= 3.1.0)"                                   
#> digest       "R (>= 3.3.0)"                                   
#> downlit      "R (>= 4.0.0)"                                   
#> dplyr        "R (>= 4.1.0)"                                   
#> ellipsis     "R (>= 3.2)"                                     
#> evaluate     "R (>= 3.6.0)"                                   
#> fansi        "R (>= 3.1.0)"                                   
#> farver       NA                                               
#> fastmap      NA                                               
#> fontawesome  "R (>= 3.3.0)"                                   
#> fs           "R (>= 4.1)"                                     
#> gdx          "gdxrrw (>= 1.0.2), magclass (>= 2.43)"          
#> gdxrrw       "R (>= 3.0)"                                     
#> generics     "R (>= 3.6)"                                     
#> gert         NA                                               
#> ggplot2      "R (>= 4.1)"                                     
#> gh           "R (>= 4.1)"                                     
#> gitcreds     "R (>= 3.4)"                                     
#> glue         "R (>= 4.1)"                                     
#> gtable       "R (>= 4.0)"                                     
#> highr        "R (>= 3.3.0)"                                   
#> htmltools    "R (>= 2.14.1)"                                  
#> htmlwidgets  NA                                               
#> httpuv       "R (>= 2.15.1)"                                  
#> httr         "R (>= 3.6)"                                     
#> httr2        "R (>= 4.1)"                                     
#> ini          NA                                               
#> isoband      NA                                               
#> jquerylib    NA                                               
#> jsonlite     "methods"                                        
#> knitr        "R (>= 3.6.0)"                                   
#> labeling     NA                                               
#> later        "R (>= 3.5)"                                     
#> lifecycle    "R (>= 3.6)"                                     
#> lintr        "R (>= 4.0)"                                     
#> lucode2      NA                                               
#> lusweave     "methods, R (>= 2.10.0)"                         
#> magclass     "methods, R (>= 2.10.0)"                         
#> magrittr     "R (>= 3.4.0)"                                   
#> memoise      NA                                               
#> mime         NA                                               
#> miniUI       NA                                               
#> openssl      NA                                               
#> otel         "R (>= 3.6.0)"                                   
#> pak          "R (>= 3.5)"                                     
#> pillar       NA                                               
#> pkgbuild     "R (>= 3.5)"                                     
#> pkgconfig    NA                                               
#> pkgdown      "R (>= 4.1)"                                     
#> pkgload      "R (>= 3.4.0)"                                   
#> plyr         "R (>= 3.1.0)"                                   
#> poorman      "R (>= 3.3)"                                     
#> praise       NA                                               
#> prettyunits  "R(>= 2.10)"                                     
#> processx     "R (>= 3.4.0)"                                   
#> profvis      "R (>= 4.0)"                                     
#> promises     "R (>= 4.1.0)"                                   
#> ps           "R (>= 3.4)"                                     
#> purrr        "R (>= 4.1)"                                     
#> ragg         NA                                               
#> rappdirs     "R (>= 4.1)"                                     
#> rcmdcheck    NA                                               
#> renv         NA                                               
#> reshape2     "R (>= 3.1)"                                     
#> rex          NA                                               
#> rlang        "R (>= 4.0.0)"                                   
#> rmarkdown    "R (>= 3.0)"                                     
#> roxygen2     "R (>= 3.6)"                                     
#> rprojroot    "R (>= 3.0.0)"                                   
#> rstudioapi   NA                                               
#> rversions    NA                                               
#> sass         NA                                               
#> scales       "R (>= 4.1)"                                     
#> sessioninfo  "R (>= 3.4)"                                     
#> shiny        "methods, R (>= 3.1.2)"                          
#> sourcetools  "R (>= 3.0.2)"                                   
#> stringi      "R (>= 3.4)"                                     
#> stringr      "R (>= 4.1.0)"                                   
#> styler       "R (>= 4.0.0)"                                   
#> sys          NA                                               
#> systemfonts  "R (>= 3.2.0)"                                   
#> testthat     "R (>= 4.1.0)"                                   
#> textshaping  "R (>= 3.2.0)"                                   
#> tibble       "R (>= 3.4.0)"                                   
#> tidyselect   "R (>= 3.4)"                                     
#> tidytemplate NA                                               
#> tinytex      NA                                               
#> urlchecker   "R (>= 3.3)"                                     
#> usethis      "R (>= 4.1)"                                     
#> utf8         "R (>= 2.10)"                                    
#> vctrs        "R (>= 4.0.0)"                                   
#> viridisLite  "R (>= 2.10)"                                    
#> waldo        "R (>= 4.0)"                                     
#> whisker      NA                                               
#> withr        "R (>= 3.6.0)"                                   
#> xfun         "R (>= 3.2.0)"                                   
#> xml2         "R (>= 3.6.0)"                                   
#> xmlparsedata "R (>= 3.0.0)"                                   
#> xopen        "R (>= 3.1)"                                     
#> xtable       "R (>= 2.10.0)"                                  
#> yaml         NA                                               
#> zip          NA                                               
#> pak          "R (>= 3.5)"                                     
#> KernSmooth   "R (>= 2.5.0), stats"                            
#> MASS         "R (>= 4.4.0), grDevices, graphics, stats, utils"
#> Matrix       "R (>= 4.4), methods"                            
#> base         NA                                               
#> boot         "R (>= 3.0.0), graphics, stats"                  
#> class        "R (>= 3.0.0), stats, utils"                     
#> cluster      "R (>= 3.5.0)"                                   
#> codetools    "R (>= 2.1)"                                     
#> compiler     NA                                               
#> datasets     NA                                               
#> foreign      "R (>= 4.0.0)"                                   
#> grDevices    NA                                               
#> graphics     NA                                               
#> grid         NA                                               
#> lattice      "R (>= 4.0.0)"                                   
#> methods      NA                                               
#> mgcv         "R (>= 4.4.0), nlme (>= 3.1-64)"                 
#> nlme         "R (>= 3.6.0)"                                   
#> nnet         "R (>= 3.0.0), stats, utils"                     
#> parallel     NA                                               
#> rpart        "R (>= 2.15.0), graphics, stats, grDevices"      
#> spatial      "R (>= 3.0.0), graphics, stats, utils"           
#> splines      NA                                               
#> stats        NA                                               
#> stats4       NA                                               
#> survival     "R (>= 3.5.0)"                                   
#> tcltk        NA                                               
#> tools        NA                                               
#> utils        NA                                               
#>              Imports                                                                                                                                                                                                                                                                                                                                                                                                           
#> R.cache      "utils, R.methodsS3 (>= 1.8.1), R.oo (>= 1.24.0), R.utils (>=\n2.10.1), digest (>= 0.6.13)"                                                                                                                                                                                                                                                                                                                       
#> R.methodsS3  "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> R.oo         "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> R.utils      "methods, utils, tools, R.methodsS3"                                                                                                                                                                                                                                                                                                                                                                              
#> R6           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> RColorBrewer NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> Rcpp         "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> S7           "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> abind        "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> askpass      "sys (>= 2.1)"                                                                                                                                                                                                                                                                                                                                                                                                    
#> backports    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> base64enc    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> brew         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> brio         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bslib        "base64enc, cachem, fastmap (>= 1.1.1), grDevices, htmltools\n(>= 0.5.8), jquerylib (>= 0.1.3), jsonlite, lifecycle, memoise\n(>= 2.0.1), mime, rlang, sass (>= 0.4.9)"                                                                                                                                                                                                                                           
#> cachem       "rlang, fastmap (>= 1.2.0)"                                                                                                                                                                                                                                                                                                                                                                                       
#> callr        "processx (>= 3.6.1), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                  
#> citation     "desc, jsonlite, utils, withr, yaml"                                                                                                                                                                                                                                                                                                                                                                              
#> cli          "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> clipr        "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> commonmark   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> covr         "digest, stats, utils, jsonlite, rex, httr, cli, withr (>=\n1.0.2), yaml"                                                                                                                                                                                                                                                                                                                                         
#> crayon       "grDevices, methods, utils"                                                                                                                                                                                                                                                                                                                                                                                       
#> credentials  "openssl (>= 1.3), sys (>= 2.1), curl, jsonlite, askpass"                                                                                                                                                                                                                                                                                                                                                         
#> curl         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> data.table   "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> desc         "cli, R6, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> devtools     "cli (>= 3.6.6), desc (>= 1.4.3), ellipsis (>= 0.3.3), fs (>=\n2.0.1), lifecycle (>= 1.0.5), memoise (>= 2.0.1), miniUI (>=\n0.1.2), pak (>= 0.9.3), pkgbuild (>= 1.4.8), pkgdown (>=\n2.2.0), pkgload (>= 1.5.1), profvis (>= 0.4.0), rcmdcheck (>=\n1.4.0), rlang (>= 1.2.0), roxygen2 (>= 7.3.3), rversions (>=\n3.0.0), sessioninfo (>= 1.2.3), testthat (>= 3.3.2), urlchecker\n(>= 1.0.1), withr (>= 3.0.2)"
#> diffobj      "crayon (>= 1.3.2), tools, methods, utils, stats"                                                                                                                                                                                                                                                                                                                                                                 
#> digest       "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> downlit      "brio, desc, digest, evaluate, fansi, memoise, rlang, vctrs,\nwithr, yaml"                                                                                                                                                                                                                                                                                                                                        
#> dplyr        "cli (>= 3.6.2), generics, glue (>= 1.3.2), lifecycle (>=\n1.0.5), magrittr (>= 1.5), methods, pillar (>= 1.9.0), R6,\nrlang (>= 1.1.7), tibble (>= 3.2.0), tidyselect (>= 1.2.0),\nutils, vctrs (>= 0.7.1)"                                                                                                                                                                                                      
#> ellipsis     "rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                                                                
#> evaluate     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fansi        "grDevices, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> farver       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fastmap      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fontawesome  "rlang (>= 1.0.6), htmltools (>= 0.5.1.1)"                                                                                                                                                                                                                                                                                                                                                                        
#> fs           "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gdx          "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> gdxrrw       "reshape2"                                                                                                                                                                                                                                                                                                                                                                                                        
#> generics     "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gert         "askpass, credentials (>= 1.2.1), openssl (>= 2.0.3),\nrstudioapi (>= 0.11), sys, zip (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                 
#> ggplot2      "cli, grDevices, grid, gtable (>= 0.3.6), isoband, lifecycle (>\n1.0.1), rlang (>= 1.1.0), S7, scales (>= 1.4.0), stats, vctrs\n(>= 0.6.0), withr (>= 2.5.0)"                                                                                                                                                                                                                                                     
#> gh           "cli (>= 3.0.1), gitcreds, glue, httr2 (>= 1.0.6), ini,\njsonlite, lifecycle, rlang (>= 1.0.0)"                                                                                                                                                                                                                                                                                                                   
#> gitcreds     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> glue         "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gtable       "cli, glue, grid, lifecycle, rlang (>= 1.1.0), stats"                                                                                                                                                                                                                                                                                                                                                             
#> highr        "xfun (>= 0.18)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> htmltools    "base64enc, digest, fastmap (>= 1.1.0), grDevices, rlang (>=\n1.0.0), utils"                                                                                                                                                                                                                                                                                                                                      
#> htmlwidgets  "grDevices, htmltools (>= 0.5.7), jsonlite (>= 0.9.16), knitr\n(>= 1.8), rmarkdown, yaml"                                                                                                                                                                                                                                                                                                                         
#> httpuv       "later (>= 0.8.0), promises, R6, Rcpp (>= 1.0.7), utils"                                                                                                                                                                                                                                                                                                                                                          
#> httr         "curl (>= 5.1.0), jsonlite, mime, openssl (>= 0.8), R6"                                                                                                                                                                                                                                                                                                                                                           
#> httr2        "cli (>= 3.0.0), curl (>= 6.4.0), glue, lifecycle, magrittr,\nopenssl, R6, rappdirs, rlang (>= 1.1.0), vctrs (>= 0.6.3),\nwithr"                                                                                                                                                                                                                                                                                  
#> ini          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> isoband      "cli, grid, rlang, utils"                                                                                                                                                                                                                                                                                                                                                                                         
#> jquerylib    "htmltools"                                                                                                                                                                                                                                                                                                                                                                                                       
#> jsonlite     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> knitr        "evaluate (>= 0.15), highr (>= 0.11), methods, tools, xfun (>=\n0.52), yaml (>= 2.1.19)"                                                                                                                                                                                                                                                                                                                          
#> labeling     "stats, graphics"                                                                                                                                                                                                                                                                                                                                                                                                 
#> later        "Rcpp (>= 1.0.10), rlang"                                                                                                                                                                                                                                                                                                                                                                                         
#> lifecycle    "cli (>= 3.4.0), rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                
#> lintr        "backports (>= 1.5.0), cli (>= 3.4.0), codetools, digest, glue,\nknitr, rex, stats, utils, xfun, xml2 (>= 1.0.0), xmlparsedata\n(>= 1.0.5)"                                                                                                                                                                                                                                                                       
#> lucode2      "callr, citation (>= 0.11.3), data.table, desc, devtools,\ndplyr, lintr (>= 3.1.0), pak, rlang, tools, usethis (>= 2.1.0),\nwithr, yaml"                                                                                                                                                                                                                                                                          
#> lusweave     "knitr (>= 1.38), withr, xtable"                                                                                                                                                                                                                                                                                                                                                                                  
#> magclass     "abind, data.table, rlang, stats"                                                                                                                                                                                                                                                                                                                                                                                 
#> magrittr     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> memoise      "rlang (>= 0.4.10), cachem"                                                                                                                                                                                                                                                                                                                                                                                       
#> mime         "tools"                                                                                                                                                                                                                                                                                                                                                                                                           
#> miniUI       "shiny (>= 0.13), htmltools (>= 0.3), utils"                                                                                                                                                                                                                                                                                                                                                                      
#> openssl      "askpass"                                                                                                                                                                                                                                                                                                                                                                                                         
#> otel         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> pak          "tools, utils"                                                                                                                                                                                                                                                                                                                                                                                                    
#> pillar       "cli (>= 2.3.0), glue, lifecycle, rlang (>= 1.0.2), utf8 (>=\n1.1.0), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                                                    
#> pkgbuild     "callr (>= 3.2.0), cli (>= 3.4.0), desc, processx, R6"                                                                                                                                                                                                                                                                                                                                                            
#> pkgconfig    "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> pkgdown      "bslib (>= 0.5.1), callr (>= 3.7.3), cli (>= 3.6.1), desc (>=\n1.4.0), downlit (>= 0.4.4), fontawesome, fs (>= 1.4.0), httr2\n(>= 1.0.2), jsonlite, lifecycle, openssl, purrr (>= 1.0.0),\nragg (>= 1.4.0), rlang (>= 1.1.4), rmarkdown (>= 2.27), tibble,\nwhisker, withr (>= 2.4.3), xml2 (>= 1.3.1), yaml (>= 2.3.9)"                                                                                          
#> pkgload      "cli (>= 3.3.0), desc, fs, glue, lifecycle, methods, pkgbuild,\nprocessx, rlang (>= 1.1.1), rprojroot, utils"                                                                                                                                                                                                                                                                                                     
#> plyr         "Rcpp (>= 0.11.0)"                                                                                                                                                                                                                                                                                                                                                                                                
#> poorman      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> praise       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> prettyunits  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> processx     "ps (>= 1.9.3), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                        
#> profvis      "htmlwidgets (>= 0.3.2), rlang (>= 1.1.0), vctrs"                                                                                                                                                                                                                                                                                                                                                                 
#> promises     "fastmap (>= 1.1.0), later, lifecycle, magrittr (>= 1.5), otel\n(>= 0.2.0), R6, rlang"                                                                                                                                                                                                                                                                                                                            
#> ps           "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> purrr        "cli (>= 3.6.1), lifecycle (>= 1.0.3), magrittr (>= 1.5.0),\nrlang (>= 1.1.1), vctrs (>= 0.6.3)"                                                                                                                                                                                                                                                                                                                  
#> ragg         "systemfonts (>= 1.0.3), textshaping (>= 0.3.0)"                                                                                                                                                                                                                                                                                                                                                                  
#> rappdirs     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rcmdcheck    "callr (>= 3.1.1.9000), cli (>= 3.0.0), curl, desc (>= 1.2.0),\ndigest, pkgbuild, prettyunits, R6, rprojroot, sessioninfo (>=\n1.1.1), utils, withr, xopen"                                                                                                                                                                                                                                                       
#> renv         "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> reshape2     "plyr (>= 1.8.1), Rcpp, stringr"                                                                                                                                                                                                                                                                                                                                                                                  
#> rex          "withr"                                                                                                                                                                                                                                                                                                                                                                                                           
#> rlang        "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> rmarkdown    "bslib (>= 0.2.5.1), evaluate (>= 0.13), fontawesome (>=\n0.5.0), htmltools (>= 0.5.1), jquerylib, jsonlite, knitr (>=\n1.43), methods, tinytex (>= 0.31), tools, utils, xfun (>=\n0.36), yaml (>= 2.1.19)"                                                                                                                                                                                                       
#> roxygen2     "brew, cli (>= 3.3.0), commonmark, desc (>= 1.2.0), knitr,\nmethods, pkgload (>= 1.0.2), purrr (>= 1.0.0), R6 (>= 2.1.2),\nrlang (>= 1.0.6), stringi, stringr (>= 1.0.0), utils, withr,\nxml2"                                                                                                                                                                                                                    
#> rprojroot    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rstudioapi   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rversions    "curl"                                                                                                                                                                                                                                                                                                                                                                                                            
#> sass         "fs (>= 1.2.4), rlang (>= 0.4.10), htmltools (>= 0.5.1), R6,\nrappdirs"                                                                                                                                                                                                                                                                                                                                           
#> scales       "cli, farver (>= 2.0.3), glue, labeling, lifecycle, R6,\nRColorBrewer, rlang (>= 1.1.0), viridisLite"                                                                                                                                                                                                                                                                                                             
#> sessioninfo  "cli (>= 3.1.0), tools, utils"                                                                                                                                                                                                                                                                                                                                                                                    
#> shiny        "bslib (>= 0.6.0), cachem (>= 1.1.0), cli, commonmark (>=\n2.0.0), fastmap (>= 1.1.1), fontawesome (>= 0.4.0), glue (>=\n1.3.2), grDevices, htmltools (>= 0.5.4), httpuv (>= 1.5.2),\njsonlite (>= 0.9.16), later (>= 1.0.0), lifecycle (>= 0.2.0),\nmime (>= 0.3), otel, promises (>= 1.5.0), R6 (>= 2.0), rlang\n(>= 0.4.10), sourcetools, tools, utils, withr, xtable"                                         
#> sourcetools  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> stringi      "tools, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                             
#> stringr      "cli, glue (>= 1.6.1), lifecycle (>= 1.0.3), magrittr, rlang\n(>= 1.0.0), stringi (>= 1.5.3), vctrs (>= 0.4.0)"                                                                                                                                                                                                                                                                                                   
#> styler       "cli (>= 3.1.1), magrittr (>= 2.0.0), purrr (>= 1.0.2), R.cache\n(>= 0.15.0), rlang (>= 1.0.0), rprojroot (>= 1.1), tools, vctrs\n(>= 0.4.1), withr (>= 2.3.0),"                                                                                                                                                                                                                                                  
#> sys          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> systemfonts  "base64enc, grid, jsonlite, lifecycle, tools, utils"                                                                                                                                                                                                                                                                                                                                                              
#> testthat     "brio (>= 1.1.5), callr (>= 3.7.6), cli (>= 3.6.5), desc (>=\n1.4.3), evaluate (>= 1.0.4), jsonlite (>= 2.0.0), lifecycle (>=\n1.0.4), magrittr (>= 2.0.3), methods, pkgload (>= 1.4.0),\npraise (>= 1.0.0), processx (>= 3.8.6), ps (>= 1.9.1), R6 (>=\n2.6.1), rlang (>= 1.1.6), utils, waldo (>= 0.6.2), withr (>=\n3.0.2)"                                                                                    
#> textshaping  "lifecycle, stats, stringi, systemfonts (>= 1.3.0), utils"                                                                                                                                                                                                                                                                                                                                                        
#> tibble       "cli, lifecycle (>= 1.0.0), magrittr, methods, pillar (>=\n1.8.1), pkgconfig, rlang (>= 1.0.2), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                          
#> tidyselect   "cli (>= 3.3.0), glue (>= 1.3.0), lifecycle (>= 1.0.3), rlang\n(>= 1.0.4), vctrs (>= 0.5.2), withr"                                                                                                                                                                                                                                                                                                               
#> tidytemplate NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> tinytex      "xfun (>= 0.48)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> urlchecker   "cli, curl, tools, xml2"                                                                                                                                                                                                                                                                                                                                                                                          
#> usethis      "cli (>= 3.0.1), clipr (>= 0.3.0), crayon, curl (>= 2.7), desc\n(>= 1.4.2), fs (>= 1.3.0), gert (>= 1.4.1), gh (>= 1.2.1), glue\n(>= 1.3.0), jsonlite, lifecycle (>= 1.0.0), purrr, rappdirs,\nrlang (>= 1.1.0), rprojroot (>= 2.1.1), rstudioapi, stats,\ntools, utils, whisker, withr (>= 2.3.0), yaml"                                                                                                         
#> utf8         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> vctrs        "cli (>= 3.4.0), glue, lifecycle (>= 1.0.3), rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                    
#> viridisLite  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> waldo        "cli, diffobj (>= 0.3.4), glue, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                        
#> whisker      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> withr        "graphics, grDevices"                                                                                                                                                                                                                                                                                                                                                                                             
#> xfun         "grDevices, stats, tools"                                                                                                                                                                                                                                                                                                                                                                                         
#> xml2         "cli, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                  
#> xmlparsedata NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> xopen        "processx"                                                                                                                                                                                                                                                                                                                                                                                                        
#> xtable       "stats, utils, methods"                                                                                                                                                                                                                                                                                                                                                                                           
#> yaml         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> zip          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> pak          "tools, utils"                                                                                                                                                                                                                                                                                                                                                                                                    
#> KernSmooth   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> MASS         "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> Matrix       "grDevices, graphics, grid, lattice, stats, utils"                                                                                                                                                                                                                                                                                                                                                                
#> base         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> boot         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> class        "MASS"                                                                                                                                                                                                                                                                                                                                                                                                            
#> cluster      "graphics, grDevices, stats, utils"                                                                                                                                                                                                                                                                                                                                                                               
#> codetools    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> compiler     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> datasets     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> foreign      "methods, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                           
#> grDevices    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> graphics     "grDevices"                                                                                                                                                                                                                                                                                                                                                                                                       
#> grid         "grDevices, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> lattice      "grid, grDevices, graphics, stats, utils"                                                                                                                                                                                                                                                                                                                                                                         
#> methods      "utils, stats"                                                                                                                                                                                                                                                                                                                                                                                                    
#> mgcv         "methods, stats, graphics, Matrix, splines, utils"                                                                                                                                                                                                                                                                                                                                                                
#> nlme         "graphics, stats, utils, lattice"                                                                                                                                                                                                                                                                                                                                                                                 
#> nnet         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> parallel     "tools, compiler"                                                                                                                                                                                                                                                                                                                                                                                                 
#> rpart        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> spatial      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> splines      "graphics, stats"                                                                                                                                                                                                                                                                                                                                                                                                 
#> stats        "utils, grDevices, graphics"                                                                                                                                                                                                                                                                                                                                                                                      
#> stats4       "graphics, methods, stats"                                                                                                                                                                                                                                                                                                                                                                                        
#> survival     "graphics, Matrix, methods, splines, stats, utils"                                                                                                                                                                                                                                                                                                                                                                
#> tcltk        "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> tools        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> utils        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#>              LinkingTo                                 
#> R.cache      NA                                        
#> R.methodsS3  NA                                        
#> R.oo         NA                                        
#> R.utils      NA                                        
#> R6           NA                                        
#> RColorBrewer NA                                        
#> Rcpp         NA                                        
#> S7           NA                                        
#> abind        NA                                        
#> askpass      NA                                        
#> backports    NA                                        
#> base64enc    NA                                        
#> brew         NA                                        
#> brio         NA                                        
#> bslib        NA                                        
#> cachem       NA                                        
#> callr        NA                                        
#> citation     NA                                        
#> cli          NA                                        
#> clipr        NA                                        
#> commonmark   NA                                        
#> covr         NA                                        
#> crayon       NA                                        
#> credentials  NA                                        
#> curl         NA                                        
#> data.table   NA                                        
#> desc         NA                                        
#> devtools     NA                                        
#> diffobj      NA                                        
#> digest       NA                                        
#> downlit      NA                                        
#> dplyr        NA                                        
#> ellipsis     NA                                        
#> evaluate     NA                                        
#> fansi        NA                                        
#> farver       NA                                        
#> fastmap      NA                                        
#> fontawesome  NA                                        
#> fs           NA                                        
#> gdx          NA                                        
#> gdxrrw       NA                                        
#> generics     NA                                        
#> gert         NA                                        
#> ggplot2      NA                                        
#> gh           NA                                        
#> gitcreds     NA                                        
#> glue         NA                                        
#> gtable       NA                                        
#> highr        NA                                        
#> htmltools    NA                                        
#> htmlwidgets  NA                                        
#> httpuv       "later, Rcpp"                             
#> httr         NA                                        
#> httr2        NA                                        
#> ini          NA                                        
#> isoband      "cpp11"                                   
#> jquerylib    NA                                        
#> jsonlite     NA                                        
#> knitr        NA                                        
#> labeling     NA                                        
#> later        "Rcpp"                                    
#> lifecycle    NA                                        
#> lintr        NA                                        
#> lucode2      NA                                        
#> lusweave     NA                                        
#> magclass     NA                                        
#> magrittr     NA                                        
#> memoise      NA                                        
#> mime         NA                                        
#> miniUI       NA                                        
#> openssl      NA                                        
#> otel         NA                                        
#> pak          NA                                        
#> pillar       NA                                        
#> pkgbuild     NA                                        
#> pkgconfig    NA                                        
#> pkgdown      NA                                        
#> pkgload      NA                                        
#> plyr         "Rcpp"                                    
#> poorman      NA                                        
#> praise       NA                                        
#> prettyunits  NA                                        
#> processx     NA                                        
#> profvis      NA                                        
#> promises     NA                                        
#> ps           NA                                        
#> purrr        "cli"                                     
#> ragg         "systemfonts, textshaping"                
#> rappdirs     NA                                        
#> rcmdcheck    NA                                        
#> renv         NA                                        
#> reshape2     "Rcpp"                                    
#> rex          NA                                        
#> rlang        NA                                        
#> rmarkdown    NA                                        
#> roxygen2     "cpp11"                                   
#> rprojroot    NA                                        
#> rstudioapi   NA                                        
#> rversions    NA                                        
#> sass         NA                                        
#> scales       NA                                        
#> sessioninfo  NA                                        
#> shiny        NA                                        
#> sourcetools  NA                                        
#> stringi      NA                                        
#> stringr      NA                                        
#> styler       NA                                        
#> sys          NA                                        
#> systemfonts  "cpp11 (>= 0.2.1)"                        
#> testthat     NA                                        
#> textshaping  "cpp11 (>= 0.2.1), systemfonts (>= 1.0.0)"
#> tibble       NA                                        
#> tidyselect   NA                                        
#> tidytemplate NA                                        
#> tinytex      NA                                        
#> urlchecker   NA                                        
#> usethis      NA                                        
#> utf8         NA                                        
#> vctrs        NA                                        
#> viridisLite  NA                                        
#> waldo        NA                                        
#> whisker      NA                                        
#> withr        NA                                        
#> xfun         NA                                        
#> xml2         NA                                        
#> xmlparsedata NA                                        
#> xopen        NA                                        
#> xtable       NA                                        
#> yaml         NA                                        
#> zip          NA                                        
#> pak          NA                                        
#> KernSmooth   NA                                        
#> MASS         NA                                        
#> Matrix       NA                                        
#> base         NA                                        
#> boot         NA                                        
#> class        NA                                        
#> cluster      NA                                        
#> codetools    NA                                        
#> compiler     NA                                        
#> datasets     NA                                        
#> foreign      NA                                        
#> grDevices    NA                                        
#> graphics     NA                                        
#> grid         NA                                        
#> lattice      NA                                        
#> methods      NA                                        
#> mgcv         NA                                        
#> nlme         NA                                        
#> nnet         NA                                        
#> parallel     NA                                        
#> rpart        NA                                        
#> spatial      NA                                        
#> splines      NA                                        
#> stats        NA                                        
#> stats4       NA                                        
#> survival     NA                                        
#> tcltk        NA                                        
#> tools        NA                                        
#> utils        NA                                        
#>              Suggests                                                                                                                                                                                                                                                                                                                                                                                  
#> R.cache      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> R.methodsS3  "codetools"                                                                                                                                                                                                                                                                                                                                                                               
#> R.oo         "tools"                                                                                                                                                                                                                                                                                                                                                                                   
#> R.utils      "datasets, digest (>= 0.6.10)"                                                                                                                                                                                                                                                                                                                                                            
#> R6           "lobstr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                             
#> RColorBrewer NA                                                                                                                                                                                                                                                                                                                                                                                        
#> Rcpp         "tinytest, inline, rbenchmark, pkgKitten (>= 0.1.2)"                                                                                                                                                                                                                                                                                                                                      
#> S7           "bench, callr, covr, knitr, methods, rmarkdown, testthat (>=\n3.2.0), tibble"                                                                                                                                                                                                                                                                                                             
#> abind        NA                                                                                                                                                                                                                                                                                                                                                                                        
#> askpass      "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> backports    NA                                                                                                                                                                                                                                                                                                                                                                                        
#> base64enc    NA                                                                                                                                                                                                                                                                                                                                                                                        
#> brew         "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                     
#> brio         "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> bslib        "brand.yml, bsicons, curl, fontawesome, future, ggplot2,\nknitr, lattice, magrittr, rappdirs, rmarkdown (>= 2.7), shiny\n(>= 1.11.1), testthat, thematic, tools, utils, withr, yaml"                                                                                                                                                                                                      
#> cachem       "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> callr        "asciicast (>= 2.3.1), cli (>= 1.1.0), mockery, ps, rprojroot,\nspelling, testthat (>= 3.2.0), withr (>= 2.3.0)"                                                                                                                                                                                                                                                                          
#> citation     "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                          
#> cli          "callr, covr, crayon, digest, glue (>= 1.6.0), grDevices,\nhtmltools, htmlwidgets, knitr, methods, processx, ps (>=\n1.3.4.9000), rlang (>= 1.0.2.9003), rmarkdown, rprojroot,\nrstudioapi, testthat (>= 3.2.0), tibble, whoami, withr"                                                                                                                                                   
#> clipr        "covr, knitr, rmarkdown, rstudioapi (>= 0.5), testthat (>=\n2.0.0)"                                                                                                                                                                                                                                                                                                                       
#> commonmark   "curl, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                    
#> covr         "R6, S7 (>= 0.2.0), curl, knitr, rmarkdown, htmltools, DT (>=\n0.2), testthat (>= 3.0.0), rlang, rstudioapi (>= 0.2), xml2 (>=\n1.0.0), parallel, memoise, covr, box (>= 1.2.0)"                                                                                                                                                                                                          
#> crayon       "mockery, rstudioapi, testthat, withr"                                                                                                                                                                                                                                                                                                                                                    
#> credentials  "testthat, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                              
#> curl         "spelling, testthat (>= 1.0.0), knitr, jsonlite, later,\nrmarkdown, httpuv (>= 1.4.4), webutils"                                                                                                                                                                                                                                                                                          
#> data.table   "bit64 (>= 4.0.0), bit (>= 4.0.4), R.utils (>= 2.13.0), xts,\nzoo (>= 1.8-1), yaml, knitr, markdown"                                                                                                                                                                                                                                                                                      
#> desc         "callr, covr, gh, spelling, testthat, whoami, withr"                                                                                                                                                                                                                                                                                                                                      
#> devtools     "BiocManager (>= 1.30.18), callr (>= 3.7.1), covr (>= 3.5.1),\ncurl (>= 4.3.2), digest (>= 0.6.29), DT (>= 0.23), foghorn (>=\n1.4.2), gh (>= 1.3.0), httr2 (>= 1.0.0), knitr (>= 1.39), lintr\n(>= 3.0.0), quarto (>= 1.5.1), remotes (>= 2.5.0), rmarkdown\n(>= 2.14), rstudioapi (>= 0.13), spelling (>= 2.2), xml2"                                                                   
#> diffobj      "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                        
#> digest       "tinytest, simplermarkdown, rbenchmark"                                                                                                                                                                                                                                                                                                                                                   
#> downlit      "covr, htmltools, jsonlite, MASS, MassSpecWavelet, pkgload,\nrmarkdown, testthat (>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                        
#> dplyr        "broom, covr, DBI, dbplyr (>= 2.2.1), ggplot2, knitr, Lahman,\nlobstr, nycflights13, purrr, rmarkdown, RSQLite, stringi (>=\n1.7.6), testthat (>= 3.1.5), tidyr (>= 1.3.0), withr"                                                                                                                                                                                                        
#> ellipsis     "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                          
#> evaluate     "callr, covr, ggplot2 (>= 3.3.6), lattice, methods, pkgload,\nragg (>= 1.4.0), rlang (>= 1.1.5), knitr, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                      
#> fansi        "unitizer, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                              
#> farver       "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> fastmap      "testthat (>= 2.1.1)"                                                                                                                                                                                                                                                                                                                                                                     
#> fontawesome  "covr, dplyr (>= 1.0.8), gt (>= 0.9.0), knitr (>= 1.31),\ntestthat (>= 3.0.0), rsvg"                                                                                                                                                                                                                                                                                                      
#> fs           "covr, crayon, knitr, pillar (>= 1.0.0), rmarkdown, spelling,\ntestthat (>= 3.0.0), tibble (>= 1.1.0), vctrs (>= 0.3.0), withr"                                                                                                                                                                                                                                                           
#> gdx          "covr"                                                                                                                                                                                                                                                                                                                                                                                    
#> gdxrrw       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> generics     "covr, pkgload, testthat (>= 3.0.0), tibble, withr"                                                                                                                                                                                                                                                                                                                                       
#> gert         "spelling, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                    
#> ggplot2      "broom, covr, dplyr, ggplot2movies, hexbin, Hmisc, hms, knitr,\nmapproj, maps, MASS, mgcv, multcomp, munsell, nlme, profvis,\nquantreg, quarto, ragg (>= 1.2.6), RColorBrewer, roxygen2,\nrpart, sf (>= 0.7-3), svglite (>= 2.1.2), testthat (>= 3.1.5),\ntibble, vdiffr (>= 1.0.6), xml2"                                                                                                
#> gh           "connectcreds, covr, knitr, rmarkdown, rprojroot, spelling,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                  
#> gitcreds     "codetools, covr, knitr, mockery, oskeyring, rmarkdown,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                      
#> glue         "crayon, DBI (>= 1.2.0), dplyr, knitr, rlang, rmarkdown,\nRSQLite, testthat (>= 3.2.0), vctrs (>= 0.3.0), waldo (>=\n0.5.3), withr"                                                                                                                                                                                                                                                       
#> gtable       "covr, ggplot2, knitr, profvis, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                           
#> highr        "knitr, markdown, testit"                                                                                                                                                                                                                                                                                                                                                                 
#> htmltools    "Cairo, markdown, ragg, shiny, testthat, withr"                                                                                                                                                                                                                                                                                                                                           
#> htmlwidgets  "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> httpuv       "callr, curl, jsonlite, testthat (>= 3.0.0), websocket"                                                                                                                                                                                                                                                                                                                                   
#> httr         "covr, httpuv, jpeg, knitr, png, readr, rmarkdown, testthat\n(>= 0.8.0), xml2"                                                                                                                                                                                                                                                                                                            
#> httr2        "askpass, bench, clipr, covr, docopt, httpuv, jose, jsonlite,\nknitr, later (>= 1.4.0), nanonext, otel (>= 0.2.0), otelsdk (>=\n0.2.0), paws.common (>= 0.8.0), promises, rmarkdown, testthat\n(>= 3.1.8), tibble, webfakes (>= 1.4.0), xml2"                                                                                                                                             
#> ini          "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> isoband      "covr, ggplot2, knitr, magick, bench, rmarkdown, sf, testthat\n(>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                                          
#> jquerylib    "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> jsonlite     "httr, vctrs, testthat, knitr, rmarkdown, R.rsp, sf"                                                                                                                                                                                                                                                                                                                                      
#> knitr        "bslib, DBI (>= 0.4-1), digest, formatR, gifski, gridSVG,\nhtmlwidgets (>= 0.7), jpeg, JuliaCall (>= 0.11.1), magick,\nlitedown, markdown (>= 1.3), otel, otelsdk, png, ragg,\nreticulate (>= 1.4), rgl (>= 0.95.1201), rlang, rmarkdown,\nsass, showtext, styler (>= 1.2.0), targets (>= 0.6.0), testit,\ntibble, tikzDevice (>= 0.10), tinytex (>= 0.56), webshot,\nrstudioapi, svglite"
#> labeling     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> later        "knitr, nanonext, promises, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                               
#> lifecycle    "covr, knitr, lintr (>= 3.1.0), rmarkdown, testthat (>=\n3.0.1), tibble, tidyverse, tools, vctrs, withr, xml2"                                                                                                                                                                                                                                                                            
#> lintr        "bookdown, cyclocomp, jsonlite, patrick (>= 0.2.0), rlang,\nrmarkdown, rstudioapi (>= 0.2), testthat (>= 3.2.1), tibble,\ntufte, withr (>= 2.5.0)"                                                                                                                                                                                                                                        
#> lucode2      "covr, gdx, gdxrrw, gert, ggplot2, knitr, lusweave, magclass,\npoorman, renv, rmarkdown, styler, testthat"                                                                                                                                                                                                                                                                                
#> lusweave     "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> magclass     "covr, ggplot2, knitr, lpjmlkit, ncdf4, pkgconfig, quitte,\nraster, rmarkdown, terra, testthat (>= 3.1.5), tibble, withr"                                                                                                                                                                                                                                                                 
#> magrittr     "covr, knitr, rlang, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                 
#> memoise      "digest, aws.s3, covr, googleAuthR, googleCloudStorageR, httr,\ntestthat"                                                                                                                                                                                                                                                                                                                 
#> mime         NA                                                                                                                                                                                                                                                                                                                                                                                        
#> miniUI       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> openssl      "curl, testthat (>= 2.1.0), digest, knitr, rmarkdown,\njsonlite, jose, sodium"                                                                                                                                                                                                                                                                                                            
#> otel         "callr, cli, glue, jsonlite, otelsdk, processx, shiny,\nspelling, testthat (>= 3.0.0), utils, withr"                                                                                                                                                                                                                                                                                      
#> pak          "callr (>= 3.7.0), cli (>= 3.2.0), covr, curl (>= 4.3.2), desc\n(>= 1.4.1), filelock (>= 1.0.2), gitcreds, glue (>= 1.6.2),\njsonlite (>= 1.8.0), keyring (>= 1.4.0), pingr, pkgbuild (>=\n1.4.2), pkgcache (>= 2.2.4), pkgdepends (>= 0.9.0), pkgload,\npkgsearch (>= 3.1.0), processx (>= 3.8.1), ps (>= 1.6.0),\nrstudioapi, testthat (>= 3.2.0), webfakes, withr, yaml"               
#> pillar       "bit64, DBI, debugme, DiagrammeR, dplyr, formattable, ggplot2,\nknitr, lubridate, nanotime, nycflights13, palmerpenguins,\nrmarkdown, scales, stringi, survival, testthat (>= 3.1.1),\ntibble, units (>= 0.7.2), vdiffr, withr"                                                                                                                                                           
#> pkgbuild     "covr, cpp11, knitr, Rcpp, rmarkdown, testthat (>= 3.2.0),\nwithr (>= 2.3.0)"                                                                                                                                                                                                                                                                                                             
#> pkgconfig    "covr, testthat, disposables (>= 1.0.3)"                                                                                                                                                                                                                                                                                                                                                  
#> pkgdown      "covr, diffviewer, evaluate (>= 0.24.0), gert, gt, htmltools,\nhtmlwidgets, knitr (>= 1.50), magick, methods, pkgload (>=\n1.0.2), quarto, rsconnect, rstudioapi, rticles, sass, testthat\n(>= 3.1.3), tools"                                                                                                                                                                             
#> pkgload      "bitops, jsonlite, mathjaxr, pak, Rcpp, remotes, rstudioapi,\ntestthat (>= 3.2.1.1), usethis, withr"                                                                                                                                                                                                                                                                                      
#> plyr         "abind, covr, doParallel, foreach, iterators, itertools,\ntcltk, testthat"                                                                                                                                                                                                                                                                                                                
#> poorman      "knitr, rmarkdown, roxygen2, tinytest"                                                                                                                                                                                                                                                                                                                                                    
#> praise       "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> prettyunits  "codetools, covr, testthat"                                                                                                                                                                                                                                                                                                                                                               
#> processx     "callr (>= 3.7.3), cli (>= 3.3.0), codetools, covr, curl,\ndebugme, parallel, rlang (>= 1.0.2), testthat (>= 3.0.0),\nwebfakes, withr"                                                                                                                                                                                                                                                    
#> profvis      "htmltools, knitr, rmarkdown, shiny, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                 
#> promises     "future (>= 1.21.0), knitr, mirai, otelsdk (>= 0.2.0), purrr,\nRcpp, rmarkdown, spelling, testthat (>= 3.0.0), vembedr"                                                                                                                                                                                                                                                                   
#> ps           "callr, covr, curl, pillar, pingr, processx (>= 3.1.0), R6,\nrlang, testthat (>= 3.0.0), webfakes, withr"                                                                                                                                                                                                                                                                                 
#> purrr        "carrier (>= 0.3.0), covr, dplyr (>= 0.7.8), httr, knitr,\nlubridate, mirai (>= 2.5.1), rmarkdown, testthat (>= 3.0.0),\ntibble, tidyselect"                                                                                                                                                                                                                                              
#> ragg         "covr, graphics, grid, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                               
#> rappdirs     "covr, roxygen2, testthat (>= 3.2.0), withr"                                                                                                                                                                                                                                                                                                                                              
#> rcmdcheck    "covr, knitr, mockery, processx, ps, rmarkdown, svglite,\ntestthat, webfakes"                                                                                                                                                                                                                                                                                                             
#> renv         "BiocManager, cli, compiler, covr, cpp11, curl, devtools,\ngenerics, gitcreds, jsonlite, jsonvalidate, knitr, miniUI,\nmodules, packrat, pak, R6, remotes, reticulate, rmarkdown,\nrstudioapi, shiny, testthat, uuid, waldo, yaml, webfakes"                                                                                                                                              
#> reshape2     "covr, lattice, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                      
#> rex          "covr, dplyr, ggplot2, Hmisc, knitr, magrittr, rmarkdown,\nroxygen2, rvest, stringr, testthat"                                                                                                                                                                                                                                                                                            
#> rlang        "cli (>= 3.1.0), covr, crayon, desc, fs, glue, knitr,\nmagrittr, methods, pillar, pkgload, rmarkdown, stats, testthat\n(>= 3.3.2), tibble, usethis, vctrs (>= 0.2.3), withr"                                                                                                                                                                                                              
#> rmarkdown    "digest, dygraphs, fs, rsconnect, downlit (>= 0.4.0), katex\n(>= 1.4.0), sass (>= 0.4.0), shiny (>= 1.6.0), testthat (>=\n3.0.3), tibble, vctrs, cleanrmd, withr (>= 2.4.2), xml2"                                                                                                                                                                                                        
#> roxygen2     "covr, R.methodsS3, R.oo, rmarkdown (>= 2.16), testthat (>=\n3.1.2), yaml"                                                                                                                                                                                                                                                                                                                
#> rprojroot    "covr, knitr, lifecycle, rlang, rmarkdown, testthat (>=\n3.2.0), withr"                                                                                                                                                                                                                                                                                                                   
#> rstudioapi   "testthat, knitr, rmarkdown, clipr, covr, curl, jsonlite,\nwithr"                                                                                                                                                                                                                                                                                                                         
#> rversions    "pillar, testthat (>= 3.0.0), webfakes, withr"                                                                                                                                                                                                                                                                                                                                            
#> sass         "testthat, knitr, rmarkdown, withr, shiny, curl"                                                                                                                                                                                                                                                                                                                                          
#> scales       "bit64, covr, dichromat, ggplot2, hms (>= 0.5.0), stringi,\ntestthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                          
#> sessioninfo  "callr, covr, gh, reticulate, rmarkdown, testthat (>= 3.2.0),\nwithr"                                                                                                                                                                                                                                                                                                                     
#> shiny        "Cairo (>= 1.5-5), coro (>= 1.1.0), datasets, DT, dygraphs,\nfuture, ggplot2, knitr (>= 1.6), magrittr, markdown, mirai,\notelsdk (>= 0.2.0), ragg, reactlog (>= 1.0.0), rmarkdown, sass,\nshowtext, testthat (>= 3.2.1), watcher, yaml"                                                                                                                                                  
#> sourcetools  "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> stringi      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> stringr      "covr, dplyr, gt, htmltools, htmlwidgets, knitr, rmarkdown,\ntestthat (>= 3.0.0), tibble"                                                                                                                                                                                                                                                                                                 
#> styler       "data.tree (>= 0.1.6), digest, here, knitr, prettycode,\nrmarkdown, roxygen2, rstudioapi (>= 0.7), tibble (>= 1.4.2),\ntestthat (>= 3.2.1)"                                                                                                                                                                                                                                               
#> sys          "unix (>= 1.4), spelling, testthat"                                                                                                                                                                                                                                                                                                                                                       
#> systemfonts  "covr, farver, ggplot2, graphics, knitr, ragg, rmarkdown,\nsvglite, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                  
#> testthat     "covr, curl (>= 0.9.5), diffviewer (>= 0.1.0), digest (>=\n0.6.33), gh, knitr, otel, otelsdk, rmarkdown, rstudioapi, S7,\nshiny, usethis, vctrs (>= 0.1.0), xml2"                                                                                                                                                                                                                         
#> textshaping  "covr, grDevices, grid, knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                            
#> tibble       "bench, bit64, blob, brio, callr, DiagrammeR, dplyr, evaluate,\nformattable, ggplot2, here, hms, htmltools, knitr, lubridate,\nnycflights13, pkgload, purrr, rmarkdown, stringi, testthat (>=\n3.0.2), tidyr, withr"                                                                                                                                                                      
#> tidyselect   "covr, crayon, dplyr, knitr, magrittr, rmarkdown, stringr,\ntestthat (>= 3.1.1), tibble (>= 2.1.3)"                                                                                                                                                                                                                                                                                       
#> tidytemplate "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                        
#> tinytex      "testit, rstudioapi"                                                                                                                                                                                                                                                                                                                                                                      
#> urlchecker   "covr"                                                                                                                                                                                                                                                                                                                                                                                    
#> usethis      "covr, knitr, magick, pkgload (>= 1.3.2.1), quarto (>= 1.5.1),\nrmarkdown, roxygen2 (>= 7.1.2), spelling (>= 1.2), testthat (>=\n3.1.8)"                                                                                                                                                                                                                                                  
#> utf8         "cli, covr, knitr, rlang, rmarkdown, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                                                                                         
#> vctrs        "bit64, covr, crayon, dplyr (>= 0.8.5), generics, knitr,\npillar (>= 1.4.4), pkgdown (>= 2.0.1), rmarkdown, testthat (>=\n3.0.0), tibble (>= 3.1.3), waldo (>= 0.2.0), withr, xml2,\nzeallot"                                                                                                                                                                                             
#> viridisLite  "hexbin (>= 1.27.0), ggplot2 (>= 1.0.1), testthat, covr"                                                                                                                                                                                                                                                                                                                                  
#> waldo        "bit64, R6, S7, testthat (>= 3.0.0), withr, xml2"                                                                                                                                                                                                                                                                                                                                         
#> whisker      "markdown"                                                                                                                                                                                                                                                                                                                                                                                
#> withr        "callr, DBI, knitr, methods, rlang, rmarkdown (>= 2.12),\nRSQLite, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                   
#> xfun         "testit, parallel, codetools, methods, rstudioapi, tinytex (>=\n0.30), mime, litedown (>= 0.6), commonmark, knitr (>= 1.50),\nremotes, pak, curl, xml2, jsonlite, magick, yaml, data.table,\nqs2"                                                                                                                                                                                         
#> xml2         "covr, curl, httr, knitr, mockery, rmarkdown, testthat (>=\n3.2.0), xslt"                                                                                                                                                                                                                                                                                                                 
#> xmlparsedata "covr, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                    
#> xopen        "ps, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                 
#> xtable       "knitr, zoo, survival, glue, tinytex"                                                                                                                                                                                                                                                                                                                                                     
#> yaml         "knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                   
#> zip          "covr, pillar, processx, R6, testthat, withr"                                                                                                                                                                                                                                                                                                                                             
#> pak          "callr (>= 3.7.0), cli (>= 3.2.0), covr, curl (>= 4.3.2), desc\n(>= 1.4.1), filelock (>= 1.0.2), gitcreds, glue (>= 1.6.2),\njsonlite (>= 1.8.0), keyring (>= 1.4.0), pingr, pkgbuild (>=\n1.4.2), pkgcache (>= 2.2.4), pkgdepends (>= 0.9.0), pkgload,\npkgsearch (>= 3.1.0), processx (>= 3.8.1), ps (>= 1.6.0),\nrstudioapi, testthat (>= 3.2.0), webfakes, withr, yaml"               
#> KernSmooth   "MASS, carData"                                                                                                                                                                                                                                                                                                                                                                           
#> MASS         "lattice, nlme, nnet, survival"                                                                                                                                                                                                                                                                                                                                                           
#> Matrix       "MASS, datasets, sfsmisc, tools"                                                                                                                                                                                                                                                                                                                                                          
#> base         "methods"                                                                                                                                                                                                                                                                                                                                                                                 
#> boot         "MASS, survival"                                                                                                                                                                                                                                                                                                                                                                          
#> class        NA                                                                                                                                                                                                                                                                                                                                                                                        
#> cluster      "MASS, Matrix"                                                                                                                                                                                                                                                                                                                                                                            
#> codetools    NA                                                                                                                                                                                                                                                                                                                                                                                        
#> compiler     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> datasets     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> foreign      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> grDevices    "KernSmooth"                                                                                                                                                                                                                                                                                                                                                                              
#> graphics     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> grid         NA                                                                                                                                                                                                                                                                                                                                                                                        
#> lattice      "KernSmooth, MASS, latticeExtra, colorspace"                                                                                                                                                                                                                                                                                                                                              
#> methods      "codetools"                                                                                                                                                                                                                                                                                                                                                                               
#> mgcv         "parallel, survival, MASS"                                                                                                                                                                                                                                                                                                                                                                
#> nlme         "MASS, SASmixed"                                                                                                                                                                                                                                                                                                                                                                          
#> nnet         "MASS"                                                                                                                                                                                                                                                                                                                                                                                    
#> parallel     "methods"                                                                                                                                                                                                                                                                                                                                                                                 
#> rpart        "survival"                                                                                                                                                                                                                                                                                                                                                                                
#> spatial      "MASS"                                                                                                                                                                                                                                                                                                                                                                                    
#> splines      "Matrix, methods"                                                                                                                                                                                                                                                                                                                                                                         
#> stats        "MASS, Matrix, SuppDists, methods, stats4"                                                                                                                                                                                                                                                                                                                                                
#> stats4       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> survival     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> tcltk        NA                                                                                                                                                                                                                                                                                                                                                                                        
#> tools        "codetools, methods, xml2, curl, commonmark, knitr, xfun, mathjaxr, V8, bibtex"                                                                                                                                                                                                                                                                                                           
#> utils        "methods, xml2, commonmark, knitr, jsonlite"                                                                                                                                                                                                                                                                                                                                              
#>              Enhances                                               
#> R.cache      NA                                                     
#> R.methodsS3  NA                                                     
#> R.oo         NA                                                     
#> R.utils      NA                                                     
#> R6           NA                                                     
#> RColorBrewer NA                                                     
#> Rcpp         NA                                                     
#> S7           NA                                                     
#> abind        NA                                                     
#> askpass      NA                                                     
#> backports    NA                                                     
#> base64enc    "png"                                                  
#> brew         NA                                                     
#> brio         NA                                                     
#> bslib        NA                                                     
#> cachem       NA                                                     
#> callr        NA                                                     
#> citation     NA                                                     
#> cli          NA                                                     
#> clipr        NA                                                     
#> commonmark   NA                                                     
#> covr         NA                                                     
#> crayon       NA                                                     
#> credentials  NA                                                     
#> curl         NA                                                     
#> data.table   NA                                                     
#> desc         NA                                                     
#> devtools     NA                                                     
#> diffobj      NA                                                     
#> digest       NA                                                     
#> downlit      NA                                                     
#> dplyr        NA                                                     
#> ellipsis     NA                                                     
#> evaluate     NA                                                     
#> fansi        NA                                                     
#> farver       NA                                                     
#> fastmap      NA                                                     
#> fontawesome  NA                                                     
#> fs           NA                                                     
#> gdx          NA                                                     
#> gdxrrw       NA                                                     
#> generics     NA                                                     
#> gert         NA                                                     
#> ggplot2      "sp"                                                   
#> gh           NA                                                     
#> gitcreds     NA                                                     
#> glue         NA                                                     
#> gtable       NA                                                     
#> highr        NA                                                     
#> htmltools    "knitr"                                                
#> htmlwidgets  "shiny (>= 1.1)"                                       
#> httpuv       NA                                                     
#> httr         NA                                                     
#> httr2        NA                                                     
#> ini          NA                                                     
#> isoband      NA                                                     
#> jquerylib    NA                                                     
#> jsonlite     NA                                                     
#> knitr        NA                                                     
#> labeling     NA                                                     
#> later        NA                                                     
#> lifecycle    NA                                                     
#> lintr        "data.table"                                           
#> lucode2      NA                                                     
#> lusweave     NA                                                     
#> magclass     NA                                                     
#> magrittr     NA                                                     
#> memoise      NA                                                     
#> mime         NA                                                     
#> miniUI       NA                                                     
#> openssl      NA                                                     
#> otel         NA                                                     
#> pak          NA                                                     
#> pillar       NA                                                     
#> pkgbuild     NA                                                     
#> pkgconfig    NA                                                     
#> pkgdown      NA                                                     
#> pkgload      NA                                                     
#> plyr         NA                                                     
#> poorman      NA                                                     
#> praise       NA                                                     
#> prettyunits  NA                                                     
#> processx     NA                                                     
#> profvis      NA                                                     
#> promises     NA                                                     
#> ps           NA                                                     
#> purrr        NA                                                     
#> ragg         NA                                                     
#> rappdirs     NA                                                     
#> rcmdcheck    NA                                                     
#> renv         NA                                                     
#> reshape2     NA                                                     
#> rex          NA                                                     
#> rlang        "winch"                                                
#> rmarkdown    NA                                                     
#> roxygen2     NA                                                     
#> rprojroot    NA                                                     
#> rstudioapi   NA                                                     
#> rversions    NA                                                     
#> sass         NA                                                     
#> scales       NA                                                     
#> sessioninfo  NA                                                     
#> shiny        NA                                                     
#> sourcetools  NA                                                     
#> stringi      NA                                                     
#> stringr      NA                                                     
#> styler       NA                                                     
#> sys          NA                                                     
#> systemfonts  NA                                                     
#> testthat     NA                                                     
#> textshaping  NA                                                     
#> tibble       NA                                                     
#> tidyselect   NA                                                     
#> tidytemplate NA                                                     
#> tinytex      NA                                                     
#> urlchecker   NA                                                     
#> usethis      NA                                                     
#> utf8         NA                                                     
#> vctrs        NA                                                     
#> viridisLite  NA                                                     
#> waldo        NA                                                     
#> whisker      NA                                                     
#> withr        NA                                                     
#> xfun         NA                                                     
#> xml2         NA                                                     
#> xmlparsedata NA                                                     
#> xopen        NA                                                     
#> xtable       NA                                                     
#> yaml         NA                                                     
#> zip          NA                                                     
#> pak          NA                                                     
#> KernSmooth   NA                                                     
#> MASS         NA                                                     
#> Matrix       "SparseM, graph"                                       
#> base         "chron, date, round"                                   
#> boot         NA                                                     
#> class        NA                                                     
#> cluster      "mvoutlier, fpc, ellipse, sfsmisc"                     
#> codetools    NA                                                     
#> compiler     NA                                                     
#> datasets     NA                                                     
#> foreign      NA                                                     
#> grDevices    NA                                                     
#> graphics     "vcd"                                                  
#> grid         NA                                                     
#> lattice      "chron, zoo"                                           
#> methods      NA                                                     
#> mgcv         NA                                                     
#> nlme         NA                                                     
#> nnet         NA                                                     
#> parallel     "snow, Rmpi, mirai, parallelly"                        
#> rpart        NA                                                     
#> spatial      NA                                                     
#> splines      NA                                                     
#> stats        "Kendall, coin, multcomp, pcaPP, pspearman, robustbase"
#> stats4       NA                                                     
#> survival     NA                                                     
#> tcltk        NA                                                     
#> tools        NA                                                     
#> utils        NA                                                     
#>              License                                         
#> R.cache      "LGPL (>= 2.1)"                                 
#> R.methodsS3  "LGPL (>= 2.1)"                                 
#> R.oo         "LGPL (>= 2.1)"                                 
#> R.utils      "LGPL (>= 2.1)"                                 
#> R6           "MIT + file LICENSE"                            
#> RColorBrewer "Apache License 2.0"                            
#> Rcpp         "GPL (>= 2)"                                    
#> S7           "MIT + file LICENSE"                            
#> abind        "MIT + file LICENSE"                            
#> askpass      "MIT + file LICENSE"                            
#> backports    "GPL-2 | GPL-3"                                 
#> base64enc    "GPL-2 | GPL-3"                                 
#> brew         "GPL (>= 2)"                                    
#> brio         "MIT + file LICENSE"                            
#> bslib        "MIT + file LICENSE"                            
#> cachem       "MIT + file LICENSE"                            
#> callr        "MIT + file LICENSE"                            
#> citation     "BSD_2_clause + file LICENSE"                   
#> cli          "MIT + file LICENSE"                            
#> clipr        "GPL-3"                                         
#> commonmark   "BSD_2_clause + file LICENSE"                   
#> covr         "MIT + file LICENSE"                            
#> crayon       "MIT + file LICENSE"                            
#> credentials  "MIT + file LICENSE"                            
#> curl         "MIT + file LICENSE"                            
#> data.table   "MPL-2.0 | file LICENSE"                        
#> desc         "MIT + file LICENSE"                            
#> devtools     "MIT + file LICENSE"                            
#> diffobj      "GPL-2 | GPL-3"                                 
#> digest       "GPL (>= 2)"                                    
#> downlit      "MIT + file LICENSE"                            
#> dplyr        "MIT + file LICENSE"                            
#> ellipsis     "MIT + file LICENSE"                            
#> evaluate     "MIT + file LICENSE"                            
#> fansi        "GPL-2 | GPL-3"                                 
#> farver       "MIT + file LICENSE"                            
#> fastmap      "MIT + file LICENSE"                            
#> fontawesome  "MIT + file LICENSE"                            
#> fs           "MIT + file LICENSE"                            
#> gdx          "BSD_2_clause + file LICENSE"                   
#> gdxrrw       "EPL2 with Secondary License GPL-2.0 or greater"
#> generics     "MIT + file LICENSE"                            
#> gert         "MIT + file LICENSE"                            
#> ggplot2      "MIT + file LICENSE"                            
#> gh           "MIT + file LICENSE"                            
#> gitcreds     "MIT + file LICENSE"                            
#> glue         "MIT + file LICENSE"                            
#> gtable       "MIT + file LICENSE"                            
#> highr        "GPL"                                           
#> htmltools    "GPL (>= 2)"                                    
#> htmlwidgets  "MIT + file LICENSE"                            
#> httpuv       "GPL (>= 2) | file LICENSE"                     
#> httr         "MIT + file LICENSE"                            
#> httr2        "MIT + file LICENSE"                            
#> ini          "GPL-3"                                         
#> isoband      "MIT + file LICENSE"                            
#> jquerylib    "MIT + file LICENSE"                            
#> jsonlite     "MIT + file LICENSE"                            
#> knitr        "GPL"                                           
#> labeling     "MIT + file LICENSE | Unlimited"                
#> later        "MIT + file LICENSE"                            
#> lifecycle    "MIT + file LICENSE"                            
#> lintr        "MIT + file LICENSE"                            
#> lucode2      "BSD_2_clause + file LICENSE"                   
#> lusweave     "BSD_2_clause + file LICENSE"                   
#> magclass     "LGPL-3 | file LICENSE"                         
#> magrittr     "MIT + file LICENSE"                            
#> memoise      "MIT + file LICENSE"                            
#> mime         "GPL"                                           
#> miniUI       "GPL-3"                                         
#> openssl      "MIT + file LICENSE"                            
#> otel         "MIT + file LICENSE"                            
#> pak          "GPL-3"                                         
#> pillar       "MIT + file LICENSE"                            
#> pkgbuild     "MIT + file LICENSE"                            
#> pkgconfig    "MIT + file LICENSE"                            
#> pkgdown      "MIT + file LICENSE"                            
#> pkgload      "MIT + file LICENSE"                            
#> plyr         "MIT + file LICENSE"                            
#> poorman      "MIT + file LICENSE"                            
#> praise       "MIT + file LICENSE"                            
#> prettyunits  "MIT + file LICENSE"                            
#> processx     "MIT + file LICENSE"                            
#> profvis      "MIT + file LICENSE"                            
#> promises     "MIT + file LICENSE"                            
#> ps           "MIT + file LICENSE"                            
#> purrr        "MIT + file LICENSE"                            
#> ragg         "MIT + file LICENSE"                            
#> rappdirs     "MIT + file LICENSE"                            
#> rcmdcheck    "MIT + file LICENSE"                            
#> renv         "MIT + file LICENSE"                            
#> reshape2     "MIT + file LICENSE"                            
#> rex          "MIT + file LICENSE"                            
#> rlang        "MIT + file LICENSE"                            
#> rmarkdown    "GPL-3"                                         
#> roxygen2     "MIT + file LICENSE"                            
#> rprojroot    "MIT + file LICENSE"                            
#> rstudioapi   "MIT + file LICENSE"                            
#> rversions    "MIT + file LICENSE"                            
#> sass         "MIT + file LICENSE"                            
#> scales       "MIT + file LICENSE"                            
#> sessioninfo  "GPL-2"                                         
#> shiny        "MIT + file LICENSE"                            
#> sourcetools  "MIT + file LICENSE"                            
#> stringi      "file LICENSE"                                  
#> stringr      "MIT + file LICENSE"                            
#> styler       "MIT + file LICENSE"                            
#> sys          "MIT + file LICENSE"                            
#> systemfonts  "MIT + file LICENSE"                            
#> testthat     "MIT + file LICENSE"                            
#> textshaping  "MIT + file LICENSE"                            
#> tibble       "MIT + file LICENSE"                            
#> tidyselect   "MIT + file LICENSE"                            
#> tidytemplate "MIT + file LICENSE"                            
#> tinytex      "MIT + file LICENSE"                            
#> urlchecker   "GPL-3"                                         
#> usethis      "MIT + file LICENSE"                            
#> utf8         "Apache License (== 2.0) | file LICENSE"        
#> vctrs        "MIT + file LICENSE"                            
#> viridisLite  "MIT + file LICENSE"                            
#> waldo        "MIT + file LICENSE"                            
#> whisker      "GPL-3"                                         
#> withr        "MIT + file LICENSE"                            
#> xfun         "MIT + file LICENSE"                            
#> xml2         "MIT + file LICENSE"                            
#> xmlparsedata "MIT + file LICENSE"                            
#> xopen        "MIT + file LICENSE"                            
#> xtable       "GPL (>= 2)"                                    
#> yaml         "BSD_3_clause + file LICENSE"                   
#> zip          "MIT + file LICENSE"                            
#> pak          "GPL-3"                                         
#> KernSmooth   "Unlimited"                                     
#> MASS         "GPL-2 | GPL-3"                                 
#> Matrix       "GPL (>= 2) | file LICENCE"                     
#> base         "Part of R 4.6.0"                               
#> boot         "Unlimited"                                     
#> class        "GPL-2 | GPL-3"                                 
#> cluster      "GPL (>= 2)"                                    
#> codetools    "GPL"                                           
#> compiler     "Part of R 4.6.0"                               
#> datasets     "Part of R 4.6.0"                               
#> foreign      "GPL (>= 2)"                                    
#> grDevices    "Part of R 4.6.0"                               
#> graphics     "Part of R 4.6.0"                               
#> grid         "Part of R 4.6.0"                               
#> lattice      "GPL (>= 2)"                                    
#> methods      "Part of R 4.6.0"                               
#> mgcv         "GPL (>= 2)"                                    
#> nlme         "GPL (>= 2)"                                    
#> nnet         "GPL-2 | GPL-3"                                 
#> parallel     "Part of R 4.6.0"                               
#> rpart        "GPL-2 | GPL-3"                                 
#> spatial      "GPL-2 | GPL-3"                                 
#> splines      "Part of R 4.6.0"                               
#> stats        "Part of R 4.6.0"                               
#> stats4       "Part of R 4.6.0"                               
#> survival     "LGPL (>= 2)"                                   
#> tcltk        "Part of R 4.6.0"                               
#> tools        "Part of R 4.6.0"                               
#> utils        "Part of R 4.6.0"                               
#>              License_is_FOSS License_restricts_use OS_type MD5sum
#> R.cache      NA              NA                    NA      NA    
#> R.methodsS3  NA              NA                    NA      NA    
#> R.oo         NA              NA                    NA      NA    
#> R.utils      NA              NA                    NA      NA    
#> R6           NA              NA                    NA      NA    
#> RColorBrewer NA              NA                    NA      NA    
#> Rcpp         NA              NA                    NA      NA    
#> S7           NA              NA                    NA      NA    
#> abind        NA              NA                    NA      NA    
#> askpass      NA              NA                    NA      NA    
#> backports    NA              NA                    NA      NA    
#> base64enc    NA              NA                    NA      NA    
#> brew         NA              NA                    NA      NA    
#> brio         NA              NA                    NA      NA    
#> bslib        NA              NA                    NA      NA    
#> cachem       NA              NA                    NA      NA    
#> callr        NA              NA                    NA      NA    
#> citation     NA              NA                    NA      NA    
#> cli          NA              NA                    NA      NA    
#> clipr        NA              NA                    NA      NA    
#> commonmark   NA              NA                    NA      NA    
#> covr         NA              NA                    NA      NA    
#> crayon       NA              NA                    NA      NA    
#> credentials  NA              NA                    NA      NA    
#> curl         NA              NA                    NA      NA    
#> data.table   NA              NA                    NA      NA    
#> desc         NA              NA                    NA      NA    
#> devtools     NA              NA                    NA      NA    
#> diffobj      NA              NA                    NA      NA    
#> digest       NA              NA                    NA      NA    
#> downlit      NA              NA                    NA      NA    
#> dplyr        NA              NA                    NA      NA    
#> ellipsis     NA              NA                    NA      NA    
#> evaluate     NA              NA                    NA      NA    
#> fansi        NA              NA                    NA      NA    
#> farver       NA              NA                    NA      NA    
#> fastmap      NA              NA                    NA      NA    
#> fontawesome  NA              NA                    NA      NA    
#> fs           NA              NA                    NA      NA    
#> gdx          NA              NA                    NA      NA    
#> gdxrrw       NA              NA                    NA      NA    
#> generics     NA              NA                    NA      NA    
#> gert         NA              NA                    NA      NA    
#> ggplot2      NA              NA                    NA      NA    
#> gh           NA              NA                    NA      NA    
#> gitcreds     NA              NA                    NA      NA    
#> glue         NA              NA                    NA      NA    
#> gtable       NA              NA                    NA      NA    
#> highr        NA              NA                    NA      NA    
#> htmltools    NA              NA                    NA      NA    
#> htmlwidgets  NA              NA                    NA      NA    
#> httpuv       NA              NA                    NA      NA    
#> httr         NA              NA                    NA      NA    
#> httr2        NA              NA                    NA      NA    
#> ini          NA              NA                    NA      NA    
#> isoband      NA              NA                    NA      NA    
#> jquerylib    NA              NA                    NA      NA    
#> jsonlite     NA              NA                    NA      NA    
#> knitr        NA              NA                    NA      NA    
#> labeling     NA              NA                    NA      NA    
#> later        NA              NA                    NA      NA    
#> lifecycle    NA              NA                    NA      NA    
#> lintr        NA              NA                    NA      NA    
#> lucode2      NA              NA                    NA      NA    
#> lusweave     NA              NA                    NA      NA    
#> magclass     NA              NA                    NA      NA    
#> magrittr     NA              NA                    NA      NA    
#> memoise      NA              NA                    NA      NA    
#> mime         NA              NA                    NA      NA    
#> miniUI       NA              NA                    NA      NA    
#> openssl      NA              NA                    NA      NA    
#> otel         NA              NA                    NA      NA    
#> pak          NA              NA                    NA      NA    
#> pillar       NA              NA                    NA      NA    
#> pkgbuild     NA              NA                    NA      NA    
#> pkgconfig    NA              NA                    NA      NA    
#> pkgdown      NA              NA                    NA      NA    
#> pkgload      NA              NA                    NA      NA    
#> plyr         NA              NA                    NA      NA    
#> poorman      NA              NA                    NA      NA    
#> praise       NA              NA                    NA      NA    
#> prettyunits  NA              NA                    NA      NA    
#> processx     NA              NA                    NA      NA    
#> profvis      NA              NA                    NA      NA    
#> promises     NA              NA                    NA      NA    
#> ps           NA              NA                    NA      NA    
#> purrr        NA              NA                    NA      NA    
#> ragg         NA              NA                    NA      NA    
#> rappdirs     NA              NA                    NA      NA    
#> rcmdcheck    NA              NA                    NA      NA    
#> renv         NA              NA                    NA      NA    
#> reshape2     NA              NA                    NA      NA    
#> rex          NA              NA                    NA      NA    
#> rlang        NA              NA                    NA      NA    
#> rmarkdown    NA              NA                    NA      NA    
#> roxygen2     NA              NA                    NA      NA    
#> rprojroot    NA              NA                    NA      NA    
#> rstudioapi   NA              NA                    NA      NA    
#> rversions    NA              NA                    NA      NA    
#> sass         NA              NA                    NA      NA    
#> scales       NA              NA                    NA      NA    
#> sessioninfo  NA              NA                    NA      NA    
#> shiny        NA              NA                    NA      NA    
#> sourcetools  NA              NA                    NA      NA    
#> stringi      "yes"           NA                    NA      NA    
#> stringr      NA              NA                    NA      NA    
#> styler       NA              NA                    NA      NA    
#> sys          NA              NA                    NA      NA    
#> systemfonts  NA              NA                    NA      NA    
#> testthat     NA              NA                    NA      NA    
#> textshaping  NA              NA                    NA      NA    
#> tibble       NA              NA                    NA      NA    
#> tidyselect   NA              NA                    NA      NA    
#> tidytemplate NA              NA                    NA      NA    
#> tinytex      NA              NA                    NA      NA    
#> urlchecker   NA              NA                    NA      NA    
#> usethis      NA              NA                    NA      NA    
#> utf8         NA              NA                    NA      NA    
#> vctrs        NA              NA                    NA      NA    
#> viridisLite  NA              NA                    NA      NA    
#> waldo        NA              NA                    NA      NA    
#> whisker      NA              NA                    NA      NA    
#> withr        NA              NA                    NA      NA    
#> xfun         NA              NA                    NA      NA    
#> xml2         NA              NA                    NA      NA    
#> xmlparsedata NA              NA                    NA      NA    
#> xopen        NA              NA                    NA      NA    
#> xtable       NA              NA                    NA      NA    
#> yaml         NA              NA                    NA      NA    
#> zip          NA              NA                    NA      NA    
#> pak          NA              NA                    NA      NA    
#> KernSmooth   NA              NA                    NA      NA    
#> MASS         NA              NA                    NA      NA    
#> Matrix       NA              NA                    NA      NA    
#> base         NA              NA                    NA      NA    
#> boot         NA              NA                    NA      NA    
#> class        NA              NA                    NA      NA    
#> cluster      NA              NA                    NA      NA    
#> codetools    NA              NA                    NA      NA    
#> compiler     NA              NA                    NA      NA    
#> datasets     NA              NA                    NA      NA    
#> foreign      NA              NA                    NA      NA    
#> grDevices    NA              NA                    NA      NA    
#> graphics     NA              NA                    NA      NA    
#> grid         NA              NA                    NA      NA    
#> lattice      NA              NA                    NA      NA    
#> methods      NA              NA                    NA      NA    
#> mgcv         NA              NA                    NA      NA    
#> nlme         NA              NA                    NA      NA    
#> nnet         NA              NA                    NA      NA    
#> parallel     NA              NA                    NA      NA    
#> rpart        NA              NA                    NA      NA    
#> spatial      NA              NA                    NA      NA    
#> splines      NA              NA                    NA      NA    
#> stats        NA              NA                    NA      NA    
#> stats4       NA              NA                    NA      NA    
#> survival     NA              NA                    NA      NA    
#> tcltk        NA              NA                    NA      NA    
#> tools        NA              NA                    NA      NA    
#> utils        NA              NA                    NA      NA    
#>              NeedsCompilation
#> R.cache      "no"            
#> R.methodsS3  "no"            
#> R.oo         "no"            
#> R.utils      "no"            
#> R6           "no"            
#> RColorBrewer "no"            
#> Rcpp         "yes"           
#> S7           "yes"           
#> abind        "no"            
#> askpass      "yes"           
#> backports    "yes"           
#> base64enc    "yes"           
#> brew         "no"            
#> brio         "yes"           
#> bslib        "no"            
#> cachem       "yes"           
#> callr        "no"            
#> citation     "no"            
#> cli          "yes"           
#> clipr        "no"            
#> commonmark   "yes"           
#> covr         "yes"           
#> crayon       "no"            
#> credentials  "no"            
#> curl         "yes"           
#> data.table   "yes"           
#> desc         "no"            
#> devtools     "no"            
#> diffobj      "yes"           
#> digest       "yes"           
#> downlit      "no"            
#> dplyr        "yes"           
#> ellipsis     "no"            
#> evaluate     "no"            
#> fansi        "yes"           
#> farver       "yes"           
#> fastmap      "yes"           
#> fontawesome  "no"            
#> fs           "yes"           
#> gdx          "no"            
#> gdxrrw       "yes"           
#> generics     "no"            
#> gert         "yes"           
#> ggplot2      "no"            
#> gh           "no"            
#> gitcreds     "no"            
#> glue         "yes"           
#> gtable       "no"            
#> highr        "no"            
#> htmltools    "yes"           
#> htmlwidgets  "no"            
#> httpuv       "yes"           
#> httr         "no"            
#> httr2        "no"            
#> ini          "no"            
#> isoband      "yes"           
#> jquerylib    "no"            
#> jsonlite     "yes"           
#> knitr        "no"            
#> labeling     "no"            
#> later        "yes"           
#> lifecycle    "no"            
#> lintr        "no"            
#> lucode2      "no"            
#> lusweave     "no"            
#> magclass     "no"            
#> magrittr     "yes"           
#> memoise      "no"            
#> mime         "yes"           
#> miniUI       "no"            
#> openssl      "yes"           
#> otel         "no"            
#> pak          "yes"           
#> pillar       "no"            
#> pkgbuild     "no"            
#> pkgconfig    "no"            
#> pkgdown      "no"            
#> pkgload      "no"            
#> plyr         "yes"           
#> poorman      "no"            
#> praise       "no"            
#> prettyunits  "no"            
#> processx     "yes"           
#> profvis      "yes"           
#> promises     "no"            
#> ps           "yes"           
#> purrr        "yes"           
#> ragg         "yes"           
#> rappdirs     "yes"           
#> rcmdcheck    "no"            
#> renv         "no"            
#> reshape2     "yes"           
#> rex          "no"            
#> rlang        "yes"           
#> rmarkdown    "no"            
#> roxygen2     "yes"           
#> rprojroot    "no"            
#> rstudioapi   "no"            
#> rversions    "no"            
#> sass         "yes"           
#> scales       "no"            
#> sessioninfo  "no"            
#> shiny        "no"            
#> sourcetools  "yes"           
#> stringi      "yes"           
#> stringr      "no"            
#> styler       "no"            
#> sys          "yes"           
#> systemfonts  "yes"           
#> testthat     "yes"           
#> textshaping  "yes"           
#> tibble       "yes"           
#> tidyselect   "yes"           
#> tidytemplate "no"            
#> tinytex      "no"            
#> urlchecker   "no"            
#> usethis      "no"            
#> utf8         "yes"           
#> vctrs        "yes"           
#> viridisLite  "no"            
#> waldo        "no"            
#> whisker      "no"            
#> withr        "no"            
#> xfun         "yes"           
#> xml2         "yes"           
#> xmlparsedata "no"            
#> xopen        "no"            
#> xtable       "no"            
#> yaml         "yes"           
#> zip          "yes"           
#> pak          "yes"           
#> KernSmooth   "yes"           
#> MASS         "yes"           
#> Matrix       "yes"           
#> base         NA              
#> boot         "no"            
#> class        "yes"           
#> cluster      "yes"           
#> codetools    "no"            
#> compiler     NA              
#> datasets     NA              
#> foreign      "yes"           
#> grDevices    "yes"           
#> graphics     "yes"           
#> grid         "yes"           
#> lattice      "yes"           
#> methods      "yes"           
#> mgcv         "yes"           
#> nlme         "yes"           
#> nnet         "yes"           
#> parallel     "yes"           
#> rpart        "yes"           
#> spatial      "yes"           
#> splines      "yes"           
#> stats        "yes"           
#> stats4       NA              
#> survival     "yes"           
#> tcltk        "yes"           
#> tools        "yes"           
#> utils        "yes"           
#>              Built                                                         
#> R.cache      "R 4.6.0; ; 2026-04-24 21:12:21 UTC; unix"                    
#> R.methodsS3  "R 4.6.0; ; 2026-04-24 01:34:58 UTC; unix"                    
#> R.oo         "R 4.6.0; ; 2026-04-24 01:37:41 UTC; unix"                    
#> R.utils      "R 4.6.0; ; 2026-04-24 01:39:00 UTC; unix"                    
#> R6           "R 4.6.0; ; 2026-04-24 01:25:41 UTC; unix"                    
#> RColorBrewer "R 4.6.0; ; 2026-04-24 01:19:19 UTC; unix"                    
#> Rcpp         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-25 12:48:18 UTC; unix" 
#> S7           "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:21:55 UTC; unix" 
#> abind        "R 4.6.0; ; 2026-04-24 01:22:17 UTC; unix"                    
#> askpass      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:25:18 UTC; unix" 
#> backports    "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:12:26 UTC; unix" 
#> base64enc    "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:05:52 UTC; unix" 
#> brew         "R 4.6.0; ; 2026-04-24 21:09:21 UTC; unix"                    
#> brio         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:12:08 UTC; unix" 
#> bslib        "R 4.6.0; ; 2026-04-24 02:57:14 UTC; unix"                    
#> cachem       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:49:23 UTC; unix" 
#> callr        "R 4.6.0; ; 2026-04-24 02:16:09 UTC; unix"                    
#> citation     "R 4.6.0; ; 2026-04-24 22:39:03 UTC; unix"                    
#> cli          "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:56:15 UTC; unix" 
#> clipr        "R 4.6.0; ; 2026-04-24 01:15:27 UTC; unix"                    
#> commonmark   "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:23:17 UTC; unix" 
#> covr         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 22:26:55 UTC; unix" 
#> crayon       "R 4.6.0; ; 2026-04-24 01:23:50 UTC; unix"                    
#> credentials  "R 4.6.0; ; 2026-04-24 01:37:50 UTC; unix"                    
#> curl         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:32:50 UTC; unix" 
#> data.table   "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:22:19 UTC; unix" 
#> desc         "R 4.6.0; ; 2026-04-24 01:58:25 UTC; unix"                    
#> devtools     "R 4.6.0; ; 2026-04-24 23:23:15 UTC; unix"                    
#> diffobj      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:08:51 UTC; unix" 
#> digest       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:12:08 UTC; unix" 
#> downlit      "R 4.6.0; ; 2026-04-24 22:45:05 UTC; unix"                    
#> dplyr        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:59:58 UTC; unix" 
#> ellipsis     "R 4.6.0; ; 2026-04-24 21:36:12 UTC; unix"                    
#> evaluate     "R 4.6.0; ; 2026-04-24 01:39:41 UTC; unix"                    
#> fansi        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 21:16:37 UTC; unix" 
#> farver       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:18:01 UTC; unix" 
#> fastmap      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:49:13 UTC; unix" 
#> fontawesome  "R 4.6.0; ; 2026-04-24 02:45:51 UTC; unix"                    
#> fs           "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:13:01 UTC; unix" 
#> gdx          "R 4.6.0; ; 2026-04-29 09:25:50 UTC; unix"                    
#> gdxrrw       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-29 09:25:31 UTC; unix" 
#> generics     "R 4.6.0; ; 2026-04-24 01:24:54 UTC; unix"                    
#> gert         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:41:34 UTC; unix" 
#> ggplot2      "R 4.6.0; ; 2026-04-24 02:57:17 UTC; unix"                    
#> gh           "R 4.6.0; ; 2026-04-24 03:07:30 UTC; unix"                    
#> gitcreds     "R 4.6.0; ; 2026-04-24 01:13:09 UTC; unix"                    
#> glue         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:12:31 UTC; unix" 
#> gtable       "R 4.6.0; ; 2026-04-24 01:59:24 UTC; unix"                    
#> highr        "R 4.6.0; ; 2026-04-24 03:25:28 UTC; unix"                    
#> htmltools    "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:41:28 UTC; unix" 
#> htmlwidgets  "R 4.6.0; ; 2026-04-24 03:40:28 UTC; unix"                    
#> httpuv       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-25 14:27:19 UTC; unix" 
#> httr         "R 4.6.0; ; 2026-04-24 01:37:52 UTC; unix"                    
#> httr2        "R 4.6.0; ; 2026-04-24 02:45:35 UTC; unix"                    
#> ini          "R 4.6.0; ; 2026-04-24 01:22:01 UTC; unix"                    
#> isoband      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:58:57 UTC; unix" 
#> jquerylib    "R 4.6.0; ; 2026-04-24 02:45:26 UTC; unix"                    
#> jsonlite     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:24:54 UTC; unix" 
#> knitr        "R 4.6.0; ; 2026-04-24 03:26:17 UTC; unix"                    
#> labeling     "R 4.6.0; ; 2026-04-24 01:27:24 UTC; unix"                    
#> later        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-25 14:15:52 UTC; unix" 
#> lifecycle    "R 4.6.0; ; 2026-04-24 01:58:39 UTC; unix"                    
#> lintr        "R 4.6.0; ; 2026-04-24 22:41:00 UTC; unix"                    
#> lucode2      "R 4.6.0; ; 2026-04-29 09:25:45 UTC; unix"                    
#> lusweave     "R 4.6.0; ; 2026-04-29 09:25:42 UTC; unix"                    
#> magclass     "R 4.6.0; ; 2026-04-24 21:36:31 UTC; unix"                    
#> magrittr     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:39:46 UTC; unix" 
#> memoise      "R 4.6.0; ; 2026-04-24 02:16:20 UTC; unix"                    
#> mime         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:12:59 UTC; unix" 
#> miniUI       "R 4.6.0; ; 2026-04-24 22:47:52 UTC; unix"                    
#> openssl      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:26:47 UTC; unix" 
#> otel         "R 4.6.0; ; 2026-04-24 01:29:05 UTC; unix"                    
#> pak          "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-28 03:40:03 UTC; unix" 
#> pillar       "R 4.6.0; ; 2026-04-24 02:36:29 UTC; unix"                    
#> pkgbuild     "R 4.6.0; ; 2026-04-24 02:21:47 UTC; unix"                    
#> pkgconfig    "R 4.6.0; ; 2026-04-24 01:18:01 UTC; unix"                    
#> pkgdown      "R 4.6.0; ; 2026-04-24 23:02:30 UTC; unix"                    
#> pkgload      "R 4.6.0; ; 2026-04-24 02:25:10 UTC; unix"                    
#> plyr         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-25 14:32:22 UTC; unix" 
#> poorman      "R 4.6.0; ; 2026-04-24 21:32:36 UTC; unix"                    
#> praise       "R 4.6.0; ; 2026-04-24 01:16:13 UTC; unix"                    
#> prettyunits  "R 4.6.0; ; 2026-04-24 01:22:56 UTC; unix"                    
#> processx     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:14:48 UTC; unix" 
#> profvis      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 22:19:47 UTC; unix" 
#> promises     "R 4.6.0; ; 2026-04-24 02:01:43 UTC; unix"                    
#> ps           "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:59:38 UTC; unix" 
#> purrr        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:56:19 UTC; unix" 
#> ragg         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 21:55:10 UTC; unix" 
#> rappdirs     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:26:10 UTC; unix" 
#> rcmdcheck    "R 4.6.0; ; 2026-04-24 22:11:24 UTC; unix"                    
#> renv         "R 4.6.0; ; 2026-04-24 21:35:32 UTC; unix"                    
#> reshape2     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-25 15:01:35 UTC; unix" 
#> rex          "R 4.6.0; ; 2026-04-24 22:06:31 UTC; unix"                    
#> rlang        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:41:30 UTC; unix" 
#> rmarkdown    "R 4.6.0; ; 2026-04-24 03:34:56 UTC; unix"                    
#> roxygen2     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 22:18:39 UTC; unix" 
#> rprojroot    "R 4.6.0; ; 2026-04-24 01:29:06 UTC; unix"                    
#> rstudioapi   "R 4.6.0; ; 2026-04-24 01:31:06 UTC; unix"                    
#> rversions    "R 4.6.0; ; 2026-04-24 21:11:46 UTC; unix"                    
#> sass         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:46:56 UTC; unix" 
#> scales       "R 4.6.0; ; 2026-04-24 02:00:26 UTC; unix"                    
#> sessioninfo  "R 4.6.0; ; 2026-04-24 21:10:29 UTC; unix"                    
#> shiny        "R 4.6.0; ; 2026-04-24 22:23:51 UTC; unix"                    
#> sourcetools  "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:26:01 UTC; unix" 
#> stringi      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:28:52 UTC; unix" 
#> stringr      "R 4.6.0; ; 2026-04-24 02:37:54 UTC; unix"                    
#> styler       "R 4.6.0; ; 2026-04-24 22:07:55 UTC; unix"                    
#> sys          "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:23:34 UTC; unix" 
#> systemfonts  "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:24:48 UTC; unix" 
#> testthat     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 03:02:07 UTC; unix" 
#> textshaping  "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 21:46:25 UTC; unix" 
#> tibble       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:41:18 UTC; unix" 
#> tidyselect   "R 4.6.0; ; 2026-04-24 02:57:25 UTC; unix"                    
#> tidytemplate "R 4.6.0; ; 2026-04-29 09:23:57 UTC; unix"                    
#> tinytex      "R 4.6.0; ; 2026-04-24 03:26:16 UTC; unix"                    
#> urlchecker   "R 4.6.0; ; 2026-04-24 21:39:46 UTC; unix"                    
#> usethis      "R 4.6.0; ; 2026-04-24 03:11:10 UTC; unix"                    
#> utf8         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:27:06 UTC; unix" 
#> vctrs        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:34:26 UTC; unix" 
#> viridisLite  "R 4.6.0; ; 2026-04-24 01:27:48 UTC; unix"                    
#> waldo        "R 4.6.0; ; 2026-04-24 02:10:14 UTC; unix"                    
#> whisker      "R 4.6.0; ; 2026-04-24 02:27:36 UTC; unix"                    
#> withr        "R 4.6.0; ; 2026-04-24 01:28:21 UTC; unix"                    
#> xfun         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 03:10:53 UTC; unix" 
#> xml2         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 02:01:15 UTC; unix" 
#> xmlparsedata "R 4.6.0; ; 2026-04-24 22:36:57 UTC; unix"                    
#> xopen        "R 4.6.0; ; 2026-04-24 21:34:04 UTC; unix"                    
#> xtable       "R 4.6.0; ; 2026-04-24 01:35:24 UTC; unix"                    
#> yaml         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:35:34 UTC; unix" 
#> zip          "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 01:33:36 UTC; unix" 
#> pak          "R 4.6.0; x86_64-pc-linux-musl; 2026-04-27 11:28:07 UTC; unix"
#> KernSmooth   "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:26 UTC; unix" 
#> MASS         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:58:08 UTC; unix" 
#> Matrix       "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:58:21 UTC; unix" 
#> base         "R 4.6.0; ; 2026-04-24 08:57:19 UTC; unix"                    
#> boot         "R 4.6.0; ; 2026-04-24 09:01:13 UTC; unix"                    
#> class        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:31 UTC; unix" 
#> cluster      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:16 UTC; unix" 
#> codetools    "R 4.6.0; ; 2026-04-24 09:01:20 UTC; unix"                    
#> compiler     "R 4.6.0; ; 2026-04-24 08:55:35 UTC; unix"                    
#> datasets     "R 4.6.0; ; 2026-04-24 08:56:55 UTC; unix"                    
#> foreign      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:22 UTC; unix" 
#> grDevices    "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:56:09 UTC; unix" 
#> graphics     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:56:16 UTC; unix" 
#> grid         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:57:08 UTC; unix" 
#> lattice      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:58:13 UTC; unix" 
#> methods      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:56:56 UTC; unix" 
#> mgcv         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:36 UTC; unix" 
#> nlme         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:00:35 UTC; unix" 
#> nnet         "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:33 UTC; unix" 
#> parallel     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:57:17 UTC; unix" 
#> rpart        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:27 UTC; unix" 
#> spatial      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:01:34 UTC; unix" 
#> splines      "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:57:14 UTC; unix" 
#> stats        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:56:25 UTC; unix" 
#> stats4       "R 4.6.0; ; 2026-04-24 08:57:15 UTC; unix"                    
#> survival     "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 09:00:47 UTC; unix" 
#> tcltk        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:57:16 UTC; unix" 
#> tools        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:55:35 UTC; unix" 
#> utils        "R 4.6.0; x86_64-pc-linux-gnu; 2026-04-24 08:56:01 UTC; unix" 
#>              Published
#> R.cache      NA       
#> R.methodsS3  NA       
#> R.oo         NA       
#> R.utils      NA       
#> R6           NA       
#> RColorBrewer NA       
#> Rcpp         NA       
#> S7           NA       
#> abind        NA       
#> askpass      NA       
#> backports    NA       
#> base64enc    NA       
#> brew         NA       
#> brio         NA       
#> bslib        NA       
#> cachem       NA       
#> callr        NA       
#> citation     NA       
#> cli          NA       
#> clipr        NA       
#> commonmark   NA       
#> covr         NA       
#> crayon       NA       
#> credentials  NA       
#> curl         NA       
#> data.table   NA       
#> desc         NA       
#> devtools     NA       
#> diffobj      NA       
#> digest       NA       
#> downlit      NA       
#> dplyr        NA       
#> ellipsis     NA       
#> evaluate     NA       
#> fansi        NA       
#> farver       NA       
#> fastmap      NA       
#> fontawesome  NA       
#> fs           NA       
#> gdx          NA       
#> gdxrrw       NA       
#> generics     NA       
#> gert         NA       
#> ggplot2      NA       
#> gh           NA       
#> gitcreds     NA       
#> glue         NA       
#> gtable       NA       
#> highr        NA       
#> htmltools    NA       
#> htmlwidgets  NA       
#> httpuv       NA       
#> httr         NA       
#> httr2        NA       
#> ini          NA       
#> isoband      NA       
#> jquerylib    NA       
#> jsonlite     NA       
#> knitr        NA       
#> labeling     NA       
#> later        NA       
#> lifecycle    NA       
#> lintr        NA       
#> lucode2      NA       
#> lusweave     NA       
#> magclass     NA       
#> magrittr     NA       
#> memoise      NA       
#> mime         NA       
#> miniUI       NA       
#> openssl      NA       
#> otel         NA       
#> pak          NA       
#> pillar       NA       
#> pkgbuild     NA       
#> pkgconfig    NA       
#> pkgdown      NA       
#> pkgload      NA       
#> plyr         NA       
#> poorman      NA       
#> praise       NA       
#> prettyunits  NA       
#> processx     NA       
#> profvis      NA       
#> promises     NA       
#> ps           NA       
#> purrr        NA       
#> ragg         NA       
#> rappdirs     NA       
#> rcmdcheck    NA       
#> renv         NA       
#> reshape2     NA       
#> rex          NA       
#> rlang        NA       
#> rmarkdown    NA       
#> roxygen2     NA       
#> rprojroot    NA       
#> rstudioapi   NA       
#> rversions    NA       
#> sass         NA       
#> scales       NA       
#> sessioninfo  NA       
#> shiny        NA       
#> sourcetools  NA       
#> stringi      NA       
#> stringr      NA       
#> styler       NA       
#> sys          NA       
#> systemfonts  NA       
#> testthat     NA       
#> textshaping  NA       
#> tibble       NA       
#> tidyselect   NA       
#> tidytemplate NA       
#> tinytex      NA       
#> urlchecker   NA       
#> usethis      NA       
#> utf8         NA       
#> vctrs        NA       
#> viridisLite  NA       
#> waldo        NA       
#> whisker      NA       
#> withr        NA       
#> xfun         NA       
#> xml2         NA       
#> xmlparsedata NA       
#> xopen        NA       
#> xtable       NA       
#> yaml         NA       
#> zip          NA       
#> pak          NA       
#> KernSmooth   NA       
#> MASS         NA       
#> Matrix       NA       
#> base         NA       
#> boot         NA       
#> class        NA       
#> cluster      NA       
#> codetools    NA       
#> compiler     NA       
#> datasets     NA       
#> foreign      NA       
#> grDevices    NA       
#> graphics     NA       
#> grid         NA       
#> lattice      NA       
#> methods      NA       
#> mgcv         NA       
#> nlme         NA       
#> nnet         NA       
#> parallel     NA       
#> rpart        NA       
#> spatial      NA       
#> splines      NA       
#> stats        NA       
#> stats4       NA       
#> survival     NA       
#> tcltk        NA       
#> tools        NA       
#> utils        NA       
#> 
```
