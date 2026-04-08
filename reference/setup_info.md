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
#>                                              sysname 
#>                                              "Linux" 
#>                                              release 
#>                                  "6.17.0-1008-azure" 
#>                                              version 
#> "#8~24.04.1-Ubuntu SMP Mon Jan 26 18:35:40 UTC 2026" 
#>                                             nodename 
#>                                      "runnervm727z3" 
#>                                              machine 
#>                                             "x86_64" 
#>                                                login 
#>                                            "unknown" 
#>                                                 user 
#>                                             "runner" 
#>                                       effective_user 
#>                                             "runner" 
#> 
#> $sessionInfo
#> R version 4.5.3 (2026-03-11)
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
#> [1] lucode2_0.54.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] generics_0.1.4      rappdirs_0.3.4      sass_0.4.10        
#>  [4] renv_1.2.0          xml2_1.5.2          digest_0.6.39      
#>  [7] magrittr_2.0.5      evaluate_1.0.5      pkgload_1.5.1      
#> [10] fastmap_1.2.0       jsonlite_2.0.0      processx_3.8.7     
#> [13] pkgbuild_1.4.8      sessioninfo_1.2.3   whisker_0.4.1      
#> [16] backports_1.5.1     ps_1.9.2            purrr_1.2.1        
#> [19] fansi_1.0.7         lintr_3.3.0-1       textshaping_1.0.5  
#> [22] httr2_1.2.2         jquerylib_0.1.4     cli_3.6.5          
#> [25] rlang_1.2.0         tidytemplate_1.0.0  ellipsis_0.3.3     
#> [28] withr_3.0.2         cachem_1.1.0        yaml_2.3.12        
#> [31] devtools_2.5.0      otel_0.2.0          tools_4.5.3        
#> [34] memoise_2.0.1       dplyr_1.2.1         curl_7.0.0         
#> [37] vctrs_0.7.2         R6_2.6.1            lifecycle_1.0.5    
#> [40] htmlwidgets_1.6.4   fs_2.0.1            usethis_3.2.1      
#> [43] ragg_1.5.2          fontawesome_0.5.3   pkgconfig_2.0.3    
#> [46] desc_1.4.3          callr_3.7.6         rex_1.2.2          
#> [49] pkgdown_2.2.0       pillar_1.11.1       bslib_0.10.0       
#> [52] glue_1.8.0          data.table_1.18.2.1 systemfonts_1.3.2  
#> [55] tidyselect_1.2.1    xfun_0.57           tibble_3.3.1       
#> [58] rstudioapi_0.18.0   knitr_1.51          htmltools_0.5.9    
#> [61] rmarkdown_2.31      compiler_4.5.3      downlit_0.4.5      
#> [64] askpass_1.2.1       openssl_2.3.5      
#> 
#> $libPaths
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.5.3/lib/R/site-library"
#> [3] "/opt/R/4.5.3/lib/R/library"     
#> 
#> $installedpackages
#>              Package        LibPath                          
#> DT           "DT"           "/home/runner/work/_temp/Library"
#> Formula      "Formula"      "/home/runner/work/_temp/Library"
#> GDPuc        "GDPuc"        "/home/runner/work/_temp/Library"
#> GPArotation  "GPArotation"  "/home/runner/work/_temp/Library"
#> Hmisc        "Hmisc"        "/home/runner/work/_temp/Library"
#> R.cache      "R.cache"      "/home/runner/work/_temp/Library"
#> R.methodsS3  "R.methodsS3"  "/home/runner/work/_temp/Library"
#> R.oo         "R.oo"         "/home/runner/work/_temp/Library"
#> R.utils      "R.utils"      "/home/runner/work/_temp/Library"
#> R6           "R6"           "/home/runner/work/_temp/Library"
#> RColorBrewer "RColorBrewer" "/home/runner/work/_temp/Library"
#> Rcpp         "Rcpp"         "/home/runner/work/_temp/Library"
#> S7           "S7"           "/home/runner/work/_temp/Library"
#> WDI          "WDI"          "/home/runner/work/_temp/Library"
#> abind        "abind"        "/home/runner/work/_temp/Library"
#> askpass      "askpass"      "/home/runner/work/_temp/Library"
#> backports    "backports"    "/home/runner/work/_temp/Library"
#> base64enc    "base64enc"    "/home/runner/work/_temp/Library"
#> bit          "bit"          "/home/runner/work/_temp/Library"
#> bit64        "bit64"        "/home/runner/work/_temp/Library"
#> box          "box"          "/home/runner/work/_temp/Library"
#> brew         "brew"         "/home/runner/work/_temp/Library"
#> brio         "brio"         "/home/runner/work/_temp/Library"
#> bslib        "bslib"        "/home/runner/work/_temp/Library"
#> cachem       "cachem"       "/home/runner/work/_temp/Library"
#> callr        "callr"        "/home/runner/work/_temp/Library"
#> cellranger   "cellranger"   "/home/runner/work/_temp/Library"
#> checkmate    "checkmate"    "/home/runner/work/_temp/Library"
#> citation     "citation"     "/home/runner/work/_temp/Library"
#> cli          "cli"          "/home/runner/work/_temp/Library"
#> clipr        "clipr"        "/home/runner/work/_temp/Library"
#> collections  "collections"  "/home/runner/work/_temp/Library"
#> colorspace   "colorspace"   "/home/runner/work/_temp/Library"
#> commonmark   "commonmark"   "/home/runner/work/_temp/Library"
#> corpcor      "corpcor"      "/home/runner/work/_temp/Library"
#> countrycode  "countrycode"  "/home/runner/work/_temp/Library"
#> covr         "covr"         "/home/runner/work/_temp/Library"
#> crayon       "crayon"       "/home/runner/work/_temp/Library"
#> credentials  "credentials"  "/home/runner/work/_temp/Library"
#> crosstalk    "crosstalk"    "/home/runner/work/_temp/Library"
#> curl         "curl"         "/home/runner/work/_temp/Library"
#> data.table   "data.table"   "/home/runner/work/_temp/Library"
#> desc         "desc"         "/home/runner/work/_temp/Library"
#> devtools     "devtools"     "/home/runner/work/_temp/Library"
#> diffobj      "diffobj"      "/home/runner/work/_temp/Library"
#> digest       "digest"       "/home/runner/work/_temp/Library"
#> doParallel   "doParallel"   "/home/runner/work/_temp/Library"
#> downlit      "downlit"      "/home/runner/work/_temp/Library"
#> dplyr        "dplyr"        "/home/runner/work/_temp/Library"
#> ellipsis     "ellipsis"     "/home/runner/work/_temp/Library"
#> evaluate     "evaluate"     "/home/runner/work/_temp/Library"
#> fansi        "fansi"        "/home/runner/work/_temp/Library"
#> farver       "farver"       "/home/runner/work/_temp/Library"
#> fastmap      "fastmap"      "/home/runner/work/_temp/Library"
#> fdrtool      "fdrtool"      "/home/runner/work/_temp/Library"
#> filelock     "filelock"     "/home/runner/work/_temp/Library"
#> fontawesome  "fontawesome"  "/home/runner/work/_temp/Library"
#> forcats      "forcats"      "/home/runner/work/_temp/Library"
#> foreach      "foreach"      "/home/runner/work/_temp/Library"
#> fs           "fs"           "/home/runner/work/_temp/Library"
#> gamstransfer "gamstransfer" "/home/runner/work/_temp/Library"
#> gdx          "gdx"          "/home/runner/work/_temp/Library"
#> gdxrrw       "gdxrrw"       "/home/runner/work/_temp/Library"
#> generics     "generics"     "/home/runner/work/_temp/Library"
#> gert         "gert"         "/home/runner/work/_temp/Library"
#> ggplot2      "ggplot2"      "/home/runner/work/_temp/Library"
#> gh           "gh"           "/home/runner/work/_temp/Library"
#> gitcreds     "gitcreds"     "/home/runner/work/_temp/Library"
#> glasso       "glasso"       "/home/runner/work/_temp/Library"
#> glue         "glue"         "/home/runner/work/_temp/Library"
#> gms          "gms"          "/home/runner/work/_temp/Library"
#> goxygen      "goxygen"      "/home/runner/work/_temp/Library"
#> gridExtra    "gridExtra"    "/home/runner/work/_temp/Library"
#> gtable       "gtable"       "/home/runner/work/_temp/Library"
#> gtools       "gtools"       "/home/runner/work/_temp/Library"
#> highr        "highr"        "/home/runner/work/_temp/Library"
#> hms          "hms"          "/home/runner/work/_temp/Library"
#> htmlTable    "htmlTable"    "/home/runner/work/_temp/Library"
#> htmltools    "htmltools"    "/home/runner/work/_temp/Library"
#> htmlwidgets  "htmlwidgets"  "/home/runner/work/_temp/Library"
#> httpuv       "httpuv"       "/home/runner/work/_temp/Library"
#> httr         "httr"         "/home/runner/work/_temp/Library"
#> httr2        "httr2"        "/home/runner/work/_temp/Library"
#> igraph       "igraph"       "/home/runner/work/_temp/Library"
#> ini          "ini"          "/home/runner/work/_temp/Library"
#> isoband      "isoband"      "/home/runner/work/_temp/Library"
#> iterators    "iterators"    "/home/runner/work/_temp/Library"
#> jpeg         "jpeg"         "/home/runner/work/_temp/Library"
#> jquerylib    "jquerylib"    "/home/runner/work/_temp/Library"
#> jsonlite     "jsonlite"     "/home/runner/work/_temp/Library"
#> knitr        "knitr"        "/home/runner/work/_temp/Library"
#> labeling     "labeling"     "/home/runner/work/_temp/Library"
#> later        "later"        "/home/runner/work/_temp/Library"
#> lavaan       "lavaan"       "/home/runner/work/_temp/Library"
#> lazyeval     "lazyeval"     "/home/runner/work/_temp/Library"
#> lifecycle    "lifecycle"    "/home/runner/work/_temp/Library"
#> lintr        "lintr"        "/home/runner/work/_temp/Library"
#> lpjmlkit     "lpjmlkit"     "/home/runner/work/_temp/Library"
#> lubridate    "lubridate"    "/home/runner/work/_temp/Library"
#> lucode2      "lucode2"      "/home/runner/work/_temp/Library"
#> lusweave     "lusweave"     "/home/runner/work/_temp/Library"
#> madrat       "madrat"       "/home/runner/work/_temp/Library"
#> magclass     "magclass"     "/home/runner/work/_temp/Library"
#> magrittr     "magrittr"     "/home/runner/work/_temp/Library"
#> memoise      "memoise"      "/home/runner/work/_temp/Library"
#> mime         "mime"         "/home/runner/work/_temp/Library"
#> miniUI       "miniUI"       "/home/runner/work/_temp/Library"
#> mnormt       "mnormt"       "/home/runner/work/_temp/Library"
#> ncdf4        "ncdf4"        "/home/runner/work/_temp/Library"
#> numDeriv     "numDeriv"     "/home/runner/work/_temp/Library"
#> openssl      "openssl"      "/home/runner/work/_temp/Library"
#> otel         "otel"         "/home/runner/work/_temp/Library"
#> pak          "pak"          "/home/runner/work/_temp/Library"
#> pander       "pander"       "/home/runner/work/_temp/Library"
#> pbapply      "pbapply"      "/home/runner/work/_temp/Library"
#> pbivnorm     "pbivnorm"     "/home/runner/work/_temp/Library"
#> pillar       "pillar"       "/home/runner/work/_temp/Library"
#> pkgbuild     "pkgbuild"     "/home/runner/work/_temp/Library"
#> pkgconfig    "pkgconfig"    "/home/runner/work/_temp/Library"
#> pkgdown      "pkgdown"      "/home/runner/work/_temp/Library"
#> pkgload      "pkgload"      "/home/runner/work/_temp/Library"
#> plyr         "plyr"         "/home/runner/work/_temp/Library"
#> png          "png"          "/home/runner/work/_temp/Library"
#> poorman      "poorman"      "/home/runner/work/_temp/Library"
#> praise       "praise"       "/home/runner/work/_temp/Library"
#> prettyunits  "prettyunits"  "/home/runner/work/_temp/Library"
#> processx     "processx"     "/home/runner/work/_temp/Library"
#> profvis      "profvis"      "/home/runner/work/_temp/Library"
#> promises     "promises"     "/home/runner/work/_temp/Library"
#> ps           "ps"           "/home/runner/work/_temp/Library"
#> psych        "psych"        "/home/runner/work/_temp/Library"
#> purrr        "purrr"        "/home/runner/work/_temp/Library"
#> qgraph       "qgraph"       "/home/runner/work/_temp/Library"
#> quadprog     "quadprog"     "/home/runner/work/_temp/Library"
#> quitte       "quitte"       "/home/runner/work/_temp/Library"
#> ragg         "ragg"         "/home/runner/work/_temp/Library"
#> rappdirs     "rappdirs"     "/home/runner/work/_temp/Library"
#> raster       "raster"       "/home/runner/work/_temp/Library"
#> rcmdcheck    "rcmdcheck"    "/home/runner/work/_temp/Library"
#> readr        "readr"        "/home/runner/work/_temp/Library"
#> readxl       "readxl"       "/home/runner/work/_temp/Library"
#> rematch      "rematch"      "/home/runner/work/_temp/Library"
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
#> sp           "sp"           "/home/runner/work/_temp/Library"
#> stringi      "stringi"      "/home/runner/work/_temp/Library"
#> stringr      "stringr"      "/home/runner/work/_temp/Library"
#> styler       "styler"       "/home/runner/work/_temp/Library"
#> sys          "sys"          "/home/runner/work/_temp/Library"
#> systemfonts  "systemfonts"  "/home/runner/work/_temp/Library"
#> terra        "terra"        "/home/runner/work/_temp/Library"
#> testthat     "testthat"     "/home/runner/work/_temp/Library"
#> textshaping  "textshaping"  "/home/runner/work/_temp/Library"
#> tibble       "tibble"       "/home/runner/work/_temp/Library"
#> tidyr        "tidyr"        "/home/runner/work/_temp/Library"
#> tidyselect   "tidyselect"   "/home/runner/work/_temp/Library"
#> tidytemplate "tidytemplate" "/home/runner/work/_temp/Library"
#> timechange   "timechange"   "/home/runner/work/_temp/Library"
#> tinytex      "tinytex"      "/home/runner/work/_temp/Library"
#> tzdb         "tzdb"         "/home/runner/work/_temp/Library"
#> urlchecker   "urlchecker"   "/home/runner/work/_temp/Library"
#> usethis      "usethis"      "/home/runner/work/_temp/Library"
#> utf8         "utf8"         "/home/runner/work/_temp/Library"
#> vctrs        "vctrs"        "/home/runner/work/_temp/Library"
#> viridisLite  "viridisLite"  "/home/runner/work/_temp/Library"
#> vroom        "vroom"        "/home/runner/work/_temp/Library"
#> waldo        "waldo"        "/home/runner/work/_temp/Library"
#> whisker      "whisker"      "/home/runner/work/_temp/Library"
#> withr        "withr"        "/home/runner/work/_temp/Library"
#> writexl      "writexl"      "/home/runner/work/_temp/Library"
#> xfun         "xfun"         "/home/runner/work/_temp/Library"
#> xml2         "xml2"         "/home/runner/work/_temp/Library"
#> xmlparsedata "xmlparsedata" "/home/runner/work/_temp/Library"
#> xopen        "xopen"        "/home/runner/work/_temp/Library"
#> xtable       "xtable"       "/home/runner/work/_temp/Library"
#> yaml         "yaml"         "/home/runner/work/_temp/Library"
#> zip          "zip"          "/home/runner/work/_temp/Library"
#> zoo          "zoo"          "/home/runner/work/_temp/Library"
#> pak          "pak"          "/opt/R/4.5.3/lib/R/site-library"
#> KernSmooth   "KernSmooth"   "/opt/R/4.5.3/lib/R/library"     
#> MASS         "MASS"         "/opt/R/4.5.3/lib/R/library"     
#> Matrix       "Matrix"       "/opt/R/4.5.3/lib/R/library"     
#> base         "base"         "/opt/R/4.5.3/lib/R/library"     
#> boot         "boot"         "/opt/R/4.5.3/lib/R/library"     
#> class        "class"        "/opt/R/4.5.3/lib/R/library"     
#> cluster      "cluster"      "/opt/R/4.5.3/lib/R/library"     
#> codetools    "codetools"    "/opt/R/4.5.3/lib/R/library"     
#> compiler     "compiler"     "/opt/R/4.5.3/lib/R/library"     
#> datasets     "datasets"     "/opt/R/4.5.3/lib/R/library"     
#> foreign      "foreign"      "/opt/R/4.5.3/lib/R/library"     
#> grDevices    "grDevices"    "/opt/R/4.5.3/lib/R/library"     
#> graphics     "graphics"     "/opt/R/4.5.3/lib/R/library"     
#> grid         "grid"         "/opt/R/4.5.3/lib/R/library"     
#> lattice      "lattice"      "/opt/R/4.5.3/lib/R/library"     
#> methods      "methods"      "/opt/R/4.5.3/lib/R/library"     
#> mgcv         "mgcv"         "/opt/R/4.5.3/lib/R/library"     
#> nlme         "nlme"         "/opt/R/4.5.3/lib/R/library"     
#> nnet         "nnet"         "/opt/R/4.5.3/lib/R/library"     
#> parallel     "parallel"     "/opt/R/4.5.3/lib/R/library"     
#> rpart        "rpart"        "/opt/R/4.5.3/lib/R/library"     
#> spatial      "spatial"      "/opt/R/4.5.3/lib/R/library"     
#> splines      "splines"      "/opt/R/4.5.3/lib/R/library"     
#> stats        "stats"        "/opt/R/4.5.3/lib/R/library"     
#> stats4       "stats4"       "/opt/R/4.5.3/lib/R/library"     
#> survival     "survival"     "/opt/R/4.5.3/lib/R/library"     
#> tcltk        "tcltk"        "/opt/R/4.5.3/lib/R/library"     
#> tools        "tools"        "/opt/R/4.5.3/lib/R/library"     
#> utils        "utils"        "/opt/R/4.5.3/lib/R/library"     
#>              Version      Priority     
#> DT           "0.34.0"     NA           
#> Formula      "1.2-5"      NA           
#> GDPuc        "1.6.1"      NA           
#> GPArotation  "2025.3-1"   NA           
#> Hmisc        "5.2-5"      NA           
#> R.cache      "0.17.0"     NA           
#> R.methodsS3  "1.8.2"      NA           
#> R.oo         "1.27.1"     NA           
#> R.utils      "2.13.0"     NA           
#> R6           "2.6.1"      NA           
#> RColorBrewer "1.1-3"      NA           
#> Rcpp         "1.1.1"      NA           
#> S7           "0.2.1"      NA           
#> WDI          "2.7.10"     NA           
#> abind        "1.4-8"      NA           
#> askpass      "1.2.1"      NA           
#> backports    "1.5.1"      NA           
#> base64enc    "0.1-6"      NA           
#> bit          "4.6.0"      NA           
#> bit64        "4.6.0-1"    NA           
#> box          "1.2.1"      NA           
#> brew         "1.0-10"     NA           
#> brio         "1.1.5"      NA           
#> bslib        "0.10.0"     NA           
#> cachem       "1.1.0"      NA           
#> callr        "3.7.6"      NA           
#> cellranger   "1.1.0"      NA           
#> checkmate    "2.3.4"      NA           
#> citation     "0.12.2"     NA           
#> cli          "3.6.5"      NA           
#> clipr        "0.8.0"      NA           
#> collections  "0.3.12"     NA           
#> colorspace   "2.1-2"      NA           
#> commonmark   "2.0.0"      NA           
#> corpcor      "1.6.10"     NA           
#> countrycode  "1.7.0"      NA           
#> covr         "3.6.5"      NA           
#> crayon       "1.5.3"      NA           
#> credentials  "2.0.3"      NA           
#> crosstalk    "1.2.2"      NA           
#> curl         "7.0.0"      NA           
#> data.table   "1.18.2.1"   NA           
#> desc         "1.4.3"      NA           
#> devtools     "2.5.0"      NA           
#> diffobj      "0.3.6"      NA           
#> digest       "0.6.39"     NA           
#> doParallel   "1.0.17"     NA           
#> downlit      "0.4.5"      NA           
#> dplyr        "1.2.1"      NA           
#> ellipsis     "0.3.3"      NA           
#> evaluate     "1.0.5"      NA           
#> fansi        "1.0.7"      NA           
#> farver       "2.1.2"      NA           
#> fastmap      "1.2.0"      NA           
#> fdrtool      "1.2.18"     NA           
#> filelock     "1.0.3"      NA           
#> fontawesome  "0.5.3"      NA           
#> forcats      "1.0.1"      NA           
#> foreach      "1.5.2"      NA           
#> fs           "2.0.1"      NA           
#> gamstransfer "3.0.8"      NA           
#> gdx          "1.53.1"     NA           
#> gdxrrw       "1.0.10"     NA           
#> generics     "0.1.4"      NA           
#> gert         "2.3.1"      NA           
#> ggplot2      "4.0.2"      NA           
#> gh           "1.5.0"      NA           
#> gitcreds     "0.1.2"      NA           
#> glasso       "1.11"       NA           
#> glue         "1.8.0"      NA           
#> gms          "0.33.7"     NA           
#> goxygen      "1.5.0"      NA           
#> gridExtra    "2.3"        NA           
#> gtable       "0.3.6"      NA           
#> gtools       "3.9.5"      NA           
#> highr        "0.12"       NA           
#> hms          "1.1.4"      NA           
#> htmlTable    "2.4.3"      NA           
#> htmltools    "0.5.9"      NA           
#> htmlwidgets  "1.6.4"      NA           
#> httpuv       "1.6.17"     NA           
#> httr         "1.4.8"      NA           
#> httr2        "1.2.2"      NA           
#> igraph       "2.2.3"      NA           
#> ini          "0.3.1"      NA           
#> isoband      "0.3.0"      NA           
#> iterators    "1.0.14"     NA           
#> jpeg         "0.1-11"     NA           
#> jquerylib    "0.1.4"      NA           
#> jsonlite     "2.0.0"      NA           
#> knitr        "1.51"       NA           
#> labeling     "0.4.3"      NA           
#> later        "1.4.8"      NA           
#> lavaan       "0.6-21"     NA           
#> lazyeval     "0.2.3"      NA           
#> lifecycle    "1.0.5"      NA           
#> lintr        "3.3.0-1"    NA           
#> lpjmlkit     "1.8.0"      NA           
#> lubridate    "1.9.5"      NA           
#> lucode2      "0.54.1"     NA           
#> lusweave     "1.46.5"     NA           
#> madrat       "3.36.2"     NA           
#> magclass     "6.13.2"     NA           
#> magrittr     "2.0.5"      NA           
#> memoise      "2.0.1"      NA           
#> mime         "0.13"       NA           
#> miniUI       "0.1.2"      NA           
#> mnormt       "2.1.2"      NA           
#> ncdf4        "1.24"       NA           
#> numDeriv     "2016.8-1.1" NA           
#> openssl      "2.3.5"      NA           
#> otel         "0.2.0"      NA           
#> pak          "0.9.2"      NA           
#> pander       "0.6.6"      NA           
#> pbapply      "1.7-4"      NA           
#> pbivnorm     "0.6.0"      NA           
#> pillar       "1.11.1"     NA           
#> pkgbuild     "1.4.8"      NA           
#> pkgconfig    "2.0.3"      NA           
#> pkgdown      "2.2.0"      NA           
#> pkgload      "1.5.1"      NA           
#> plyr         "1.8.9"      NA           
#> png          "0.1-9"      NA           
#> poorman      "0.2.7"      NA           
#> praise       "1.0.0"      NA           
#> prettyunits  "1.2.0"      NA           
#> processx     "3.8.7"      NA           
#> profvis      "0.4.0"      NA           
#> promises     "1.5.0"      NA           
#> ps           "1.9.2"      NA           
#> psych        "2.6.3"      NA           
#> purrr        "1.2.1"      NA           
#> qgraph       "1.9.8"      NA           
#> quadprog     "1.5-8"      NA           
#> quitte       "0.3148.0"   NA           
#> ragg         "1.5.2"      NA           
#> rappdirs     "0.3.4"      NA           
#> raster       "3.6-32"     NA           
#> rcmdcheck    "1.4.0"      NA           
#> readr        "2.2.0"      NA           
#> readxl       "1.4.5"      NA           
#> rematch      "2.0.0"      NA           
#> renv         "1.2.0"      NA           
#> reshape2     "1.4.5"      NA           
#> rex          "1.2.2"      NA           
#> rlang        "1.2.0"      NA           
#> rmarkdown    "2.31"       NA           
#> roxygen2     "7.3.3"      NA           
#> rprojroot    "2.1.1"      NA           
#> rstudioapi   "0.18.0"     NA           
#> rversions    "3.0.0"      NA           
#> sass         "0.4.10"     NA           
#> scales       "1.4.0"      NA           
#> sessioninfo  "1.2.3"      NA           
#> shiny        "1.13.0"     NA           
#> sourcetools  "0.1.7-2"    NA           
#> sp           "2.2-1"      NA           
#> stringi      "1.8.7"      NA           
#> stringr      "1.6.0"      NA           
#> styler       "1.11.0"     NA           
#> sys          "3.4.3"      NA           
#> systemfonts  "1.3.2"      NA           
#> terra        "1.9-11"     NA           
#> testthat     "3.3.2"      NA           
#> textshaping  "1.0.5"      NA           
#> tibble       "3.3.1"      NA           
#> tidyr        "1.3.2"      NA           
#> tidyselect   "1.2.1"      NA           
#> tidytemplate "1.0.0"      NA           
#> timechange   "0.4.0"      NA           
#> tinytex      "0.59"       NA           
#> tzdb         "0.5.0"      NA           
#> urlchecker   "1.0.1"      NA           
#> usethis      "3.2.1"      NA           
#> utf8         "1.2.6"      NA           
#> vctrs        "0.7.2"      NA           
#> viridisLite  "0.4.3"      NA           
#> vroom        "1.7.1"      NA           
#> waldo        "0.6.2"      NA           
#> whisker      "0.4.1"      NA           
#> withr        "3.0.2"      NA           
#> writexl      "1.5.4"      NA           
#> xfun         "0.57"       NA           
#> xml2         "1.5.2"      NA           
#> xmlparsedata "1.0.5"      NA           
#> xopen        "1.0.1"      NA           
#> xtable       "1.8-8"      NA           
#> yaml         "2.3.12"     NA           
#> zip          "2.3.3"      NA           
#> zoo          "1.8-15"     NA           
#> pak          "0.9.2"      NA           
#> KernSmooth   "2.23-26"    "recommended"
#> MASS         "7.3-65"     "recommended"
#> Matrix       "1.7-4"      "recommended"
#> base         "4.5.3"      "base"       
#> boot         "1.3-32"     "recommended"
#> class        "7.3-23"     "recommended"
#> cluster      "2.1.8.2"    "recommended"
#> codetools    "0.2-20"     "recommended"
#> compiler     "4.5.3"      "base"       
#> datasets     "4.5.3"      "base"       
#> foreign      "0.8-91"     "recommended"
#> grDevices    "4.5.3"      "base"       
#> graphics     "4.5.3"      "base"       
#> grid         "4.5.3"      "base"       
#> lattice      "0.22-9"     "recommended"
#> methods      "4.5.3"      "base"       
#> mgcv         "1.9-4"      "recommended"
#> nlme         "3.1-168"    "recommended"
#> nnet         "7.3-20"     "recommended"
#> parallel     "4.5.3"      "base"       
#> rpart        "4.1.24"     "recommended"
#> spatial      "7.3-18"     "recommended"
#> splines      "4.5.3"      "base"       
#> stats        "4.5.3"      "base"       
#> stats4       "4.5.3"      "base"       
#> survival     "3.8-6"      "recommended"
#> tcltk        "4.5.3"      "base"       
#> tools        "4.5.3"      "base"       
#> utils        "4.5.3"      "base"       
#>              Depends                                                                    
#> DT           NA                                                                         
#> Formula      "R (>= 2.0.0), stats"                                                      
#> GDPuc        "R (>= 2.10)"                                                              
#> GPArotation  "R (>= 2.0.0)"                                                             
#> Hmisc        "R (>= 4.2.0)"                                                             
#> R.cache      "R (>= 2.14.0)"                                                            
#> R.methodsS3  "R (>= 2.13.0)"                                                            
#> R.oo         "R (>= 2.13.0), R.methodsS3 (>= 1.8.2)"                                    
#> R.utils      "R (>= 2.14.0), R.oo"                                                      
#> R6           "R (>= 3.6)"                                                               
#> RColorBrewer "R (>= 2.0.0)"                                                             
#> Rcpp         "R (>= 3.5.0)"                                                             
#> S7           "R (>= 3.5.0)"                                                             
#> WDI          "R (>= 3.5.0)"                                                             
#> abind        "R (>= 1.5.0)"                                                             
#> askpass      NA                                                                         
#> backports    "R (>= 3.0.0)"                                                             
#> base64enc    "R (>= 2.9.0)"                                                             
#> bit          "R (>= 3.4.0)"                                                             
#> bit64        "R (>= 3.4.0), bit (>= 4.0.0)"                                             
#> box          "R (>= 3.6.0)"                                                             
#> brew         NA                                                                         
#> brio         "R (>= 3.6)"                                                               
#> bslib        "R (>= 2.10)"                                                              
#> cachem       NA                                                                         
#> callr        "R (>= 3.4)"                                                               
#> cellranger   "R (>= 3.0.0)"                                                             
#> checkmate    "R (>= 3.0.0)"                                                             
#> citation     NA                                                                         
#> cli          "R (>= 3.4)"                                                               
#> clipr        NA                                                                         
#> collections  NA                                                                         
#> colorspace   "R (>= 3.0.0), methods"                                                    
#> commonmark   NA                                                                         
#> corpcor      "R (>= 3.0.2)"                                                             
#> countrycode  "R (>= 2.10)"                                                              
#> covr         "R (>= 3.1.0), methods"                                                    
#> crayon       NA                                                                         
#> credentials  NA                                                                         
#> crosstalk    NA                                                                         
#> curl         "R (>= 3.0.0)"                                                             
#> data.table   "R (>= 3.4.0)"                                                             
#> desc         "R (>= 3.4)"                                                               
#> devtools     "R (>= 4.1), usethis (>= 3.2.1)"                                           
#> diffobj      "R (>= 3.1.0)"                                                             
#> digest       "R (>= 3.3.0)"                                                             
#> doParallel   "R (>= 2.14.0), foreach (>= 1.2.0), iterators (>= 1.0.0),\nparallel, utils"
#> downlit      "R (>= 4.0.0)"                                                             
#> dplyr        "R (>= 4.1.0)"                                                             
#> ellipsis     "R (>= 3.2)"                                                               
#> evaluate     "R (>= 3.6.0)"                                                             
#> fansi        "R (>= 3.1.0)"                                                             
#> farver       NA                                                                         
#> fastmap      NA                                                                         
#> fdrtool      "R (>= 3.4.0)"                                                             
#> filelock     "R (>= 3.4)"                                                               
#> fontawesome  "R (>= 3.3.0)"                                                             
#> forcats      "R (>= 4.1)"                                                               
#> foreach      "R (>= 2.5.0)"                                                             
#> fs           "R (>= 4.1)"                                                               
#> gamstransfer NA                                                                         
#> gdx          "gdxrrw (>= 1.0.2), magclass (>= 2.43)"                                    
#> gdxrrw       "R (>= 3.0)"                                                               
#> generics     "R (>= 3.6)"                                                               
#> gert         NA                                                                         
#> ggplot2      "R (>= 4.1)"                                                               
#> gh           "R (>= 4.1)"                                                               
#> gitcreds     "R (>= 3.4)"                                                               
#> glasso       NA                                                                         
#> glue         "R (>= 3.6)"                                                               
#> gms          NA                                                                         
#> goxygen      NA                                                                         
#> gridExtra    NA                                                                         
#> gtable       "R (>= 4.0)"                                                               
#> gtools       "methods, stats, utils"                                                    
#> highr        "R (>= 3.3.0)"                                                             
#> hms          NA                                                                         
#> htmlTable    "R (>= 4.1)"                                                               
#> htmltools    "R (>= 2.14.1)"                                                            
#> htmlwidgets  NA                                                                         
#> httpuv       "R (>= 2.15.1)"                                                            
#> httr         "R (>= 3.6)"                                                               
#> httr2        "R (>= 4.1)"                                                               
#> igraph       "methods, R (>= 3.5.0)"                                                    
#> ini          NA                                                                         
#> isoband      NA                                                                         
#> iterators    "R (>= 2.5.0), utils"                                                      
#> jpeg         "R (>= 2.9.0)"                                                             
#> jquerylib    NA                                                                         
#> jsonlite     "methods"                                                                  
#> knitr        "R (>= 3.6.0)"                                                             
#> labeling     NA                                                                         
#> later        "R (>= 3.5)"                                                               
#> lavaan       "R(>= 3.4)"                                                                
#> lazyeval     "R (>= 3.1.0)"                                                             
#> lifecycle    "R (>= 3.6)"                                                               
#> lintr        "R (>= 4.0)"                                                               
#> lpjmlkit     "R (>= 3.5.0)"                                                             
#> lubridate    "methods, R (>= 3.2)"                                                      
#> lucode2      NA                                                                         
#> lusweave     "methods, R (>= 2.10.0)"                                                   
#> madrat       "magclass (>= 7.2.0), R (>= 2.10.0)"                                       
#> magclass     "R (>= 2.10.0), methods"                                                   
#> magrittr     "R (>= 3.4.0)"                                                             
#> memoise      NA                                                                         
#> mime         NA                                                                         
#> miniUI       NA                                                                         
#> mnormt       "R (>= 3.0.0)"                                                             
#> ncdf4        NA                                                                         
#> numDeriv     "R (>= 2.11.1)"                                                            
#> openssl      NA                                                                         
#> otel         "R (>= 3.6.0)"                                                             
#> pak          "R (>= 3.5)"                                                               
#> pander       "R (>= 2.15.0)"                                                            
#> pbapply      "R (>= 3.2.0)"                                                             
#> pbivnorm     NA                                                                         
#> pillar       NA                                                                         
#> pkgbuild     "R (>= 3.5)"                                                               
#> pkgconfig    NA                                                                         
#> pkgdown      "R (>= 4.1)"                                                               
#> pkgload      "R (>= 3.4.0)"                                                             
#> plyr         "R (>= 3.1.0)"                                                             
#> png          "R (>= 2.9.0)"                                                             
#> poorman      "R (>= 3.3)"                                                               
#> praise       NA                                                                         
#> prettyunits  "R(>= 2.10)"                                                               
#> processx     "R (>= 3.4.0)"                                                             
#> profvis      "R (>= 4.0)"                                                               
#> promises     "R (>= 4.1.0)"                                                             
#> ps           "R (>= 3.4)"                                                               
#> psych        NA                                                                         
#> purrr        "R (>= 4.1)"                                                               
#> qgraph       "R (>= 3.0.0)"                                                             
#> quadprog     "R (>= 3.1.0)"                                                             
#> quitte       "R (>= 4.1.0)"                                                             
#> ragg         NA                                                                         
#> rappdirs     "R (>= 4.1)"                                                               
#> raster       "sp (>= 1.4-5), R (>= 3.5.0)"                                              
#> rcmdcheck    NA                                                                         
#> readr        "R (>= 4.1)"                                                               
#> readxl       "R (>= 3.6)"                                                               
#> rematch      NA                                                                         
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
#> sp           "R (>= 3.5.0), methods"                                                    
#> stringi      "R (>= 3.4)"                                                               
#> stringr      "R (>= 4.1.0)"                                                             
#> styler       "R (>= 4.0.0)"                                                             
#> sys          NA                                                                         
#> systemfonts  "R (>= 3.2.0)"                                                             
#> terra        "R (>= 3.5.0), methods"                                                    
#> testthat     "R (>= 4.1.0)"                                                             
#> textshaping  "R (>= 3.2.0)"                                                             
#> tibble       "R (>= 3.4.0)"                                                             
#> tidyr        "R (>= 4.1.0)"                                                             
#> tidyselect   "R (>= 3.4)"                                                               
#> tidytemplate NA                                                                         
#> timechange   "R (>= 3.3)"                                                               
#> tinytex      NA                                                                         
#> tzdb         "R (>= 4.0.0)"                                                             
#> urlchecker   "R (>= 3.3)"                                                               
#> usethis      "R (>= 4.1)"                                                               
#> utf8         "R (>= 2.10)"                                                              
#> vctrs        "R (>= 4.0.0)"                                                             
#> viridisLite  "R (>= 2.10)"                                                              
#> vroom        "R (>= 4.1)"                                                               
#> waldo        "R (>= 4.0)"                                                               
#> whisker      NA                                                                         
#> withr        "R (>= 3.6.0)"                                                             
#> writexl      NA                                                                         
#> xfun         "R (>= 3.2.0)"                                                             
#> xml2         "R (>= 3.6.0)"                                                             
#> xmlparsedata "R (>= 3.0.0)"                                                             
#> xopen        "R (>= 3.1)"                                                               
#> xtable       "R (>= 2.10.0)"                                                            
#> yaml         NA                                                                         
#> zip          NA                                                                         
#> zoo          "R (>= 3.1.0), stats"                                                      
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
#> DT           "crosstalk, htmltools (>= 0.3.6), htmlwidgets (>= 1.3),\njquerylib, jsonlite (>= 0.9.16), magrittr, promises"                                                                                                                                                                                                                                                                                                     
#> Formula      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> GDPuc        "cli (>= 2.4.0), crayon, dplyr, glue, magrittr, rlang (>=\n1.0.0), tibble, tidyr, tidyselect, withr"                                                                                                                                                                                                                                                                                                              
#> GPArotation  "stats"                                                                                                                                                                                                                                                                                                                                                                                                           
#> Hmisc        "methods, ggplot2, cluster, rpart, nnet, foreign, gtable, grid,\ngridExtra, data.table, htmlTable (>= 1.11.0), viridisLite,\nhtmltools, base64enc, colorspace, rmarkdown, knitr, Formula"                                                                                                                                                                                                                         
#> R.cache      "utils, R.methodsS3 (>= 1.8.1), R.oo (>= 1.24.0), R.utils (>=\n2.10.1), digest (>= 0.6.13)"                                                                                                                                                                                                                                                                                                                       
#> R.methodsS3  "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> R.oo         "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> R.utils      "methods, utils, tools, R.methodsS3"                                                                                                                                                                                                                                                                                                                                                                              
#> R6           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> RColorBrewer NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> Rcpp         "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> S7           "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> WDI          "jsonlite"                                                                                                                                                                                                                                                                                                                                                                                                        
#> abind        "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> askpass      "sys (>= 2.1)"                                                                                                                                                                                                                                                                                                                                                                                                    
#> backports    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> base64enc    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bit          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bit64        "graphics, methods, stats, utils"                                                                                                                                                                                                                                                                                                                                                                                 
#> box          "tools"                                                                                                                                                                                                                                                                                                                                                                                                           
#> brew         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> brio         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bslib        "base64enc, cachem, fastmap (>= 1.1.1), grDevices, htmltools\n(>= 0.5.8), jquerylib (>= 0.1.3), jsonlite, lifecycle, memoise\n(>= 2.0.1), mime, rlang, sass (>= 0.4.9)"                                                                                                                                                                                                                                           
#> cachem       "rlang, fastmap (>= 1.2.0)"                                                                                                                                                                                                                                                                                                                                                                                       
#> callr        "processx (>= 3.6.1), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                  
#> cellranger   "rematch, tibble"                                                                                                                                                                                                                                                                                                                                                                                                 
#> checkmate    "backports (>= 1.1.0), utils"                                                                                                                                                                                                                                                                                                                                                                                     
#> citation     "desc, jsonlite, utils, withr, yaml"                                                                                                                                                                                                                                                                                                                                                                              
#> cli          "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> clipr        "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> collections  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> colorspace   "graphics, grDevices, stats"                                                                                                                                                                                                                                                                                                                                                                                      
#> commonmark   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> corpcor      "stats"                                                                                                                                                                                                                                                                                                                                                                                                           
#> countrycode  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> covr         "digest, stats, utils, jsonlite, rex, httr, cli, withr (>=\n1.0.2), yaml"                                                                                                                                                                                                                                                                                                                                         
#> crayon       "grDevices, methods, utils"                                                                                                                                                                                                                                                                                                                                                                                       
#> credentials  "openssl (>= 1.3), sys (>= 2.1), curl, jsonlite, askpass"                                                                                                                                                                                                                                                                                                                                                         
#> crosstalk    "htmltools (>= 0.3.6), jsonlite, lazyeval, R6"                                                                                                                                                                                                                                                                                                                                                                    
#> curl         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> data.table   "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> desc         "cli, R6, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> devtools     "cli (>= 3.6.5), desc (>= 1.4.3), ellipsis (>= 0.3.2), fs (>=\n1.6.7), lifecycle (>= 1.0.5), memoise (>= 2.0.1), miniUI (>=\n0.1.2), pak (>= 0.9.2), pkgbuild (>= 1.4.8), pkgdown (>=\n2.2.0), pkgload (>= 1.5.0), profvis (>= 0.4.0), rcmdcheck (>=\n1.4.0), rlang (>= 1.1.7), roxygen2 (>= 7.3.3), rversions (>=\n3.0.0), sessioninfo (>= 1.2.3), testthat (>= 3.3.2), urlchecker\n(>= 1.0.1), withr (>= 3.0.2)"
#> diffobj      "crayon (>= 1.3.2), tools, methods, utils, stats"                                                                                                                                                                                                                                                                                                                                                                 
#> digest       "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> doParallel   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> downlit      "brio, desc, digest, evaluate, fansi, memoise, rlang, vctrs,\nwithr, yaml"                                                                                                                                                                                                                                                                                                                                        
#> dplyr        "cli (>= 3.6.2), generics, glue (>= 1.3.2), lifecycle (>=\n1.0.5), magrittr (>= 1.5), methods, pillar (>= 1.9.0), R6,\nrlang (>= 1.1.7), tibble (>= 3.2.0), tidyselect (>= 1.2.0),\nutils, vctrs (>= 0.7.1)"                                                                                                                                                                                                      
#> ellipsis     "rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                                                                
#> evaluate     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fansi        "grDevices, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> farver       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fastmap      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fdrtool      "graphics, grDevices, stats"                                                                                                                                                                                                                                                                                                                                                                                      
#> filelock     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fontawesome  "rlang (>= 1.0.6), htmltools (>= 0.5.1.1)"                                                                                                                                                                                                                                                                                                                                                                        
#> forcats      "cli (>= 3.4.0), glue, lifecycle, magrittr, rlang (>= 1.0.0),\ntibble"                                                                                                                                                                                                                                                                                                                                            
#> foreach      "codetools, utils, iterators"                                                                                                                                                                                                                                                                                                                                                                                     
#> fs           "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gamstransfer "Rcpp (>= 1.0.6), R6 (>= 2.5.1), R.utils (>= 2.11.0),\ncollections(>= 0.3.6)"                                                                                                                                                                                                                                                                                                                                     
#> gdx          "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> gdxrrw       "reshape2"                                                                                                                                                                                                                                                                                                                                                                                                        
#> generics     "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gert         "askpass, credentials (>= 1.2.1), openssl (>= 2.0.3),\nrstudioapi (>= 0.11), sys, zip (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                 
#> ggplot2      "cli, grDevices, grid, gtable (>= 0.3.6), isoband, lifecycle (>\n1.0.1), rlang (>= 1.1.0), S7, scales (>= 1.4.0), stats, vctrs\n(>= 0.6.0), withr (>= 2.5.0)"                                                                                                                                                                                                                                                     
#> gh           "cli (>= 3.0.1), gitcreds, glue, httr2 (>= 1.0.6), ini,\njsonlite, lifecycle, rlang (>= 1.0.0)"                                                                                                                                                                                                                                                                                                                   
#> gitcreds     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> glasso       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> glue         "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gms          "dplyr, rlang, stringr, withr, yaml, filelock, stats"                                                                                                                                                                                                                                                                                                                                                             
#> goxygen      "citation, gms (>= 0.26.3), pander, stringi, withr, yaml"                                                                                                                                                                                                                                                                                                                                                         
#> gridExtra    "gtable, grid, grDevices, graphics, utils"                                                                                                                                                                                                                                                                                                                                                                        
#> gtable       "cli, glue, grid, lifecycle, rlang (>= 1.1.0), stats"                                                                                                                                                                                                                                                                                                                                                             
#> gtools       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> highr        "xfun (>= 0.18)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> hms          "cli, lifecycle, methods, pkgconfig, rlang (>= 1.0.2), vctrs\n(>= 0.3.8)"                                                                                                                                                                                                                                                                                                                                         
#> htmlTable    "stringr, knitr (>= 1.6), magrittr (>= 1.5), methods,\ncheckmate, htmlwidgets, htmltools, rstudioapi (>= 0.6)"                                                                                                                                                                                                                                                                                                    
#> htmltools    "base64enc, digest, fastmap (>= 1.1.0), grDevices, rlang (>=\n1.0.0), utils"                                                                                                                                                                                                                                                                                                                                      
#> htmlwidgets  "grDevices, htmltools (>= 0.5.7), jsonlite (>= 0.9.16), knitr\n(>= 1.8), rmarkdown, yaml"                                                                                                                                                                                                                                                                                                                         
#> httpuv       "later (>= 0.8.0), promises, R6, Rcpp (>= 1.0.7), utils"                                                                                                                                                                                                                                                                                                                                                          
#> httr         "curl (>= 5.1.0), jsonlite, mime, openssl (>= 0.8), R6"                                                                                                                                                                                                                                                                                                                                                           
#> httr2        "cli (>= 3.0.0), curl (>= 6.4.0), glue, lifecycle, magrittr,\nopenssl, R6, rappdirs, rlang (>= 1.1.0), vctrs (>= 0.6.3),\nwithr"                                                                                                                                                                                                                                                                                  
#> igraph       "cli, graphics, grDevices, lifecycle, magrittr, Matrix,\npkgconfig (>= 2.0.0), rlang (>= 1.1.0), stats, utils, vctrs"                                                                                                                                                                                                                                                                                             
#> ini          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> isoband      "cli, grid, rlang, utils"                                                                                                                                                                                                                                                                                                                                                                                         
#> iterators    NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> jpeg         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> jquerylib    "htmltools"                                                                                                                                                                                                                                                                                                                                                                                                       
#> jsonlite     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> knitr        "evaluate (>= 0.15), highr (>= 0.11), methods, tools, xfun (>=\n0.52), yaml (>= 2.1.19)"                                                                                                                                                                                                                                                                                                                          
#> labeling     "stats, graphics"                                                                                                                                                                                                                                                                                                                                                                                                 
#> later        "Rcpp (>= 1.0.10), rlang"                                                                                                                                                                                                                                                                                                                                                                                         
#> lavaan       "methods, stats4, stats, utils, graphics, MASS, mnormt,\npbivnorm, numDeriv, quadprog"                                                                                                                                                                                                                                                                                                                            
#> lazyeval     "rlang"                                                                                                                                                                                                                                                                                                                                                                                                           
#> lifecycle    "cli (>= 3.4.0), rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                
#> lintr        "backports (>= 1.5.0), cli (>= 3.4.0), codetools, digest, glue,\nknitr, rex, stats, utils, xfun, xml2 (>= 1.0.0), xmlparsedata\n(>= 1.0.5)"                                                                                                                                                                                                                                                                       
#> lpjmlkit     "magrittr, dplyr, processx, tibble, jsonlite, doParallel,\nforeach, utils, methods, abind, rlang, withr, grDevices, cli,\nstringi"                                                                                                                                                                                                                                                                                
#> lubridate    "generics, timechange (>= 0.4.0)"                                                                                                                                                                                                                                                                                                                                                                                 
#> lucode2      "callr, citation (>= 0.11.3), data.table, desc, devtools,\ndplyr, lintr (>= 3.1.0), pak, rlang, tools, usethis (>= 2.1.0),\nwithr, yaml"                                                                                                                                                                                                                                                                          
#> lusweave     "knitr (>= 1.38), withr, xtable"                                                                                                                                                                                                                                                                                                                                                                                  
#> madrat       "callr, digest, filelock (>= 1.0.3), igraph (>= 2.1.1), Matrix,\nmethods, pkgload, renv, stringi, tools, utils, withr, yaml"                                                                                                                                                                                                                                                                                      
#> magclass     "abind, data.table, stats"                                                                                                                                                                                                                                                                                                                                                                                        
#> magrittr     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> memoise      "rlang (>= 0.4.10), cachem"                                                                                                                                                                                                                                                                                                                                                                                       
#> mime         "tools"                                                                                                                                                                                                                                                                                                                                                                                                           
#> miniUI       "shiny (>= 0.13), htmltools (>= 0.3), utils"                                                                                                                                                                                                                                                                                                                                                                      
#> mnormt       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> ncdf4        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> numDeriv     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> openssl      "askpass"                                                                                                                                                                                                                                                                                                                                                                                                         
#> otel         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> pak          "tools, utils"                                                                                                                                                                                                                                                                                                                                                                                                    
#> pander       "grDevices, graphics, methods, utils, stats, digest, tools,\nRcpp"                                                                                                                                                                                                                                                                                                                                                
#> pbapply      "parallel"                                                                                                                                                                                                                                                                                                                                                                                                        
#> pbivnorm     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> pillar       "cli (>= 2.3.0), glue, lifecycle, rlang (>= 1.0.2), utf8 (>=\n1.1.0), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                                                    
#> pkgbuild     "callr (>= 3.2.0), cli (>= 3.4.0), desc, processx, R6"                                                                                                                                                                                                                                                                                                                                                            
#> pkgconfig    "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> pkgdown      "bslib (>= 0.5.1), callr (>= 3.7.3), cli (>= 3.6.1), desc (>=\n1.4.0), downlit (>= 0.4.4), fontawesome, fs (>= 1.4.0), httr2\n(>= 1.0.2), jsonlite, lifecycle, openssl, purrr (>= 1.0.0),\nragg (>= 1.4.0), rlang (>= 1.1.4), rmarkdown (>= 2.27), tibble,\nwhisker, withr (>= 2.4.3), xml2 (>= 1.3.1), yaml (>= 2.3.9)"                                                                                          
#> pkgload      "cli (>= 3.3.0), desc, fs, glue, lifecycle, methods, pkgbuild,\nprocessx, rlang (>= 1.1.1), rprojroot, utils"                                                                                                                                                                                                                                                                                                     
#> plyr         "Rcpp (>= 0.11.0)"                                                                                                                                                                                                                                                                                                                                                                                                
#> png          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> poorman      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> praise       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> prettyunits  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> processx     "ps (>= 1.2.0), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                        
#> profvis      "htmlwidgets (>= 0.3.2), rlang (>= 1.1.0), vctrs"                                                                                                                                                                                                                                                                                                                                                                 
#> promises     "fastmap (>= 1.1.0), later, lifecycle, magrittr (>= 1.5), otel\n(>= 0.2.0), R6, rlang"                                                                                                                                                                                                                                                                                                                            
#> ps           "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> psych        "mnormt,parallel,stats,graphics,grDevices,methods,lattice,nlme,GPArotation"                                                                                                                                                                                                                                                                                                                                       
#> purrr        "cli (>= 3.6.1), lifecycle (>= 1.0.3), magrittr (>= 1.5.0),\nrlang (>= 1.1.1), vctrs (>= 0.6.3)"                                                                                                                                                                                                                                                                                                                  
#> qgraph       "Rcpp (>= 1.0.0), methods, grDevices, psych, lavaan, plyr,\nHmisc, igraph, jpeg, png, colorspace, Matrix, corpcor,\nreshape2, ggplot2, glasso, fdrtool, gtools, parallel, pbapply,\nabind"                                                                                                                                                                                                                        
#> quadprog     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> quitte       "cli, countrycode, dplyr (>= 1.1.1), forcats (>= 1.0.0),\ngamstransfer, ggplot2 (>= 4.0.0), gms (>= 0.17.0), glue,\nlazyeval, lifecycle, lubridate, magclass, magrittr, methods,\nplyr, purrr, readr, readxl, reshape2, rlang, stats, stringr,\ntibble, tidyr, tidyselect, writexl, zoo,"                                                                                                                         
#> ragg         "systemfonts (>= 1.0.3), textshaping (>= 0.3.0)"                                                                                                                                                                                                                                                                                                                                                                  
#> rappdirs     NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> raster       "Rcpp, methods, terra (>= 1.8-5)"                                                                                                                                                                                                                                                                                                                                                                                 
#> rcmdcheck    "callr (>= 3.1.1.9000), cli (>= 3.0.0), curl, desc (>= 1.2.0),\ndigest, pkgbuild, prettyunits, R6, rprojroot, sessioninfo (>=\n1.1.1), utils, withr, xopen"                                                                                                                                                                                                                                                       
#> readr        "cli, clipr, crayon, glue, hms (>= 0.4.1), lifecycle, methods,\nR6, rlang, tibble, utils, vroom (>= 1.7.0), withr"                                                                                                                                                                                                                                                                                                
#> readxl       "cellranger, tibble (>= 2.0.1), utils"                                                                                                                                                                                                                                                                                                                                                                            
#> rematch      NA                                                                                                                                                                                                                                                                                                                                                                                                                
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
#> sp           "utils, stats, graphics, grDevices, lattice, grid"                                                                                                                                                                                                                                                                                                                                                                
#> stringi      "tools, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                             
#> stringr      "cli, glue (>= 1.6.1), lifecycle (>= 1.0.3), magrittr, rlang\n(>= 1.0.0), stringi (>= 1.5.3), vctrs (>= 0.4.0)"                                                                                                                                                                                                                                                                                                   
#> styler       "cli (>= 3.1.1), magrittr (>= 2.0.0), purrr (>= 1.0.2), R.cache\n(>= 0.15.0), rlang (>= 1.0.0), rprojroot (>= 1.1), tools, vctrs\n(>= 0.4.1), withr (>= 2.3.0),"                                                                                                                                                                                                                                                  
#> sys          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> systemfonts  "base64enc, grid, jsonlite, lifecycle, tools, utils"                                                                                                                                                                                                                                                                                                                                                              
#> terra        "Rcpp (>= 1.0-10)"                                                                                                                                                                                                                                                                                                                                                                                                
#> testthat     "brio (>= 1.1.5), callr (>= 3.7.6), cli (>= 3.6.5), desc (>=\n1.4.3), evaluate (>= 1.0.4), jsonlite (>= 2.0.0), lifecycle (>=\n1.0.4), magrittr (>= 2.0.3), methods, pkgload (>= 1.4.0),\npraise (>= 1.0.0), processx (>= 3.8.6), ps (>= 1.9.1), R6 (>=\n2.6.1), rlang (>= 1.1.6), utils, waldo (>= 0.6.2), withr (>=\n3.0.2)"                                                                                    
#> textshaping  "lifecycle, stats, stringi, systemfonts (>= 1.3.0), utils"                                                                                                                                                                                                                                                                                                                                                        
#> tibble       "cli, lifecycle (>= 1.0.0), magrittr, methods, pillar (>=\n1.8.1), pkgconfig, rlang (>= 1.0.2), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                          
#> tidyr        "cli (>= 3.4.1), dplyr (>= 1.1.0), glue, lifecycle (>= 1.0.3),\nmagrittr, purrr (>= 1.0.1), rlang (>= 1.1.1), stringr (>=\n1.5.0), tibble (>= 2.1.1), tidyselect (>= 1.2.1), utils, vctrs\n(>= 0.5.2)"                                                                                                                                                                                                            
#> tidyselect   "cli (>= 3.3.0), glue (>= 1.3.0), lifecycle (>= 1.0.3), rlang\n(>= 1.0.4), vctrs (>= 0.5.2), withr"                                                                                                                                                                                                                                                                                                               
#> tidytemplate NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> timechange   NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> tinytex      "xfun (>= 0.48)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> tzdb         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> urlchecker   "cli, curl, tools, xml2"                                                                                                                                                                                                                                                                                                                                                                                          
#> usethis      "cli (>= 3.0.1), clipr (>= 0.3.0), crayon, curl (>= 2.7), desc\n(>= 1.4.2), fs (>= 1.3.0), gert (>= 1.4.1), gh (>= 1.2.1), glue\n(>= 1.3.0), jsonlite, lifecycle (>= 1.0.0), purrr, rappdirs,\nrlang (>= 1.1.0), rprojroot (>= 2.1.1), rstudioapi, stats,\ntools, utils, whisker, withr (>= 2.3.0), yaml"                                                                                                         
#> utf8         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> vctrs        "cli (>= 3.4.0), glue, lifecycle (>= 1.0.3), rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                    
#> viridisLite  NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> vroom        "bit64, cli (>= 3.2.0), crayon, glue, hms, lifecycle (>=\n1.0.3), methods, rlang (>= 1.1.0), stats, tibble (>= 2.0.0),\ntidyselect, tzdb (>= 0.1.1), vctrs (>= 0.2.0), withr"                                                                                                                                                                                                                                     
#> waldo        "cli, diffobj (>= 0.3.4), glue, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                        
#> whisker      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> withr        "graphics, grDevices"                                                                                                                                                                                                                                                                                                                                                                                             
#> writexl      NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> xfun         "grDevices, stats, tools"                                                                                                                                                                                                                                                                                                                                                                                         
#> xml2         "cli, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                  
#> xmlparsedata NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> xopen        "processx"                                                                                                                                                                                                                                                                                                                                                                                                        
#> xtable       "stats, utils, methods"                                                                                                                                                                                                                                                                                                                                                                                           
#> yaml         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> zip          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> zoo          "utils, graphics, grDevices, lattice (>= 0.20-27)"                                                                                                                                                                                                                                                                                                                                                                
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
#> DT           NA                                                      
#> Formula      NA                                                      
#> GDPuc        NA                                                      
#> GPArotation  NA                                                      
#> Hmisc        NA                                                      
#> R.cache      NA                                                      
#> R.methodsS3  NA                                                      
#> R.oo         NA                                                      
#> R.utils      NA                                                      
#> R6           NA                                                      
#> RColorBrewer NA                                                      
#> Rcpp         NA                                                      
#> S7           NA                                                      
#> WDI          NA                                                      
#> abind        NA                                                      
#> askpass      NA                                                      
#> backports    NA                                                      
#> base64enc    NA                                                      
#> bit          NA                                                      
#> bit64        NA                                                      
#> box          NA                                                      
#> brew         NA                                                      
#> brio         NA                                                      
#> bslib        NA                                                      
#> cachem       NA                                                      
#> callr        NA                                                      
#> cellranger   NA                                                      
#> checkmate    NA                                                      
#> citation     NA                                                      
#> cli          NA                                                      
#> clipr        NA                                                      
#> collections  NA                                                      
#> colorspace   NA                                                      
#> commonmark   NA                                                      
#> corpcor      NA                                                      
#> countrycode  NA                                                      
#> covr         NA                                                      
#> crayon       NA                                                      
#> credentials  NA                                                      
#> crosstalk    NA                                                      
#> curl         NA                                                      
#> data.table   NA                                                      
#> desc         NA                                                      
#> devtools     NA                                                      
#> diffobj      NA                                                      
#> digest       NA                                                      
#> doParallel   NA                                                      
#> downlit      NA                                                      
#> dplyr        NA                                                      
#> ellipsis     NA                                                      
#> evaluate     NA                                                      
#> fansi        NA                                                      
#> farver       NA                                                      
#> fastmap      NA                                                      
#> fdrtool      NA                                                      
#> filelock     NA                                                      
#> fontawesome  NA                                                      
#> forcats      NA                                                      
#> foreach      NA                                                      
#> fs           NA                                                      
#> gamstransfer "Rcpp"                                                  
#> gdx          NA                                                      
#> gdxrrw       NA                                                      
#> generics     NA                                                      
#> gert         NA                                                      
#> ggplot2      NA                                                      
#> gh           NA                                                      
#> gitcreds     NA                                                      
#> glasso       NA                                                      
#> glue         NA                                                      
#> gms          NA                                                      
#> goxygen      NA                                                      
#> gridExtra    NA                                                      
#> gtable       NA                                                      
#> gtools       NA                                                      
#> highr        NA                                                      
#> hms          NA                                                      
#> htmlTable    NA                                                      
#> htmltools    NA                                                      
#> htmlwidgets  NA                                                      
#> httpuv       "later, Rcpp"                                           
#> httr         NA                                                      
#> httr2        NA                                                      
#> igraph       "cpp11 (>= 0.5.0)"                                      
#> ini          NA                                                      
#> isoband      "cpp11"                                                 
#> iterators    NA                                                      
#> jpeg         NA                                                      
#> jquerylib    NA                                                      
#> jsonlite     NA                                                      
#> knitr        NA                                                      
#> labeling     NA                                                      
#> later        "Rcpp"                                                  
#> lavaan       NA                                                      
#> lazyeval     NA                                                      
#> lifecycle    NA                                                      
#> lintr        NA                                                      
#> lpjmlkit     NA                                                      
#> lubridate    NA                                                      
#> lucode2      NA                                                      
#> lusweave     NA                                                      
#> madrat       NA                                                      
#> magclass     NA                                                      
#> magrittr     NA                                                      
#> memoise      NA                                                      
#> mime         NA                                                      
#> miniUI       NA                                                      
#> mnormt       NA                                                      
#> ncdf4        NA                                                      
#> numDeriv     NA                                                      
#> openssl      NA                                                      
#> otel         NA                                                      
#> pak          NA                                                      
#> pander       "Rcpp"                                                  
#> pbapply      NA                                                      
#> pbivnorm     NA                                                      
#> pillar       NA                                                      
#> pkgbuild     NA                                                      
#> pkgconfig    NA                                                      
#> pkgdown      NA                                                      
#> pkgload      NA                                                      
#> plyr         "Rcpp"                                                  
#> png          NA                                                      
#> poorman      NA                                                      
#> praise       NA                                                      
#> prettyunits  NA                                                      
#> processx     NA                                                      
#> profvis      NA                                                      
#> promises     NA                                                      
#> ps           NA                                                      
#> psych        NA                                                      
#> purrr        "cli"                                                   
#> qgraph       "Rcpp"                                                  
#> quadprog     NA                                                      
#> quitte       NA                                                      
#> ragg         "systemfonts, textshaping"                              
#> rappdirs     NA                                                      
#> raster       "Rcpp"                                                  
#> rcmdcheck    NA                                                      
#> readr        "cpp11, tzdb (>= 0.1.1)"                                
#> readxl       "cpp11 (>= 0.4.0), progress"                            
#> rematch      NA                                                      
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
#> sp           NA                                                      
#> stringi      NA                                                      
#> stringr      NA                                                      
#> styler       NA                                                      
#> sys          NA                                                      
#> systemfonts  "cpp11 (>= 0.2.1)"                                      
#> terra        "Rcpp"                                                  
#> testthat     NA                                                      
#> textshaping  "cpp11 (>= 0.2.1), systemfonts (>= 1.0.0)"              
#> tibble       NA                                                      
#> tidyr        "cpp11 (>= 0.4.0)"                                      
#> tidyselect   NA                                                      
#> tidytemplate NA                                                      
#> timechange   "cpp11 (>= 0.2.7)"                                      
#> tinytex      NA                                                      
#> tzdb         "cpp11 (>= 0.5.2)"                                      
#> urlchecker   NA                                                      
#> usethis      NA                                                      
#> utf8         NA                                                      
#> vctrs        NA                                                      
#> viridisLite  NA                                                      
#> vroom        "cpp11 (>= 0.2.0), progress (>= 1.2.3), tzdb (>= 0.1.1)"
#> waldo        NA                                                      
#> whisker      NA                                                      
#> withr        NA                                                      
#> writexl      NA                                                      
#> xfun         NA                                                      
#> xml2         NA                                                      
#> xmlparsedata NA                                                      
#> xopen        NA                                                      
#> xtable       NA                                                      
#> yaml         NA                                                      
#> zip          NA                                                      
#> zoo          NA                                                      
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
#> DT           "bslib, future, httpuv, knitr (>= 1.8), rmarkdown, shiny (>=\n1.6), testit, tibble"                                                                                                                                                                                                                                                                                                       
#> Formula      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> GDPuc        "countrycode, covr, knitr, magclass, madrat (>= 3.6.4), purrr,\nrmarkdown, stringr, testthat (>= 3.0.0), usethis, WDI, zoo"                                                                                                                                                                                                                                                               
#> GPArotation  NA                                                                                                                                                                                                                                                                                                                                                                                        
#> Hmisc        "survival, qreport, acepack, chron, rms, mice, rstudioapi,\ntables, plotly (>= 4.5.6), rlang, VGAM, leaps, pcaPP, digest,\nparallel, polspline, abind, kableExtra, rio, lattice,\nlatticeExtra, gt, sparkline, jsonlite, htmlwidgets, qs,\ngetPass, keyring, safer, htm2txt, boot"                                                                                                        
#> R.cache      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> R.methodsS3  "codetools"                                                                                                                                                                                                                                                                                                                                                                               
#> R.oo         "tools"                                                                                                                                                                                                                                                                                                                                                                                   
#> R.utils      "datasets, digest (>= 0.6.10)"                                                                                                                                                                                                                                                                                                                                                            
#> R6           "lobstr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                             
#> RColorBrewer NA                                                                                                                                                                                                                                                                                                                                                                                        
#> Rcpp         "tinytest, inline, rbenchmark, pkgKitten (>= 0.1.2)"                                                                                                                                                                                                                                                                                                                                      
#> S7           "bench, callr, covr, knitr, methods, rmarkdown, testthat (>=\n3.2.0), tibble"                                                                                                                                                                                                                                                                                                             
#> WDI          "altdoc, curl, testthat, tidyr (>= 0.8.0)"                                                                                                                                                                                                                                                                                                                                                
#> abind        NA                                                                                                                                                                                                                                                                                                                                                                                        
#> askpass      "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> backports    NA                                                                                                                                                                                                                                                                                                                                                                                        
#> base64enc    NA                                                                                                                                                                                                                                                                                                                                                                                        
#> bit          "testthat (>= 3.0.0), roxygen2, knitr, markdown, rmarkdown,\nmicrobenchmark, bit64 (>= 4.0.0), ff (>= 4.0.0)"                                                                                                                                                                                                                                                                             
#> bit64        "testthat (>= 3.0.3), withr"                                                                                                                                                                                                                                                                                                                                                              
#> box          "devtools, knitr (>= 1.40), rmarkdown, R6, rlang, roxygen2 (>=\n7.2.1), shiny, stringr, testthat (>= 3.1.7)"                                                                                                                                                                                                                                                                              
#> brew         "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                     
#> brio         "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> bslib        "brand.yml, bsicons, curl, fontawesome, future, ggplot2,\nknitr, lattice, magrittr, rappdirs, rmarkdown (>= 2.7), shiny\n(>= 1.11.1), testthat, thematic, tools, utils, withr, yaml"                                                                                                                                                                                                      
#> cachem       "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> callr        "asciicast (>= 2.3.1), cli (>= 1.1.0), mockery, ps, rprojroot,\nspelling, testthat (>= 3.2.0), withr (>= 2.3.0)"                                                                                                                                                                                                                                                                          
#> cellranger   "covr, testthat (>= 1.0.0), knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                             
#> checkmate    "R6, fastmatch, data.table (>= 1.9.8), devtools, ggplot2,\nknitr, magrittr, microbenchmark, rmarkdown, testthat (>=\n3.0.4), tinytest (>= 1.1.0), tibble"                                                                                                                                                                                                                                 
#> citation     "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                          
#> cli          "callr, covr, crayon, digest, glue (>= 1.6.0), grDevices,\nhtmltools, htmlwidgets, knitr, methods, processx, ps (>=\n1.3.4.9000), rlang (>= 1.0.2.9003), rmarkdown, rprojroot,\nrstudioapi, testthat (>= 3.2.0), tibble, whoami, withr"                                                                                                                                                   
#> clipr        "covr, knitr, rmarkdown, rstudioapi (>= 0.5), testthat (>=\n2.0.0)"                                                                                                                                                                                                                                                                                                                       
#> collections  "testthat (>= 2.3.1)"                                                                                                                                                                                                                                                                                                                                                                     
#> colorspace   "datasets, utils, KernSmooth, MASS, kernlab, mvtnorm, vcd,\ntcltk, shiny, shinyjs, ggplot2, dplyr, scales, grid, png, jpeg,\nknitr, rmarkdown, RColorBrewer, rcartocolor, scico, viridis,\nwesanderson"                                                                                                                                                                                   
#> commonmark   "curl, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                    
#> corpcor      ""                                                                                                                                                                                                                                                                                                                                                                                        
#> countrycode  "altdoc, eurostat, testthat, tibble, ISOcodes, utf8"                                                                                                                                                                                                                                                                                                                                      
#> covr         "R6, S7 (>= 0.2.0), curl, knitr, rmarkdown, htmltools, DT (>=\n0.2), testthat (>= 3.0.0), rlang, rstudioapi (>= 0.2), xml2 (>=\n1.0.0), parallel, memoise, covr, box (>= 1.2.0)"                                                                                                                                                                                                          
#> crayon       "mockery, rstudioapi, testthat, withr"                                                                                                                                                                                                                                                                                                                                                    
#> credentials  "testthat, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                              
#> crosstalk    "bslib, ggplot2, sass, shiny, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                                        
#> curl         "spelling, testthat (>= 1.0.0), knitr, jsonlite, later,\nrmarkdown, httpuv (>= 1.4.4), webutils"                                                                                                                                                                                                                                                                                          
#> data.table   "bit64 (>= 4.0.0), bit (>= 4.0.4), R.utils (>= 2.13.0), xts,\nzoo (>= 1.8-1), yaml, knitr, markdown"                                                                                                                                                                                                                                                                                      
#> desc         "callr, covr, gh, spelling, testthat, whoami, withr"                                                                                                                                                                                                                                                                                                                                      
#> devtools     "BiocManager (>= 1.30.18), callr (>= 3.7.1), covr (>= 3.5.1),\ncurl (>= 4.3.2), digest (>= 0.6.29), DT (>= 0.23), foghorn (>=\n1.4.2), gh (>= 1.3.0), httr2 (>= 1.0.0), knitr (>= 1.39), lintr\n(>= 3.0.0), quarto (>= 1.5.1), remotes (>= 2.5.0), rmarkdown\n(>= 2.14), rstudioapi (>= 0.13), spelling (>= 2.2), xml2"                                                                   
#> diffobj      "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                        
#> digest       "tinytest, simplermarkdown, rbenchmark"                                                                                                                                                                                                                                                                                                                                                   
#> doParallel   "caret, mlbench, rpart, RUnit"                                                                                                                                                                                                                                                                                                                                                            
#> downlit      "covr, htmltools, jsonlite, MASS, MassSpecWavelet, pkgload,\nrmarkdown, testthat (>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                        
#> dplyr        "broom, covr, DBI, dbplyr (>= 2.2.1), ggplot2, knitr, Lahman,\nlobstr, nycflights13, purrr, rmarkdown, RSQLite, stringi (>=\n1.7.6), testthat (>= 3.1.5), tidyr (>= 1.3.0), withr"                                                                                                                                                                                                        
#> ellipsis     "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                          
#> evaluate     "callr, covr, ggplot2 (>= 3.3.6), lattice, methods, pkgload,\nragg (>= 1.4.0), rlang (>= 1.1.5), knitr, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                      
#> fansi        "unitizer, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                              
#> farver       "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> fastmap      "testthat (>= 2.1.1)"                                                                                                                                                                                                                                                                                                                                                                     
#> fdrtool      ""                                                                                                                                                                                                                                                                                                                                                                                        
#> filelock     "callr (>= 2.0.0), covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                             
#> fontawesome  "covr, dplyr (>= 1.0.8), gt (>= 0.9.0), knitr (>= 1.31),\ntestthat (>= 3.0.0), rsvg"                                                                                                                                                                                                                                                                                                      
#> forcats      "covr, dplyr, ggplot2, knitr, readr, rmarkdown, testthat (>=\n3.0.0), withr"                                                                                                                                                                                                                                                                                                              
#> foreach      "randomForest, doMC, doParallel, testthat, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                              
#> fs           "covr, crayon, knitr, pillar (>= 1.0.0), rmarkdown, spelling,\ntestthat (>= 3.0.0), tibble (>= 1.1.0), vctrs (>= 0.3.0), withr"                                                                                                                                                                                                                                                           
#> gamstransfer "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                     
#> gdx          "covr"                                                                                                                                                                                                                                                                                                                                                                                    
#> gdxrrw       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> generics     "covr, pkgload, testthat (>= 3.0.0), tibble, withr"                                                                                                                                                                                                                                                                                                                                       
#> gert         "spelling, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                    
#> ggplot2      "broom, covr, dplyr, ggplot2movies, hexbin, Hmisc, hms, knitr,\nmapproj, maps, MASS, mgcv, multcomp, munsell, nlme, profvis,\nquantreg, quarto, ragg (>= 1.2.6), RColorBrewer, roxygen2,\nrpart, sf (>= 0.7-3), svglite (>= 2.1.2), testthat (>= 3.1.5),\ntibble, vdiffr (>= 1.0.6), xml2"                                                                                                
#> gh           "connectcreds, covr, knitr, rmarkdown, rprojroot, spelling,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                  
#> gitcreds     "codetools, covr, knitr, mockery, oskeyring, rmarkdown,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                      
#> glasso       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> glue         "crayon, DBI (>= 1.2.0), dplyr, knitr, magrittr, rlang,\nrmarkdown, RSQLite, testthat (>= 3.2.0), vctrs (>= 0.3.0),\nwaldo (>= 0.5.3), withr"                                                                                                                                                                                                                                             
#> gms          "covr, curl, magclass, qgraph, testthat, callr"                                                                                                                                                                                                                                                                                                                                           
#> goxygen      "covr, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                        
#> gridExtra    "ggplot2, egg, lattice, knitr, testthat"                                                                                                                                                                                                                                                                                                                                                  
#> gtable       "covr, ggplot2, knitr, profvis, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                           
#> gtools       "car, gplots, knitr, rstudioapi, SGP, taxize"                                                                                                                                                                                                                                                                                                                                             
#> highr        "knitr, markdown, testit"                                                                                                                                                                                                                                                                                                                                                                 
#> hms          "crayon, lubridate, pillar (>= 1.1.0), testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                               
#> htmlTable    "testthat, XML, xml2, Hmisc, rmarkdown, chron, lubridate,\ntibble, purrr, tidyselect, glue, rlang, tidyr (>= 0.7.2), dplyr\n(>= 0.7.4)"                                                                                                                                                                                                                                                   
#> htmltools    "Cairo, markdown, ragg, shiny, testthat, withr"                                                                                                                                                                                                                                                                                                                                           
#> htmlwidgets  "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> httpuv       "callr, curl, jsonlite, testthat (>= 3.0.0), websocket"                                                                                                                                                                                                                                                                                                                                   
#> httr         "covr, httpuv, jpeg, knitr, png, readr, rmarkdown, testthat\n(>= 0.8.0), xml2"                                                                                                                                                                                                                                                                                                            
#> httr2        "askpass, bench, clipr, covr, docopt, httpuv, jose, jsonlite,\nknitr, later (>= 1.4.0), nanonext, otel (>= 0.2.0), otelsdk (>=\n0.2.0), paws.common (>= 0.8.0), promises, rmarkdown, testthat\n(>= 3.1.8), tibble, webfakes (>= 1.4.0), xml2"                                                                                                                                             
#> igraph       "ape (>= 5.7-0.1), callr, decor, digest, igraphdata, knitr,\nrgl (>= 1.3.14), rmarkdown, scales, stats4, tcltk, testthat,\nvdiffr, withr"                                                                                                                                                                                                                                                 
#> ini          "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> isoband      "covr, ggplot2, knitr, magick, bench, rmarkdown, sf, testthat\n(>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                                          
#> iterators    "RUnit, foreach"                                                                                                                                                                                                                                                                                                                                                                          
#> jpeg         NA                                                                                                                                                                                                                                                                                                                                                                                        
#> jquerylib    "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> jsonlite     "httr, vctrs, testthat, knitr, rmarkdown, R.rsp, sf"                                                                                                                                                                                                                                                                                                                                      
#> knitr        "bslib, DBI (>= 0.4-1), digest, formatR, gifski, gridSVG,\nhtmlwidgets (>= 0.7), jpeg, JuliaCall (>= 0.11.1), magick,\nlitedown, markdown (>= 1.3), otel, otelsdk, png, ragg,\nreticulate (>= 1.4), rgl (>= 0.95.1201), rlang, rmarkdown,\nsass, showtext, styler (>= 1.2.0), targets (>= 0.6.0), testit,\ntibble, tikzDevice (>= 0.10), tinytex (>= 0.56), webshot,\nrstudioapi, svglite"
#> labeling     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> later        "knitr, nanonext, promises, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                               
#> lavaan       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> lazyeval     "knitr, rmarkdown (>= 0.2.65), testthat, covr"                                                                                                                                                                                                                                                                                                                                            
#> lifecycle    "covr, knitr, lintr (>= 3.1.0), rmarkdown, testthat (>=\n3.0.1), tibble, tidyverse, tools, vctrs, withr, xml2"                                                                                                                                                                                                                                                                            
#> lintr        "bookdown, cyclocomp, jsonlite, patrick (>= 0.2.0), rlang,\nrmarkdown, rstudioapi (>= 0.2), testthat (>= 3.2.1), tibble,\ntufte, withr (>= 2.5.0)"                                                                                                                                                                                                                                        
#> lpjmlkit     "rmarkdown, knitr, testthat (>= 3.0.0), terra, raster,\nreshape2, maps, sf, ncdf4, CFtime, R6"                                                                                                                                                                                                                                                                                            
#> lubridate    "covr, knitr, rmarkdown, testthat (>= 2.1.0), vctrs (>= 0.6.5)"                                                                                                                                                                                                                                                                                                                           
#> lucode2      "covr, gdx, gdxrrw, gert, ggplot2, knitr, lusweave, magclass,\npoorman, renv, rmarkdown, styler, testthat"                                                                                                                                                                                                                                                                                
#> lusweave     "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> madrat       "covr, ggplot2, graphics, grDevices, knitr, rmarkdown, terra,\ntestthat, tibble"                                                                                                                                                                                                                                                                                                          
#> magclass     "covr, lpjmlkit, knitr, ncdf4, pkgconfig, quitte, raster,\nrmarkdown, terra, testthat (>= 3.1.5), tibble, withr"                                                                                                                                                                                                                                                                          
#> magrittr     "covr, knitr, rlang, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                 
#> memoise      "digest, aws.s3, covr, googleAuthR, googleCloudStorageR, httr,\ntestthat"                                                                                                                                                                                                                                                                                                                 
#> mime         NA                                                                                                                                                                                                                                                                                                                                                                                        
#> miniUI       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> mnormt       NA                                                                                                                                                                                                                                                                                                                                                                                        
#> ncdf4        NA                                                                                                                                                                                                                                                                                                                                                                                        
#> numDeriv     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> openssl      "curl, testthat (>= 2.1.0), digest, knitr, rmarkdown,\njsonlite, jose, sodium"                                                                                                                                                                                                                                                                                                            
#> otel         "callr, cli, glue, jsonlite, otelsdk, processx, shiny,\nspelling, testthat (>= 3.0.0), utils, withr"                                                                                                                                                                                                                                                                                      
#> pak          "callr (>= 3.7.0), cli (>= 3.2.0), covr, curl (>= 4.3.2), desc\n(>= 1.4.1), filelock (>= 1.0.2), gitcreds, glue (>= 1.6.2),\njsonlite (>= 1.8.0), keyring (>= 1.4.0), pingr, pkgbuild (>=\n1.4.2), pkgcache (>= 2.2.4), pkgdepends (>= 0.9.0), pkgload,\npkgsearch (>= 3.1.0), processx (>= 3.8.1), ps (>= 1.6.0),\nrstudioapi, testthat (>= 3.2.0), webfakes, withr, yaml"               
#> pander       "grid, lattice, ggplot2 (>= 0.9.2), sylly, sylly.en, logger,\nsurvival, microbenchmark, zoo, nlme, descr, MASS, knitr,\nrmarkdown, tables, reshape, memisc, Epi, randomForest, tseries,\ngtable, rms, forecast, data.table"                                                                                                                                                               
#> pbapply      "shiny, future, future.apply"                                                                                                                                                                                                                                                                                                                                                             
#> pbivnorm     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> pillar       "bit64, DBI, debugme, DiagrammeR, dplyr, formattable, ggplot2,\nknitr, lubridate, nanotime, nycflights13, palmerpenguins,\nrmarkdown, scales, stringi, survival, testthat (>= 3.1.1),\ntibble, units (>= 0.7.2), vdiffr, withr"                                                                                                                                                           
#> pkgbuild     "covr, cpp11, knitr, Rcpp, rmarkdown, testthat (>= 3.2.0),\nwithr (>= 2.3.0)"                                                                                                                                                                                                                                                                                                             
#> pkgconfig    "covr, testthat, disposables (>= 1.0.3)"                                                                                                                                                                                                                                                                                                                                                  
#> pkgdown      "covr, diffviewer, evaluate (>= 0.24.0), gert, gt, htmltools,\nhtmlwidgets, knitr (>= 1.50), magick, methods, pkgload (>=\n1.0.2), quarto, rsconnect, rstudioapi, rticles, sass, testthat\n(>= 3.1.3), tools"                                                                                                                                                                             
#> pkgload      "bitops, jsonlite, mathjaxr, pak, Rcpp, remotes, rstudioapi,\ntestthat (>= 3.2.1.1), usethis, withr"                                                                                                                                                                                                                                                                                      
#> plyr         "abind, covr, doParallel, foreach, iterators, itertools,\ntcltk, testthat"                                                                                                                                                                                                                                                                                                                
#> png          NA                                                                                                                                                                                                                                                                                                                                                                                        
#> poorman      "knitr, rmarkdown, roxygen2, tinytest"                                                                                                                                                                                                                                                                                                                                                    
#> praise       "testthat"                                                                                                                                                                                                                                                                                                                                                                                
#> prettyunits  "codetools, covr, testthat"                                                                                                                                                                                                                                                                                                                                                               
#> processx     "callr (>= 3.7.3), cli (>= 3.3.0), codetools, covr, curl,\ndebugme, parallel, rlang (>= 1.0.2), testthat (>= 3.0.0),\nwebfakes, withr"                                                                                                                                                                                                                                                    
#> profvis      "htmltools, knitr, rmarkdown, shiny, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                 
#> promises     "future (>= 1.21.0), knitr, mirai, otelsdk (>= 0.2.0), purrr,\nRcpp, rmarkdown, spelling, testthat (>= 3.0.0), vembedr"                                                                                                                                                                                                                                                                   
#> ps           "callr, covr, curl, pillar, pingr, processx (>= 3.1.0), R6,\nrlang, testthat (>= 3.0.0), webfakes, withr"                                                                                                                                                                                                                                                                                 
#> psych        "psychTools, lavaan, lme4, Rcsdp, graph, knitr, Rgraphviz"                                                                                                                                                                                                                                                                                                                                
#> purrr        "carrier (>= 0.3.0), covr, dplyr (>= 0.7.8), httr, knitr,\nlubridate, mirai (>= 2.5.1), rmarkdown, testthat (>= 3.0.0),\ntibble, tidyselect"                                                                                                                                                                                                                                              
#> qgraph       "BDgraph, huge"                                                                                                                                                                                                                                                                                                                                                                           
#> quadprog     NA                                                                                                                                                                                                                                                                                                                                                                                        
#> quitte       "covr, gdxrrw, knitr, mip, rmarkdown, testthat (>= 3.2.0),\ntidyverse, withr,"                                                                                                                                                                                                                                                                                                            
#> ragg         "covr, graphics, grid, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                               
#> rappdirs     "covr, roxygen2, testthat (>= 3.2.0), withr"                                                                                                                                                                                                                                                                                                                                              
#> raster       "ncdf4, igraph, tcltk, parallel, rasterVis, MASS, sf,\ntinytest, gstat, fields, exactextractr"                                                                                                                                                                                                                                                                                            
#> rcmdcheck    "covr, knitr, mockery, processx, ps, rmarkdown, svglite,\ntestthat, webfakes"                                                                                                                                                                                                                                                                                                             
#> readr        "covr, curl, datasets, knitr, rmarkdown, spelling, stringi,\ntestthat (>= 3.2.0), tzdb (>= 0.1.1), waldo, xml2"                                                                                                                                                                                                                                                                           
#> readxl       "covr, knitr, rmarkdown, testthat (>= 3.1.6), withr"                                                                                                                                                                                                                                                                                                                                      
#> rematch      "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                          
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
#> sp           "RColorBrewer, gstat, deldir, knitr, maps, mapview, rmarkdown,\nsf, terra, raster"                                                                                                                                                                                                                                                                                                        
#> stringi      NA                                                                                                                                                                                                                                                                                                                                                                                        
#> stringr      "covr, dplyr, gt, htmltools, htmlwidgets, knitr, rmarkdown,\ntestthat (>= 3.0.0), tibble"                                                                                                                                                                                                                                                                                                 
#> styler       "data.tree (>= 0.1.6), digest, here, knitr, prettycode,\nrmarkdown, roxygen2, rstudioapi (>= 0.7), tibble (>= 1.4.2),\ntestthat (>= 3.2.1)"                                                                                                                                                                                                                                               
#> sys          "unix (>= 1.4), spelling, testthat"                                                                                                                                                                                                                                                                                                                                                       
#> systemfonts  "covr, farver, ggplot2, graphics, knitr, ragg, rmarkdown,\nsvglite, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                  
#> terra        "parallel, tinytest, ncdf4, sf (>= 0.9-8), deldir, XML,\nleaflet (>= 2.2.1), htmlwidgets"                                                                                                                                                                                                                                                                                                 
#> testthat     "covr, curl (>= 0.9.5), diffviewer (>= 0.1.0), digest (>=\n0.6.33), gh, knitr, otel, otelsdk, rmarkdown, rstudioapi, S7,\nshiny, usethis, vctrs (>= 0.1.0), xml2"                                                                                                                                                                                                                         
#> textshaping  "covr, grDevices, grid, knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                            
#> tibble       "bench, bit64, blob, brio, callr, DiagrammeR, dplyr, evaluate,\nformattable, ggplot2, here, hms, htmltools, knitr, lubridate,\nnycflights13, pkgload, purrr, rmarkdown, stringi, testthat (>=\n3.0.2), tidyr, withr"                                                                                                                                                                      
#> tidyr        "covr, data.table, knitr, readr, repurrrsive (>= 1.1.0),\nrmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                 
#> tidyselect   "covr, crayon, dplyr, knitr, magrittr, rmarkdown, stringr,\ntestthat (>= 3.1.1), tibble (>= 2.1.3)"                                                                                                                                                                                                                                                                                       
#> tidytemplate "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                        
#> timechange   "testthat (>= 0.7.1.99), knitr"                                                                                                                                                                                                                                                                                                                                                           
#> tinytex      "testit, rstudioapi"                                                                                                                                                                                                                                                                                                                                                                      
#> tzdb         "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                               
#> urlchecker   "covr"                                                                                                                                                                                                                                                                                                                                                                                    
#> usethis      "covr, knitr, magick, pkgload (>= 1.3.2.1), quarto (>= 1.5.1),\nrmarkdown, roxygen2 (>= 7.1.2), spelling (>= 1.2), testthat (>=\n3.1.8)"                                                                                                                                                                                                                                                  
#> utf8         "cli, covr, knitr, rlang, rmarkdown, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                                                                                         
#> vctrs        "bit64, covr, crayon, dplyr (>= 0.8.5), generics, knitr,\npillar (>= 1.4.4), pkgdown (>= 2.0.1), rmarkdown, testthat (>=\n3.0.0), tibble (>= 3.1.3), waldo (>= 0.2.0), withr, xml2,\nzeallot"                                                                                                                                                                                             
#> viridisLite  "hexbin (>= 1.27.0), ggplot2 (>= 1.0.1), testthat, covr"                                                                                                                                                                                                                                                                                                                                  
#> vroom        "archive, bench (>= 1.1.0), covr, curl, dplyr, forcats, fs,\nggplot2, knitr, patchwork, prettyunits, purrr, rmarkdown,\nrstudioapi, scales, spelling, testthat (>= 2.1.0), tidyr,\nutils, waldo, xml2"                                                                                                                                                                                    
#> waldo        "bit64, R6, S7, testthat (>= 3.0.0), withr, xml2"                                                                                                                                                                                                                                                                                                                                         
#> whisker      "markdown"                                                                                                                                                                                                                                                                                                                                                                                
#> withr        "callr, DBI, knitr, methods, rlang, rmarkdown (>= 2.12),\nRSQLite, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                   
#> writexl      "spelling, readxl, nycflights13, testthat, bit64"                                                                                                                                                                                                                                                                                                                                         
#> xfun         "testit, parallel, codetools, methods, rstudioapi, tinytex (>=\n0.30), mime, litedown (>= 0.6), commonmark, knitr (>= 1.50),\nremotes, pak, curl, xml2, jsonlite, magick, yaml, data.table,\nqs2"                                                                                                                                                                                         
#> xml2         "covr, curl, httr, knitr, mockery, rmarkdown, testthat (>=\n3.2.0), xslt"                                                                                                                                                                                                                                                                                                                 
#> xmlparsedata "covr, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                    
#> xopen        "ps, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                 
#> xtable       "knitr, zoo, survival, glue, tinytex"                                                                                                                                                                                                                                                                                                                                                     
#> yaml         "knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                   
#> zip          "covr, pillar, processx, R6, testthat, withr"                                                                                                                                                                                                                                                                                                                                             
#> zoo          "AER, coda, chron, ggplot2 (>= 3.5.0), mondate, scales,\nstinepack, strucchange, timeDate, timeSeries, tinyplot, tis,\ntseries, xts"                                                                                                                                                                                                                                                      
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
#> tools        "codetools, methods, xml2, curl, commonmark, knitr, xfun, mathjaxr, V8"                                                                                                                                                                                                                                                                                                                   
#> utils        "methods, xml2, commonmark, knitr, jsonlite"                                                                                                                                                                                                                                                                                                                                              
#>              Enhances                                               
#> DT           NA                                                     
#> Formula      NA                                                     
#> GDPuc        NA                                                     
#> GPArotation  NA                                                     
#> Hmisc        NA                                                     
#> R.cache      NA                                                     
#> R.methodsS3  NA                                                     
#> R.oo         NA                                                     
#> R.utils      NA                                                     
#> R6           NA                                                     
#> RColorBrewer NA                                                     
#> Rcpp         NA                                                     
#> S7           NA                                                     
#> WDI          NA                                                     
#> abind        NA                                                     
#> askpass      NA                                                     
#> backports    NA                                                     
#> base64enc    "png"                                                  
#> bit          NA                                                     
#> bit64        NA                                                     
#> box          "rstudioapi"                                           
#> brew         NA                                                     
#> brio         NA                                                     
#> bslib        NA                                                     
#> cachem       NA                                                     
#> callr        NA                                                     
#> cellranger   NA                                                     
#> checkmate    NA                                                     
#> citation     NA                                                     
#> cli          NA                                                     
#> clipr        NA                                                     
#> collections  NA                                                     
#> colorspace   NA                                                     
#> commonmark   NA                                                     
#> corpcor      NA                                                     
#> countrycode  NA                                                     
#> covr         NA                                                     
#> crayon       NA                                                     
#> credentials  NA                                                     
#> crosstalk    NA                                                     
#> curl         NA                                                     
#> data.table   NA                                                     
#> desc         NA                                                     
#> devtools     NA                                                     
#> diffobj      NA                                                     
#> digest       NA                                                     
#> doParallel   "compiler"                                             
#> downlit      NA                                                     
#> dplyr        NA                                                     
#> ellipsis     NA                                                     
#> evaluate     NA                                                     
#> fansi        NA                                                     
#> farver       NA                                                     
#> fastmap      NA                                                     
#> fdrtool      NA                                                     
#> filelock     NA                                                     
#> fontawesome  NA                                                     
#> forcats      NA                                                     
#> foreach      NA                                                     
#> fs           NA                                                     
#> gamstransfer NA                                                     
#> gdx          NA                                                     
#> gdxrrw       NA                                                     
#> generics     NA                                                     
#> gert         NA                                                     
#> ggplot2      "sp"                                                   
#> gh           NA                                                     
#> gitcreds     NA                                                     
#> glasso       NA                                                     
#> glue         NA                                                     
#> gms          NA                                                     
#> goxygen      NA                                                     
#> gridExtra    NA                                                     
#> gtable       NA                                                     
#> gtools       NA                                                     
#> highr        NA                                                     
#> hms          NA                                                     
#> htmlTable    NA                                                     
#> htmltools    "knitr"                                                
#> htmlwidgets  "shiny (>= 1.1)"                                       
#> httpuv       NA                                                     
#> httr         NA                                                     
#> httr2        NA                                                     
#> igraph       "graph"                                                
#> ini          NA                                                     
#> isoband      NA                                                     
#> iterators    NA                                                     
#> jpeg         NA                                                     
#> jquerylib    NA                                                     
#> jsonlite     NA                                                     
#> knitr        NA                                                     
#> labeling     NA                                                     
#> later        NA                                                     
#> lavaan       NA                                                     
#> lazyeval     NA                                                     
#> lifecycle    NA                                                     
#> lintr        "data.table"                                           
#> lpjmlkit     NA                                                     
#> lubridate    "chron, data.table, timeDate, tis, zoo"                
#> lucode2      NA                                                     
#> lusweave     NA                                                     
#> madrat       NA                                                     
#> magclass     NA                                                     
#> magrittr     NA                                                     
#> memoise      NA                                                     
#> mime         NA                                                     
#> miniUI       NA                                                     
#> mnormt       NA                                                     
#> ncdf4        NA                                                     
#> numDeriv     NA                                                     
#> openssl      NA                                                     
#> otel         NA                                                     
#> pak          NA                                                     
#> pander       NA                                                     
#> pbapply      NA                                                     
#> pbivnorm     NA                                                     
#> pillar       NA                                                     
#> pkgbuild     NA                                                     
#> pkgconfig    NA                                                     
#> pkgdown      NA                                                     
#> pkgload      NA                                                     
#> plyr         NA                                                     
#> png          NA                                                     
#> poorman      NA                                                     
#> praise       NA                                                     
#> prettyunits  NA                                                     
#> processx     NA                                                     
#> profvis      NA                                                     
#> promises     NA                                                     
#> ps           NA                                                     
#> psych        NA                                                     
#> purrr        NA                                                     
#> qgraph       NA                                                     
#> quadprog     NA                                                     
#> quitte       NA                                                     
#> ragg         NA                                                     
#> rappdirs     NA                                                     
#> raster       NA                                                     
#> rcmdcheck    NA                                                     
#> readr        NA                                                     
#> readxl       NA                                                     
#> rematch      NA                                                     
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
#> sp           NA                                                     
#> stringi      NA                                                     
#> stringr      NA                                                     
#> styler       NA                                                     
#> sys          NA                                                     
#> systemfonts  NA                                                     
#> terra        NA                                                     
#> testthat     NA                                                     
#> textshaping  NA                                                     
#> tibble       NA                                                     
#> tidyr        NA                                                     
#> tidyselect   NA                                                     
#> tidytemplate NA                                                     
#> timechange   NA                                                     
#> tinytex      NA                                                     
#> tzdb         NA                                                     
#> urlchecker   NA                                                     
#> usethis      NA                                                     
#> utf8         NA                                                     
#> vctrs        NA                                                     
#> viridisLite  NA                                                     
#> vroom        NA                                                     
#> waldo        NA                                                     
#> whisker      NA                                                     
#> withr        NA                                                     
#> writexl      NA                                                     
#> xfun         NA                                                     
#> xml2         NA                                                     
#> xmlparsedata NA                                                     
#> xopen        NA                                                     
#> xtable       NA                                                     
#> yaml         NA                                                     
#> zip          NA                                                     
#> zoo          NA                                                     
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
#> parallel     "snow, Rmpi, mirai"                                    
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
#> DT           "MIT + file LICENSE"                            
#> Formula      "GPL-2 | GPL-3"                                 
#> GDPuc        "GPL (>= 3)"                                    
#> GPArotation  "GPL (>= 2)"                                    
#> Hmisc        "GPL (>= 2)"                                    
#> R.cache      "LGPL (>= 2.1)"                                 
#> R.methodsS3  "LGPL (>= 2.1)"                                 
#> R.oo         "LGPL (>= 2.1)"                                 
#> R.utils      "LGPL (>= 2.1)"                                 
#> R6           "MIT + file LICENSE"                            
#> RColorBrewer "Apache License 2.0"                            
#> Rcpp         "GPL (>= 2)"                                    
#> S7           "MIT + file LICENSE"                            
#> WDI          "GPL-3"                                         
#> abind        "MIT + file LICENSE"                            
#> askpass      "MIT + file LICENSE"                            
#> backports    "GPL-2 | GPL-3"                                 
#> base64enc    "GPL-2 | GPL-3"                                 
#> bit          "GPL-2 | GPL-3"                                 
#> bit64        "GPL-2 | GPL-3"                                 
#> box          "MIT + file LICENSE"                            
#> brew         "GPL (>= 2)"                                    
#> brio         "MIT + file LICENSE"                            
#> bslib        "MIT + file LICENSE"                            
#> cachem       "MIT + file LICENSE"                            
#> callr        "MIT + file LICENSE"                            
#> cellranger   "MIT + file LICENSE"                            
#> checkmate    "BSD_3_clause + file LICENSE"                   
#> citation     "BSD_2_clause + file LICENSE"                   
#> cli          "MIT + file LICENSE"                            
#> clipr        "GPL-3"                                         
#> collections  "MIT + file LICENSE"                            
#> colorspace   "BSD_3_clause + file LICENSE"                   
#> commonmark   "BSD_2_clause + file LICENSE"                   
#> corpcor      "GPL (>= 3)"                                    
#> countrycode  "GPL-3"                                         
#> covr         "MIT + file LICENSE"                            
#> crayon       "MIT + file LICENSE"                            
#> credentials  "MIT + file LICENSE"                            
#> crosstalk    "MIT + file LICENSE"                            
#> curl         "MIT + file LICENSE"                            
#> data.table   "MPL-2.0 | file LICENSE"                        
#> desc         "MIT + file LICENSE"                            
#> devtools     "MIT + file LICENSE"                            
#> diffobj      "GPL-2 | GPL-3"                                 
#> digest       "GPL (>= 2)"                                    
#> doParallel   "GPL-2"                                         
#> downlit      "MIT + file LICENSE"                            
#> dplyr        "MIT + file LICENSE"                            
#> ellipsis     "MIT + file LICENSE"                            
#> evaluate     "MIT + file LICENSE"                            
#> fansi        "GPL-2 | GPL-3"                                 
#> farver       "MIT + file LICENSE"                            
#> fastmap      "MIT + file LICENSE"                            
#> fdrtool      "GPL (>= 3)"                                    
#> filelock     "MIT + file LICENSE"                            
#> fontawesome  "MIT + file LICENSE"                            
#> forcats      "MIT + file LICENSE"                            
#> foreach      "Apache License (== 2.0)"                       
#> fs           "MIT + file LICENSE"                            
#> gamstransfer "MIT + file LICENSE"                            
#> gdx          "BSD_2_clause + file LICENSE"                   
#> gdxrrw       "EPL2 with Secondary License GPL-2.0 or greater"
#> generics     "MIT + file LICENSE"                            
#> gert         "MIT + file LICENSE"                            
#> ggplot2      "MIT + file LICENSE"                            
#> gh           "MIT + file LICENSE"                            
#> gitcreds     "MIT + file LICENSE"                            
#> glasso       "GPL-2"                                         
#> glue         "MIT + file LICENSE"                            
#> gms          "BSD_2_clause + file LICENSE"                   
#> goxygen      "BSD_2_clause + file LICENSE"                   
#> gridExtra    "GPL (>= 2)"                                    
#> gtable       "MIT + file LICENSE"                            
#> gtools       "GPL-2"                                         
#> highr        "GPL"                                           
#> hms          "MIT + file LICENSE"                            
#> htmlTable    "GPL (>= 3)"                                    
#> htmltools    "GPL (>= 2)"                                    
#> htmlwidgets  "MIT + file LICENSE"                            
#> httpuv       "GPL (>= 2) | file LICENSE"                     
#> httr         "MIT + file LICENSE"                            
#> httr2        "MIT + file LICENSE"                            
#> igraph       "GPL (>= 2)"                                    
#> ini          "GPL-3"                                         
#> isoband      "MIT + file LICENSE"                            
#> iterators    "Apache License (== 2.0)"                       
#> jpeg         "GPL-2 | GPL-3"                                 
#> jquerylib    "MIT + file LICENSE"                            
#> jsonlite     "MIT + file LICENSE"                            
#> knitr        "GPL"                                           
#> labeling     "MIT + file LICENSE | Unlimited"                
#> later        "MIT + file LICENSE"                            
#> lavaan       "GPL (>= 2)"                                    
#> lazyeval     "GPL-3"                                         
#> lifecycle    "MIT + file LICENSE"                            
#> lintr        "MIT + file LICENSE"                            
#> lpjmlkit     "AGPL-3"                                        
#> lubridate    "MIT + file LICENSE"                            
#> lucode2      "BSD_2_clause + file LICENSE"                   
#> lusweave     "BSD_2_clause + file LICENSE"                   
#> madrat       "BSD_2_clause + file LICENSE"                   
#> magclass     "LGPL-3 | file LICENSE"                         
#> magrittr     "MIT + file LICENSE"                            
#> memoise      "MIT + file LICENSE"                            
#> mime         "GPL"                                           
#> miniUI       "GPL-3"                                         
#> mnormt       "GPL-2 | GPL-3"                                 
#> ncdf4        "GPL (>= 3)"                                    
#> numDeriv     "GPL-2"                                         
#> openssl      "MIT + file LICENSE"                            
#> otel         "MIT + file LICENSE"                            
#> pak          "GPL-3"                                         
#> pander       "AGPL-3 | file LICENSE"                         
#> pbapply      "GPL (>= 2)"                                    
#> pbivnorm     "GPL (>= 2)"                                    
#> pillar       "MIT + file LICENSE"                            
#> pkgbuild     "MIT + file LICENSE"                            
#> pkgconfig    "MIT + file LICENSE"                            
#> pkgdown      "MIT + file LICENSE"                            
#> pkgload      "MIT + file LICENSE"                            
#> plyr         "MIT + file LICENSE"                            
#> png          "GPL-2 | GPL-3"                                 
#> poorman      "MIT + file LICENSE"                            
#> praise       "MIT + file LICENSE"                            
#> prettyunits  "MIT + file LICENSE"                            
#> processx     "MIT + file LICENSE"                            
#> profvis      "MIT + file LICENSE"                            
#> promises     "MIT + file LICENSE"                            
#> ps           "MIT + file LICENSE"                            
#> psych        "GPL (>= 2)"                                    
#> purrr        "MIT + file LICENSE"                            
#> qgraph       "GPL-2"                                         
#> quadprog     "GPL (>= 2)"                                    
#> quitte       "GPL-2"                                         
#> ragg         "MIT + file LICENSE"                            
#> rappdirs     "MIT + file LICENSE"                            
#> raster       "GPL (>= 3)"                                    
#> rcmdcheck    "MIT + file LICENSE"                            
#> readr        "MIT + file LICENSE"                            
#> readxl       "MIT + file LICENSE"                            
#> rematch      "MIT + file LICENSE"                            
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
#> sp           "GPL (>= 2)"                                    
#> stringi      "file LICENSE"                                  
#> stringr      "MIT + file LICENSE"                            
#> styler       "MIT + file LICENSE"                            
#> sys          "MIT + file LICENSE"                            
#> systemfonts  "MIT + file LICENSE"                            
#> terra        "GPL (>= 3)"                                    
#> testthat     "MIT + file LICENSE"                            
#> textshaping  "MIT + file LICENSE"                            
#> tibble       "MIT + file LICENSE"                            
#> tidyr        "MIT + file LICENSE"                            
#> tidyselect   "MIT + file LICENSE"                            
#> tidytemplate "MIT + file LICENSE"                            
#> timechange   "GPL (>= 3)"                                    
#> tinytex      "MIT + file LICENSE"                            
#> tzdb         "MIT + file LICENSE"                            
#> urlchecker   "GPL-3"                                         
#> usethis      "MIT + file LICENSE"                            
#> utf8         "Apache License (== 2.0) | file LICENSE"        
#> vctrs        "MIT + file LICENSE"                            
#> viridisLite  "MIT + file LICENSE"                            
#> vroom        "MIT + file LICENSE"                            
#> waldo        "MIT + file LICENSE"                            
#> whisker      "GPL-3"                                         
#> withr        "MIT + file LICENSE"                            
#> writexl      "BSD_2_clause + file LICENSE"                   
#> xfun         "MIT + file LICENSE"                            
#> xml2         "MIT + file LICENSE"                            
#> xmlparsedata "MIT + file LICENSE"                            
#> xopen        "MIT + file LICENSE"                            
#> xtable       "GPL (>= 2)"                                    
#> yaml         "BSD_3_clause + file LICENSE"                   
#> zip          "MIT + file LICENSE"                            
#> zoo          "GPL-2 | GPL-3"                                 
#> pak          "GPL-3"                                         
#> KernSmooth   "Unlimited"                                     
#> MASS         "GPL-2 | GPL-3"                                 
#> Matrix       "GPL (>= 2) | file LICENCE"                     
#> base         "Part of R 4.5.3"                               
#> boot         "Unlimited"                                     
#> class        "GPL-2 | GPL-3"                                 
#> cluster      "GPL (>= 2)"                                    
#> codetools    "GPL"                                           
#> compiler     "Part of R 4.5.3"                               
#> datasets     "Part of R 4.5.3"                               
#> foreign      "GPL (>= 2)"                                    
#> grDevices    "Part of R 4.5.3"                               
#> graphics     "Part of R 4.5.3"                               
#> grid         "Part of R 4.5.3"                               
#> lattice      "GPL (>= 2)"                                    
#> methods      "Part of R 4.5.3"                               
#> mgcv         "GPL (>= 2)"                                    
#> nlme         "GPL (>= 2)"                                    
#> nnet         "GPL-2 | GPL-3"                                 
#> parallel     "Part of R 4.5.3"                               
#> rpart        "GPL-2 | GPL-3"                                 
#> spatial      "GPL-2 | GPL-3"                                 
#> splines      "Part of R 4.5.3"                               
#> stats        "Part of R 4.5.3"                               
#> stats4       "Part of R 4.5.3"                               
#> survival     "LGPL (>= 2)"                                   
#> tcltk        "Part of R 4.5.3"                               
#> tools        "Part of R 4.5.3"                               
#> utils        "Part of R 4.5.3"                               
#>              License_is_FOSS License_restricts_use OS_type MD5sum
#> DT           NA              NA                    NA      NA    
#> Formula      NA              NA                    NA      NA    
#> GDPuc        NA              NA                    NA      NA    
#> GPArotation  NA              NA                    NA      NA    
#> Hmisc        NA              NA                    NA      NA    
#> R.cache      NA              NA                    NA      NA    
#> R.methodsS3  NA              NA                    NA      NA    
#> R.oo         NA              NA                    NA      NA    
#> R.utils      NA              NA                    NA      NA    
#> R6           NA              NA                    NA      NA    
#> RColorBrewer NA              NA                    NA      NA    
#> Rcpp         NA              NA                    NA      NA    
#> S7           NA              NA                    NA      NA    
#> WDI          NA              NA                    NA      NA    
#> abind        NA              NA                    NA      NA    
#> askpass      NA              NA                    NA      NA    
#> backports    NA              NA                    NA      NA    
#> base64enc    NA              NA                    NA      NA    
#> bit          NA              NA                    NA      NA    
#> bit64        NA              NA                    NA      NA    
#> box          NA              NA                    NA      NA    
#> brew         NA              NA                    NA      NA    
#> brio         NA              NA                    NA      NA    
#> bslib        NA              NA                    NA      NA    
#> cachem       NA              NA                    NA      NA    
#> callr        NA              NA                    NA      NA    
#> cellranger   NA              NA                    NA      NA    
#> checkmate    NA              NA                    NA      NA    
#> citation     NA              NA                    NA      NA    
#> cli          NA              NA                    NA      NA    
#> clipr        NA              NA                    NA      NA    
#> collections  NA              NA                    NA      NA    
#> colorspace   NA              NA                    NA      NA    
#> commonmark   NA              NA                    NA      NA    
#> corpcor      NA              NA                    NA      NA    
#> countrycode  NA              NA                    NA      NA    
#> covr         NA              NA                    NA      NA    
#> crayon       NA              NA                    NA      NA    
#> credentials  NA              NA                    NA      NA    
#> crosstalk    NA              NA                    NA      NA    
#> curl         NA              NA                    NA      NA    
#> data.table   NA              NA                    NA      NA    
#> desc         NA              NA                    NA      NA    
#> devtools     NA              NA                    NA      NA    
#> diffobj      NA              NA                    NA      NA    
#> digest       NA              NA                    NA      NA    
#> doParallel   NA              NA                    NA      NA    
#> downlit      NA              NA                    NA      NA    
#> dplyr        NA              NA                    NA      NA    
#> ellipsis     NA              NA                    NA      NA    
#> evaluate     NA              NA                    NA      NA    
#> fansi        NA              NA                    NA      NA    
#> farver       NA              NA                    NA      NA    
#> fastmap      NA              NA                    NA      NA    
#> fdrtool      NA              NA                    NA      NA    
#> filelock     NA              NA                    NA      NA    
#> fontawesome  NA              NA                    NA      NA    
#> forcats      NA              NA                    NA      NA    
#> foreach      NA              NA                    NA      NA    
#> fs           NA              NA                    NA      NA    
#> gamstransfer NA              NA                    NA      NA    
#> gdx          NA              NA                    NA      NA    
#> gdxrrw       NA              NA                    NA      NA    
#> generics     NA              NA                    NA      NA    
#> gert         NA              NA                    NA      NA    
#> ggplot2      NA              NA                    NA      NA    
#> gh           NA              NA                    NA      NA    
#> gitcreds     NA              NA                    NA      NA    
#> glasso       NA              NA                    NA      NA    
#> glue         NA              NA                    NA      NA    
#> gms          NA              NA                    NA      NA    
#> goxygen      NA              NA                    NA      NA    
#> gridExtra    NA              NA                    NA      NA    
#> gtable       NA              NA                    NA      NA    
#> gtools       NA              NA                    NA      NA    
#> highr        NA              NA                    NA      NA    
#> hms          NA              NA                    NA      NA    
#> htmlTable    NA              NA                    NA      NA    
#> htmltools    NA              NA                    NA      NA    
#> htmlwidgets  NA              NA                    NA      NA    
#> httpuv       NA              NA                    NA      NA    
#> httr         NA              NA                    NA      NA    
#> httr2        NA              NA                    NA      NA    
#> igraph       NA              NA                    NA      NA    
#> ini          NA              NA                    NA      NA    
#> isoband      NA              NA                    NA      NA    
#> iterators    NA              NA                    NA      NA    
#> jpeg         NA              NA                    NA      NA    
#> jquerylib    NA              NA                    NA      NA    
#> jsonlite     NA              NA                    NA      NA    
#> knitr        NA              NA                    NA      NA    
#> labeling     NA              NA                    NA      NA    
#> later        NA              NA                    NA      NA    
#> lavaan       NA              NA                    NA      NA    
#> lazyeval     NA              NA                    NA      NA    
#> lifecycle    NA              NA                    NA      NA    
#> lintr        NA              NA                    NA      NA    
#> lpjmlkit     NA              NA                    NA      NA    
#> lubridate    NA              NA                    NA      NA    
#> lucode2      NA              NA                    NA      NA    
#> lusweave     NA              NA                    NA      NA    
#> madrat       NA              NA                    NA      NA    
#> magclass     NA              NA                    NA      NA    
#> magrittr     NA              NA                    NA      NA    
#> memoise      NA              NA                    NA      NA    
#> mime         NA              NA                    NA      NA    
#> miniUI       NA              NA                    NA      NA    
#> mnormt       NA              NA                    NA      NA    
#> ncdf4        NA              NA                    NA      NA    
#> numDeriv     NA              NA                    NA      NA    
#> openssl      NA              NA                    NA      NA    
#> otel         NA              NA                    NA      NA    
#> pak          NA              NA                    NA      NA    
#> pander       NA              NA                    NA      NA    
#> pbapply      NA              NA                    NA      NA    
#> pbivnorm     NA              NA                    NA      NA    
#> pillar       NA              NA                    NA      NA    
#> pkgbuild     NA              NA                    NA      NA    
#> pkgconfig    NA              NA                    NA      NA    
#> pkgdown      NA              NA                    NA      NA    
#> pkgload      NA              NA                    NA      NA    
#> plyr         NA              NA                    NA      NA    
#> png          NA              NA                    NA      NA    
#> poorman      NA              NA                    NA      NA    
#> praise       NA              NA                    NA      NA    
#> prettyunits  NA              NA                    NA      NA    
#> processx     NA              NA                    NA      NA    
#> profvis      NA              NA                    NA      NA    
#> promises     NA              NA                    NA      NA    
#> ps           NA              NA                    NA      NA    
#> psych        NA              NA                    NA      NA    
#> purrr        NA              NA                    NA      NA    
#> qgraph       NA              NA                    NA      NA    
#> quadprog     NA              NA                    NA      NA    
#> quitte       NA              NA                    NA      NA    
#> ragg         NA              NA                    NA      NA    
#> rappdirs     NA              NA                    NA      NA    
#> raster       NA              NA                    NA      NA    
#> rcmdcheck    NA              NA                    NA      NA    
#> readr        NA              NA                    NA      NA    
#> readxl       NA              NA                    NA      NA    
#> rematch      NA              NA                    NA      NA    
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
#> sp           NA              NA                    NA      NA    
#> stringi      "yes"           NA                    NA      NA    
#> stringr      NA              NA                    NA      NA    
#> styler       NA              NA                    NA      NA    
#> sys          NA              NA                    NA      NA    
#> systemfonts  NA              NA                    NA      NA    
#> terra        NA              NA                    NA      NA    
#> testthat     NA              NA                    NA      NA    
#> textshaping  NA              NA                    NA      NA    
#> tibble       NA              NA                    NA      NA    
#> tidyr        NA              NA                    NA      NA    
#> tidyselect   NA              NA                    NA      NA    
#> tidytemplate NA              NA                    NA      NA    
#> timechange   NA              NA                    NA      NA    
#> tinytex      NA              NA                    NA      NA    
#> tzdb         NA              NA                    NA      NA    
#> urlchecker   NA              NA                    NA      NA    
#> usethis      NA              NA                    NA      NA    
#> utf8         NA              NA                    NA      NA    
#> vctrs        NA              NA                    NA      NA    
#> viridisLite  NA              NA                    NA      NA    
#> vroom        NA              NA                    NA      NA    
#> waldo        NA              NA                    NA      NA    
#> whisker      NA              NA                    NA      NA    
#> withr        NA              NA                    NA      NA    
#> writexl      NA              NA                    NA      NA    
#> xfun         NA              NA                    NA      NA    
#> xml2         NA              NA                    NA      NA    
#> xmlparsedata NA              NA                    NA      NA    
#> xopen        NA              NA                    NA      NA    
#> xtable       NA              NA                    NA      NA    
#> yaml         NA              NA                    NA      NA    
#> zip          NA              NA                    NA      NA    
#> zoo          NA              NA                    NA      NA    
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
#>              NeedsCompilation Built  
#> DT           "no"             "4.5.0"
#> Formula      "no"             "4.5.0"
#> GDPuc        "no"             "4.5.0"
#> GPArotation  "no"             "4.5.0"
#> Hmisc        "yes"            "4.5.0"
#> R.cache      "no"             "4.5.0"
#> R.methodsS3  "no"             "4.5.0"
#> R.oo         "no"             "4.5.0"
#> R.utils      "no"             "4.5.0"
#> R6           "no"             "4.5.0"
#> RColorBrewer "no"             "4.5.0"
#> Rcpp         "yes"            "4.5.0"
#> S7           "yes"            "4.5.0"
#> WDI          "no"             "4.5.0"
#> abind        "no"             "4.5.0"
#> askpass      "yes"            "4.5.0"
#> backports    "yes"            "4.5.0"
#> base64enc    "yes"            "4.5.0"
#> bit          "yes"            "4.5.0"
#> bit64        "yes"            "4.5.0"
#> box          "yes"            "4.5.0"
#> brew         "no"             "4.5.0"
#> brio         "yes"            "4.5.0"
#> bslib        "no"             "4.5.0"
#> cachem       "yes"            "4.5.0"
#> callr        "no"             "4.5.0"
#> cellranger   "no"             "4.5.0"
#> checkmate    "yes"            "4.5.0"
#> citation     "no"             "4.5.0"
#> cli          "yes"            "4.5.0"
#> clipr        "no"             "4.5.0"
#> collections  "yes"            "4.5.0"
#> colorspace   "yes"            "4.5.0"
#> commonmark   "yes"            "4.5.0"
#> corpcor      "no"             "4.5.0"
#> countrycode  "no"             "4.5.0"
#> covr         "yes"            "4.5.0"
#> crayon       "no"             "4.5.0"
#> credentials  "no"             "4.5.0"
#> crosstalk    "no"             "4.5.0"
#> curl         "yes"            "4.5.0"
#> data.table   "yes"            "4.5.0"
#> desc         "no"             "4.5.0"
#> devtools     "no"             "4.5.0"
#> diffobj      "yes"            "4.5.0"
#> digest       "yes"            "4.5.0"
#> doParallel   "no"             "4.5.0"
#> downlit      "no"             "4.5.0"
#> dplyr        "yes"            "4.5.0"
#> ellipsis     "no"             "4.5.0"
#> evaluate     "no"             "4.5.0"
#> fansi        "yes"            "4.5.0"
#> farver       "yes"            "4.5.0"
#> fastmap      "yes"            "4.5.0"
#> fdrtool      "yes"            "4.5.0"
#> filelock     "yes"            "4.5.0"
#> fontawesome  "no"             "4.5.0"
#> forcats      "no"             "4.5.0"
#> foreach      "no"             "4.5.0"
#> fs           "yes"            "4.5.0"
#> gamstransfer "yes"            "4.5.0"
#> gdx          "no"             "4.5.3"
#> gdxrrw       "yes"            "4.5.3"
#> generics     "no"             "4.5.0"
#> gert         "yes"            "4.5.0"
#> ggplot2      "no"             "4.5.0"
#> gh           "no"             "4.5.0"
#> gitcreds     "no"             "4.5.0"
#> glasso       "yes"            "4.5.0"
#> glue         "yes"            "4.5.0"
#> gms          "no"             "4.5.3"
#> goxygen      "no"             "4.5.3"
#> gridExtra    "no"             "4.5.0"
#> gtable       "no"             "4.5.0"
#> gtools       "yes"            "4.5.0"
#> highr        "no"             "4.5.0"
#> hms          "no"             "4.5.0"
#> htmlTable    "no"             "4.5.0"
#> htmltools    "yes"            "4.5.0"
#> htmlwidgets  "no"             "4.5.0"
#> httpuv       "yes"            "4.5.0"
#> httr         "no"             "4.5.0"
#> httr2        "no"             "4.5.0"
#> igraph       "yes"            "4.5.0"
#> ini          "no"             "4.5.0"
#> isoband      "yes"            "4.5.0"
#> iterators    "no"             "4.5.0"
#> jpeg         "yes"            "4.5.0"
#> jquerylib    "no"             "4.5.0"
#> jsonlite     "yes"            "4.5.0"
#> knitr        "no"             "4.5.0"
#> labeling     "no"             "4.5.0"
#> later        "yes"            "4.5.0"
#> lavaan       "no"             "4.5.0"
#> lazyeval     "yes"            "4.5.0"
#> lifecycle    "no"             "4.5.0"
#> lintr        "no"             "4.5.0"
#> lpjmlkit     "no"             "4.5.3"
#> lubridate    "yes"            "4.5.0"
#> lucode2      "no"             "4.5.3"
#> lusweave     "no"             "4.5.3"
#> madrat       "no"             "4.5.3"
#> magclass     "no"             "4.5.0"
#> magrittr     "yes"            "4.5.0"
#> memoise      "no"             "4.5.0"
#> mime         "yes"            "4.5.0"
#> miniUI       "no"             "4.5.0"
#> mnormt       "yes"            "4.5.0"
#> ncdf4        "yes"            "4.5.0"
#> numDeriv     "no"             "4.5.0"
#> openssl      "yes"            "4.5.0"
#> otel         "no"             "4.5.0"
#> pak          "yes"            "4.5.0"
#> pander       "yes"            "4.5.0"
#> pbapply      "no"             "4.5.0"
#> pbivnorm     "yes"            "4.5.0"
#> pillar       "no"             "4.5.0"
#> pkgbuild     "no"             "4.5.0"
#> pkgconfig    "no"             "4.5.0"
#> pkgdown      "no"             "4.5.0"
#> pkgload      "no"             "4.5.0"
#> plyr         "yes"            "4.5.0"
#> png          "yes"            "4.5.0"
#> poorman      "no"             "4.5.0"
#> praise       "no"             "4.5.0"
#> prettyunits  "no"             "4.5.0"
#> processx     "yes"            "4.5.0"
#> profvis      "yes"            "4.5.0"
#> promises     "no"             "4.5.0"
#> ps           "yes"            "4.5.0"
#> psych        "no"             "4.5.0"
#> purrr        "yes"            "4.5.0"
#> qgraph       "yes"            "4.5.0"
#> quadprog     "yes"            "4.5.0"
#> quitte       "no"             "4.5.3"
#> ragg         "yes"            "4.5.0"
#> rappdirs     "yes"            "4.5.0"
#> raster       "yes"            "4.5.0"
#> rcmdcheck    "no"             "4.5.0"
#> readr        "yes"            "4.5.0"
#> readxl       "yes"            "4.5.0"
#> rematch      "no"             "4.5.0"
#> renv         "yes"            "4.5.0"
#> reshape2     "yes"            "4.5.0"
#> rex          "no"             "4.5.0"
#> rlang        "yes"            "4.5.0"
#> rmarkdown    "no"             "4.5.0"
#> roxygen2     "yes"            "4.5.0"
#> rprojroot    "no"             "4.5.0"
#> rstudioapi   "no"             "4.5.0"
#> rversions    "no"             "4.5.0"
#> sass         "yes"            "4.5.0"
#> scales       "no"             "4.5.0"
#> sessioninfo  "no"             "4.5.0"
#> shiny        "no"             "4.5.0"
#> sourcetools  "yes"            "4.5.0"
#> sp           "yes"            "4.5.0"
#> stringi      "yes"            "4.5.0"
#> stringr      "no"             "4.5.0"
#> styler       "no"             "4.5.0"
#> sys          "yes"            "4.5.0"
#> systemfonts  "yes"            "4.5.0"
#> terra        "yes"            "4.5.0"
#> testthat     "yes"            "4.5.0"
#> textshaping  "yes"            "4.5.0"
#> tibble       "yes"            "4.5.0"
#> tidyr        "yes"            "4.5.0"
#> tidyselect   "yes"            "4.5.0"
#> tidytemplate "no"             "4.5.3"
#> timechange   "yes"            "4.5.0"
#> tinytex      "no"             "4.5.0"
#> tzdb         "yes"            "4.5.0"
#> urlchecker   "no"             "4.5.0"
#> usethis      "no"             "4.5.0"
#> utf8         "yes"            "4.5.0"
#> vctrs        "yes"            "4.5.0"
#> viridisLite  "no"             "4.5.0"
#> vroom        "yes"            "4.5.0"
#> waldo        "no"             "4.5.0"
#> whisker      "no"             "4.5.0"
#> withr        "no"             "4.5.0"
#> writexl      "yes"            "4.5.0"
#> xfun         "yes"            "4.5.0"
#> xml2         "yes"            "4.5.0"
#> xmlparsedata "no"             "4.5.0"
#> xopen        "no"             "4.5.0"
#> xtable       "no"             "4.5.0"
#> yaml         "yes"            "4.5.0"
#> zip          "yes"            "4.5.0"
#> zoo          "yes"            "4.5.0"
#> pak          "yes"            "4.5.2"
#> KernSmooth   "yes"            "4.5.3"
#> MASS         "yes"            "4.5.3"
#> Matrix       "yes"            "4.5.3"
#> base         NA               "4.5.3"
#> boot         "no"             "4.5.3"
#> class        "yes"            "4.5.3"
#> cluster      "yes"            "4.5.3"
#> codetools    "no"             "4.5.3"
#> compiler     NA               "4.5.3"
#> datasets     NA               "4.5.3"
#> foreign      "yes"            "4.5.3"
#> grDevices    "yes"            "4.5.3"
#> graphics     "yes"            "4.5.3"
#> grid         "yes"            "4.5.3"
#> lattice      "yes"            "4.5.3"
#> methods      "yes"            "4.5.3"
#> mgcv         "yes"            "4.5.3"
#> nlme         "yes"            "4.5.3"
#> nnet         "yes"            "4.5.3"
#> parallel     "yes"            "4.5.3"
#> rpart        "yes"            "4.5.3"
#> spatial      "yes"            "4.5.3"
#> splines      "yes"            "4.5.3"
#> stats        "yes"            "4.5.3"
#> stats4       NA               "4.5.3"
#> survival     "yes"            "4.5.3"
#> tcltk        "yes"            "4.5.3"
#> tools        "yes"            "4.5.3"
#> utils        "yes"            "4.5.3"
#> 
```
