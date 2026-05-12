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
#>                                        "d0fc0c5cbf75" 
#>                                               machine 
#>                                              "x86_64" 
#>                                                 login 
#>                                             "unknown" 
#>                                                  user 
#>                                                "root" 
#>                                        effective_user 
#>                                                "root" 
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
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=C              
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Etc/UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods  
#> [7] base     
#> 
#> other attached packages:
#> [1] lucode2_0.55.2
#> 
#> loaded via a namespace (and not attached):
#>  [1] generics_0.1.4     rappdirs_0.3.4     sass_0.4.10       
#>  [4] renv_1.2.2         xml2_1.5.2         digest_0.6.39     
#>  [7] magrittr_2.0.5     evaluate_1.0.5     pkgload_1.5.2     
#> [10] fastmap_1.2.0      jsonlite_2.0.0     processx_3.9.0    
#> [13] pkgbuild_1.4.8     sessioninfo_1.2.3  whisker_0.4.1     
#> [16] backports_1.5.1    ps_1.9.3           purrr_1.2.2       
#> [19] fansi_1.0.7        lintr_3.3.0-1      textshaping_1.0.5 
#> [22] httr2_1.2.2        jquerylib_0.1.4    cli_3.6.6         
#> [25] rlang_1.2.0        tidytemplate_1.0.0 ellipsis_0.3.3    
#> [28] withr_3.0.2        cachem_1.1.0       yaml_2.3.12       
#> [31] devtools_2.5.2     otel_0.2.0         tools_4.5.3       
#> [34] memoise_2.0.1      dplyr_1.2.1        curl_7.1.0        
#> [37] vctrs_0.7.3        R6_2.6.1           lifecycle_1.0.5   
#> [40] htmlwidgets_1.6.4  fs_2.1.0           usethis_3.2.1     
#> [43] ragg_1.5.2         fontawesome_0.5.3  pkgconfig_2.0.3   
#> [46] desc_1.4.3         callr_3.7.6        rex_1.2.2         
#> [49] pkgdown_2.2.0      pillar_1.11.1      bslib_0.10.0      
#> [52] glue_1.8.1         data.table_1.18.4  systemfonts_1.3.2 
#> [55] tidyselect_1.2.1   xfun_0.57          tibble_3.3.1      
#> [58] rstudioapi_0.18.0  knitr_1.51         htmltools_0.5.9   
#> [61] rmarkdown_2.31     compiler_4.5.3     downlit_0.4.5     
#> [64] askpass_1.2.1      openssl_2.4.0     
#> 
#> $libPaths
#> [1] "/usr/local/lib/R/site-library" "/usr/local/lib/R/library"     
#> 
#> $installedpackages
#>                    Package             
#> Deriv              "Deriv"             
#> Formula            "Formula"           
#> GDPuc              "GDPuc"             
#> Hmisc              "Hmisc"             
#> Lmoments           "Lmoments"          
#> MatrixModels       "MatrixModels"      
#> R.methodsS3        "R.methodsS3"       
#> R.oo               "R.oo"              
#> R.utils            "R.utils"           
#> R6                 "R6"                
#> RColorBrewer       "RColorBrewer"      
#> Rcpp               "Rcpp"              
#> Rdpack             "Rdpack"            
#> S7                 "S7"                
#> SPEI               "SPEI"              
#> SparseM            "SparseM"           
#> TLMoments          "TLMoments"         
#> abind              "abind"             
#> askpass            "askpass"           
#> assertr            "assertr"           
#> backports          "backports"         
#> base64enc          "base64enc"         
#> bit                "bit"               
#> bit64              "bit64"             
#> brew               "brew"              
#> brio               "brio"              
#> broom              "broom"             
#> bslib              "bslib"             
#> cachem             "cachem"            
#> callr              "callr"             
#> car                "car"               
#> carData            "carData"           
#> cellranger         "cellranger"        
#> checkmate          "checkmate"         
#> citation           "citation"          
#> cli                "cli"               
#> clipr              "clipr"             
#> collections        "collections"       
#> colorspace         "colorspace"        
#> commonmark         "commonmark"        
#> contfrac           "contfrac"          
#> countrycode        "countrycode"       
#> covr               "covr"              
#> cowplot            "cowplot"           
#> crayon             "crayon"            
#> credentials        "credentials"       
#> crosstalk          "crosstalk"         
#> curl               "curl"              
#> data.table         "data.table"        
#> deSolve            "deSolve"           
#> desc               "desc"              
#> devtools           "devtools"          
#> diffobj            "diffobj"           
#> digest             "digest"            
#> doBy               "doBy"              
#> doParallel         "doParallel"        
#> docopt             "docopt"            
#> dotCall64          "dotCall64"         
#> downlit            "downlit"           
#> dplyr              "dplyr"             
#> edgeTransport      "edgeTransport"     
#> ellipsis           "ellipsis"          
#> elliptic           "elliptic"          
#> evaluate           "evaluate"          
#> fansi              "fansi"             
#> farver             "farver"            
#> fastmap            "fastmap"           
#> fields             "fields"            
#> filelock           "filelock"          
#> fontawesome        "fontawesome"       
#> forcats            "forcats"           
#> foreach            "foreach"           
#> forecast           "forecast"          
#> fracdiff           "fracdiff"          
#> fs                 "fs"                
#> gamstransfer       "gamstransfer"      
#> gdx                "gdx"               
#> gdx2               "gdx2"              
#> gdxdt              "gdxdt"             
#> gdxrrw             "gdxrrw"            
#> generics           "generics"          
#> geometry           "geometry"          
#> gert               "gert"              
#> ggplot2            "ggplot2"           
#> gh                 "gh"                
#> gitcreds           "gitcreds"          
#> glue               "glue"              
#> gms                "gms"               
#> goftest            "goftest"           
#> gridExtra          "gridExtra"         
#> gtable             "gtable"            
#> hdf5r              "hdf5r"             
#> highr              "highr"             
#> hms                "hms"               
#> htmlTable          "htmlTable"         
#> htmltools          "htmltools"         
#> htmlwidgets        "htmlwidgets"       
#> httpuv             "httpuv"            
#> httr               "httr"              
#> httr2              "httr2"             
#> hypergeo           "hypergeo"          
#> igraph             "igraph"            
#> ini                "ini"               
#> isoband            "isoband"           
#> iterators          "iterators"         
#> jquerylib          "jquerylib"         
#> jsonlite           "jsonlite"          
#> kableExtra         "kableExtra"        
#> knitr              "knitr"             
#> labeling           "labeling"          
#> later              "later"             
#> lazyeval           "lazyeval"          
#> lifecycle          "lifecycle"         
#> linprog            "linprog"           
#> lintr              "lintr"             
#> littler            "littler"           
#> lme4               "lme4"              
#> lmom               "lmom"              
#> lmomco             "lmomco"            
#> lmtest             "lmtest"            
#> lpSolve            "lpSolve"           
#> lpjclass           "lpjclass"          
#> lpjmlkit           "lpjmlkit"          
#> lubridate          "lubridate"         
#> lucode2            "lucode2"           
#> luplot             "luplot"            
#> luscale            "luscale"           
#> lusweave           "lusweave"          
#> madrat             "madrat"            
#> magclass           "magclass"          
#> magic              "magic"             
#> magpie4            "magpie4"           
#> magpiesets         "magpiesets"        
#> magrittr           "magrittr"          
#> maps               "maps"              
#> memoise            "memoise"           
#> microbenchmark     "microbenchmark"    
#> mime               "mime"              
#> miniUI             "miniUI"            
#> minqa              "minqa"             
#> minty              "minty"             
#> mip                "mip"               
#> modelr             "modelr"            
#> mrcommons          "mrcommons"         
#> mrdownscale        "mrdownscale"       
#> mrdrivers          "mrdrivers"         
#> mrfaocore          "mrfaocore"         
#> mrindustry         "mrindustry"        
#> mrlandcore         "mrlandcore"        
#> mrremind           "mrremind"          
#> mrtransport        "mrtransport"       
#> mstools            "mstools"           
#> ncdf4              "ncdf4"             
#> nleqslv            "nleqslv"           
#> nloptr             "nloptr"            
#> nnls               "nnls"              
#> nonparaeff         "nonparaeff"        
#> numDeriv           "numDeriv"          
#> openssl            "openssl"           
#> openxlsx           "openxlsx"          
#> otel               "otel"              
#> pak                "pak"               
#> pbkrtest           "pbkrtest"          
#> piamInterfaces     "piamInterfaces"    
#> piamPlotComparison "piamPlotComparison"
#> piamutils          "piamutils"         
#> pillar             "pillar"            
#> pkgbuild           "pkgbuild"          
#> pkgconfig          "pkgconfig"         
#> pkgdown            "pkgdown"           
#> pkgload            "pkgload"           
#> plotly             "plotly"            
#> plyr               "plyr"              
#> praise             "praise"            
#> prettyunits        "prettyunits"       
#> processx           "processx"          
#> profvis            "profvis"           
#> promises           "promises"          
#> ps                 "ps"                
#> purrr              "purrr"             
#> qualV              "qualV"             
#> quantreg           "quantreg"          
#> quitte             "quitte"            
#> ragg               "ragg"              
#> rappdirs           "rappdirs"          
#> raster             "raster"            
#> rbibutils          "rbibutils"         
#> rcmdcheck          "rcmdcheck"         
#> readODS            "readODS"           
#> readr              "readr"             
#> readxl             "readxl"            
#> reformulas         "reformulas"        
#> rematch            "rematch"           
#> remind2            "remind2"           
#> remulator          "remulator"         
#> renv               "renv"              
#> reporttransport    "reporttransport"   
#> reshape            "reshape"           
#> reshape2           "reshape2"          
#> rex                "rex"               
#> rlang              "rlang"             
#> rmarkdown          "rmarkdown"         
#> rmndt              "rmndt"             
#> rootSolve          "rootSolve"         
#> roxygen2           "roxygen2"          
#> rprojroot          "rprojroot"         
#> rstudioapi         "rstudioapi"        
#> rversions          "rversions"         
#> rworldmap          "rworldmap"         
#> sass               "sass"              
#> scales             "scales"            
#> sessioninfo        "sessioninfo"       
#> shiny              "shiny"             
#> sourcetools        "sourcetools"       
#> sp                 "sp"                
#> spam               "spam"              
#> stringi            "stringi"           
#> stringr            "stringr"           
#> svglite            "svglite"           
#> sys                "sys"               
#> systemfonts        "systemfonts"       
#> terra              "terra"             
#> testthat           "testthat"          
#> textshaping        "textshaping"       
#> tibble             "tibble"            
#> tidyr              "tidyr"             
#> tidyselect         "tidyselect"        
#> tidytemplate       "tidytemplate"      
#> timeDate           "timeDate"          
#> timechange         "timechange"        
#> tinytex            "tinytex"           
#> trafficlight       "trafficlight"      
#> tzdb               "tzdb"              
#> urca               "urca"              
#> urlchecker         "urlchecker"        
#> usethis            "usethis"           
#> utf8               "utf8"              
#> vctrs              "vctrs"             
#> viridisLite        "viridisLite"       
#> vroom              "vroom"             
#> waldo              "waldo"             
#> whisker            "whisker"           
#> whoami             "whoami"            
#> withr              "withr"             
#> writexl            "writexl"           
#> xfun               "xfun"              
#> xml2               "xml2"              
#> xmlparsedata       "xmlparsedata"      
#> xopen              "xopen"             
#> xtable             "xtable"            
#> yaml               "yaml"              
#> ymlthis            "ymlthis"           
#> zip                "zip"               
#> zoo                "zoo"               
#> KernSmooth         "KernSmooth"        
#> MASS               "MASS"              
#> Matrix             "Matrix"            
#> base               "base"              
#> boot               "boot"              
#> class              "class"             
#> cluster            "cluster"           
#> codetools          "codetools"         
#> compiler           "compiler"          
#> datasets           "datasets"          
#> foreign            "foreign"           
#> grDevices          "grDevices"         
#> graphics           "graphics"          
#> grid               "grid"              
#> lattice            "lattice"           
#> methods            "methods"           
#> mgcv               "mgcv"              
#> nlme               "nlme"              
#> nnet               "nnet"              
#> parallel           "parallel"          
#> rpart              "rpart"             
#> spatial            "spatial"           
#> splines            "splines"           
#> stats              "stats"             
#> stats4             "stats4"            
#> survival           "survival"          
#> tcltk              "tcltk"             
#> tools              "tools"             
#> utils              "utils"             
#>                    LibPath                         Version      
#> Deriv              "/usr/local/lib/R/site-library" "4.2.0"      
#> Formula            "/usr/local/lib/R/site-library" "1.2-5"      
#> GDPuc              "/usr/local/lib/R/site-library" "1.6.1"      
#> Hmisc              "/usr/local/lib/R/site-library" "5.2-5"      
#> Lmoments           "/usr/local/lib/R/site-library" "1.3-2"      
#> MatrixModels       "/usr/local/lib/R/site-library" "0.5-4"      
#> R.methodsS3        "/usr/local/lib/R/site-library" "1.8.2"      
#> R.oo               "/usr/local/lib/R/site-library" "1.27.1"     
#> R.utils            "/usr/local/lib/R/site-library" "2.13.0"     
#> R6                 "/usr/local/lib/R/site-library" "2.6.1"      
#> RColorBrewer       "/usr/local/lib/R/site-library" "1.1-3"      
#> Rcpp               "/usr/local/lib/R/site-library" "1.1.1-1.1"  
#> Rdpack             "/usr/local/lib/R/site-library" "2.6.6"      
#> S7                 "/usr/local/lib/R/site-library" "0.2.2"      
#> SPEI               "/usr/local/lib/R/site-library" "1.8.1"      
#> SparseM            "/usr/local/lib/R/site-library" "1.84-2"     
#> TLMoments          "/usr/local/lib/R/site-library" "0.7.5.3"    
#> abind              "/usr/local/lib/R/site-library" "1.4-8"      
#> askpass            "/usr/local/lib/R/site-library" "1.2.1"      
#> assertr            "/usr/local/lib/R/site-library" "3.0.1"      
#> backports          "/usr/local/lib/R/site-library" "1.5.1"      
#> base64enc          "/usr/local/lib/R/site-library" "0.1-6"      
#> bit                "/usr/local/lib/R/site-library" "4.6.0"      
#> bit64              "/usr/local/lib/R/site-library" "4.8.0"      
#> brew               "/usr/local/lib/R/site-library" "1.0-10"     
#> brio               "/usr/local/lib/R/site-library" "1.1.5"      
#> broom              "/usr/local/lib/R/site-library" "1.0.12"     
#> bslib              "/usr/local/lib/R/site-library" "0.10.0"     
#> cachem             "/usr/local/lib/R/site-library" "1.1.0"      
#> callr              "/usr/local/lib/R/site-library" "3.7.6"      
#> car                "/usr/local/lib/R/site-library" "3.1-5"      
#> carData            "/usr/local/lib/R/site-library" "3.0-6"      
#> cellranger         "/usr/local/lib/R/site-library" "1.1.0"      
#> checkmate          "/usr/local/lib/R/site-library" "2.3.4"      
#> citation           "/usr/local/lib/R/site-library" "0.12.2"     
#> cli                "/usr/local/lib/R/site-library" "3.6.6"      
#> clipr              "/usr/local/lib/R/site-library" "0.8.0"      
#> collections        "/usr/local/lib/R/site-library" "0.3.12"     
#> colorspace         "/usr/local/lib/R/site-library" "2.1-2"      
#> commonmark         "/usr/local/lib/R/site-library" "2.0.0"      
#> contfrac           "/usr/local/lib/R/site-library" "1.1-12"     
#> countrycode        "/usr/local/lib/R/site-library" "1.8.0"      
#> covr               "/usr/local/lib/R/site-library" "3.6.5"      
#> cowplot            "/usr/local/lib/R/site-library" "1.2.0"      
#> crayon             "/usr/local/lib/R/site-library" "1.5.3"      
#> credentials        "/usr/local/lib/R/site-library" "2.0.3"      
#> crosstalk          "/usr/local/lib/R/site-library" "1.2.2"      
#> curl               "/usr/local/lib/R/site-library" "7.1.0"      
#> data.table         "/usr/local/lib/R/site-library" "1.18.4"     
#> deSolve            "/usr/local/lib/R/site-library" "1.42"       
#> desc               "/usr/local/lib/R/site-library" "1.4.3"      
#> devtools           "/usr/local/lib/R/site-library" "2.5.2"      
#> diffobj            "/usr/local/lib/R/site-library" "0.3.6"      
#> digest             "/usr/local/lib/R/site-library" "0.6.39"     
#> doBy               "/usr/local/lib/R/site-library" "4.7.1"      
#> doParallel         "/usr/local/lib/R/site-library" "1.0.17"     
#> docopt             "/usr/local/lib/R/site-library" "0.7.2"      
#> dotCall64          "/usr/local/lib/R/site-library" "1.2"        
#> downlit            "/usr/local/lib/R/site-library" "0.4.5"      
#> dplyr              "/usr/local/lib/R/site-library" "1.2.1"      
#> edgeTransport      "/usr/local/lib/R/site-library" "3.13.4"     
#> ellipsis           "/usr/local/lib/R/site-library" "0.3.3"      
#> elliptic           "/usr/local/lib/R/site-library" "1.5-1"      
#> evaluate           "/usr/local/lib/R/site-library" "1.0.5"      
#> fansi              "/usr/local/lib/R/site-library" "1.0.7"      
#> farver             "/usr/local/lib/R/site-library" "2.1.2"      
#> fastmap            "/usr/local/lib/R/site-library" "1.2.0"      
#> fields             "/usr/local/lib/R/site-library" "17.3"       
#> filelock           "/usr/local/lib/R/site-library" "1.0.3"      
#> fontawesome        "/usr/local/lib/R/site-library" "0.5.3"      
#> forcats            "/usr/local/lib/R/site-library" "1.0.1"      
#> foreach            "/usr/local/lib/R/site-library" "1.5.2"      
#> forecast           "/usr/local/lib/R/site-library" "9.0.2"      
#> fracdiff           "/usr/local/lib/R/site-library" "1.5-4"      
#> fs                 "/usr/local/lib/R/site-library" "2.1.0"      
#> gamstransfer       "/usr/local/lib/R/site-library" "3.0.8"      
#> gdx                "/usr/local/lib/R/site-library" "1.53.1"     
#> gdx2               "/usr/local/lib/R/site-library" "0.5.0"      
#> gdxdt              "/usr/local/lib/R/site-library" "0.1.0"      
#> gdxrrw             "/usr/local/lib/R/site-library" "1.0.10.9001"
#> generics           "/usr/local/lib/R/site-library" "0.1.4"      
#> geometry           "/usr/local/lib/R/site-library" "0.5.2"      
#> gert               "/usr/local/lib/R/site-library" "2.3.1"      
#> ggplot2            "/usr/local/lib/R/site-library" "4.0.3"      
#> gh                 "/usr/local/lib/R/site-library" "1.5.0"      
#> gitcreds           "/usr/local/lib/R/site-library" "0.1.2"      
#> glue               "/usr/local/lib/R/site-library" "1.8.1"      
#> gms                "/usr/local/lib/R/site-library" "0.31.2"     
#> goftest            "/usr/local/lib/R/site-library" "1.2-3"      
#> gridExtra          "/usr/local/lib/R/site-library" "2.3"        
#> gtable             "/usr/local/lib/R/site-library" "0.3.6"      
#> hdf5r              "/usr/local/lib/R/site-library" "1.3.12"     
#> highr              "/usr/local/lib/R/site-library" "0.12"       
#> hms                "/usr/local/lib/R/site-library" "1.1.4"      
#> htmlTable          "/usr/local/lib/R/site-library" "2.5.0"      
#> htmltools          "/usr/local/lib/R/site-library" "0.5.9"      
#> htmlwidgets        "/usr/local/lib/R/site-library" "1.6.4"      
#> httpuv             "/usr/local/lib/R/site-library" "1.6.17"     
#> httr               "/usr/local/lib/R/site-library" "1.4.8"      
#> httr2              "/usr/local/lib/R/site-library" "1.2.2"      
#> hypergeo           "/usr/local/lib/R/site-library" "1.2-14"     
#> igraph             "/usr/local/lib/R/site-library" "2.3.1"      
#> ini                "/usr/local/lib/R/site-library" "0.3.1"      
#> isoband            "/usr/local/lib/R/site-library" "0.3.0"      
#> iterators          "/usr/local/lib/R/site-library" "1.0.14"     
#> jquerylib          "/usr/local/lib/R/site-library" "0.1.4"      
#> jsonlite           "/usr/local/lib/R/site-library" "2.0.0"      
#> kableExtra         "/usr/local/lib/R/site-library" "1.4.0"      
#> knitr              "/usr/local/lib/R/site-library" "1.51"       
#> labeling           "/usr/local/lib/R/site-library" "0.4.3"      
#> later              "/usr/local/lib/R/site-library" "1.4.8"      
#> lazyeval           "/usr/local/lib/R/site-library" "0.2.3"      
#> lifecycle          "/usr/local/lib/R/site-library" "1.0.5"      
#> linprog            "/usr/local/lib/R/site-library" "0.9-6"      
#> lintr              "/usr/local/lib/R/site-library" "3.3.0-1"    
#> littler            "/usr/local/lib/R/site-library" "0.3.23"     
#> lme4               "/usr/local/lib/R/site-library" "2.0-1"      
#> lmom               "/usr/local/lib/R/site-library" "3.3"        
#> lmomco             "/usr/local/lib/R/site-library" "2.5.5"      
#> lmtest             "/usr/local/lib/R/site-library" "0.9-40"     
#> lpSolve            "/usr/local/lib/R/site-library" "5.6.23"     
#> lpjclass           "/usr/local/lib/R/site-library" "1.19.7"     
#> lpjmlkit           "/usr/local/lib/R/site-library" "1.8.1"      
#> lubridate          "/usr/local/lib/R/site-library" "1.9.5"      
#> lucode2            "/usr/local/lib/R/site-library" "0.55.2"     
#> luplot             "/usr/local/lib/R/site-library" "4.1.4"      
#> luscale            "/usr/local/lib/R/site-library" "3.2.0"      
#> lusweave           "/usr/local/lib/R/site-library" "1.46.6"     
#> madrat             "/usr/local/lib/R/site-library" "3.36.2"     
#> magclass           "/usr/local/lib/R/site-library" "7.4.3"      
#> magic              "/usr/local/lib/R/site-library" "1.6-1"      
#> magpie4            "/usr/local/lib/R/site-library" "2.75.1"     
#> magpiesets         "/usr/local/lib/R/site-library" "0.48.0"     
#> magrittr           "/usr/local/lib/R/site-library" "2.0.5"      
#> maps               "/usr/local/lib/R/site-library" "3.4.3"      
#> memoise            "/usr/local/lib/R/site-library" "2.0.1"      
#> microbenchmark     "/usr/local/lib/R/site-library" "1.5.0"      
#> mime               "/usr/local/lib/R/site-library" "0.13"       
#> miniUI             "/usr/local/lib/R/site-library" "0.1.2"      
#> minqa              "/usr/local/lib/R/site-library" "1.2.8"      
#> minty              "/usr/local/lib/R/site-library" "0.0.6"      
#> mip                "/usr/local/lib/R/site-library" "0.155.11"   
#> modelr             "/usr/local/lib/R/site-library" "0.1.11"     
#> mrcommons          "/usr/local/lib/R/site-library" "1.67.1"     
#> mrdownscale        "/usr/local/lib/R/site-library" "0.50.0"     
#> mrdrivers          "/usr/local/lib/R/site-library" "7.2.0"      
#> mrfaocore          "/usr/local/lib/R/site-library" "1.7.0"      
#> mrindustry         "/usr/local/lib/R/site-library" "1.1.5"      
#> mrlandcore         "/usr/local/lib/R/site-library" "1.7.0"      
#> mrremind           "/usr/local/lib/R/site-library" "0.265.4"    
#> mrtransport        "/usr/local/lib/R/site-library" "0.14.0"     
#> mstools            "/usr/local/lib/R/site-library" "0.15.0"     
#> ncdf4              "/usr/local/lib/R/site-library" "1.24"       
#> nleqslv            "/usr/local/lib/R/site-library" "3.3.7"      
#> nloptr             "/usr/local/lib/R/site-library" "2.2.1"      
#> nnls               "/usr/local/lib/R/site-library" "1.6"        
#> nonparaeff         "/usr/local/lib/R/site-library" "0.5-15"     
#> numDeriv           "/usr/local/lib/R/site-library" "2016.8-1.1" 
#> openssl            "/usr/local/lib/R/site-library" "2.4.0"      
#> openxlsx           "/usr/local/lib/R/site-library" "4.2.8.1"    
#> otel               "/usr/local/lib/R/site-library" "0.2.0"      
#> pak                "/usr/local/lib/R/site-library" "0.9.5"      
#> pbkrtest           "/usr/local/lib/R/site-library" "0.5.5"      
#> piamInterfaces     "/usr/local/lib/R/site-library" "0.60.0"     
#> piamPlotComparison "/usr/local/lib/R/site-library" "0.1.5"      
#> piamutils          "/usr/local/lib/R/site-library" "0.1.1"      
#> pillar             "/usr/local/lib/R/site-library" "1.11.1"     
#> pkgbuild           "/usr/local/lib/R/site-library" "1.4.8"      
#> pkgconfig          "/usr/local/lib/R/site-library" "2.0.3"      
#> pkgdown            "/usr/local/lib/R/site-library" "2.2.0"      
#> pkgload            "/usr/local/lib/R/site-library" "1.5.2"      
#> plotly             "/usr/local/lib/R/site-library" "4.12.0"     
#> plyr               "/usr/local/lib/R/site-library" "1.8.9"      
#> praise             "/usr/local/lib/R/site-library" "1.0.0"      
#> prettyunits        "/usr/local/lib/R/site-library" "1.2.0"      
#> processx           "/usr/local/lib/R/site-library" "3.9.0"      
#> profvis            "/usr/local/lib/R/site-library" "0.4.0"      
#> promises           "/usr/local/lib/R/site-library" "1.5.0"      
#> ps                 "/usr/local/lib/R/site-library" "1.9.3"      
#> purrr              "/usr/local/lib/R/site-library" "1.2.2"      
#> qualV              "/usr/local/lib/R/site-library" "0.3-5"      
#> quantreg           "/usr/local/lib/R/site-library" "6.1"        
#> quitte             "/usr/local/lib/R/site-library" "0.3148.0"   
#> ragg               "/usr/local/lib/R/site-library" "1.5.2"      
#> rappdirs           "/usr/local/lib/R/site-library" "0.3.4"      
#> raster             "/usr/local/lib/R/site-library" "3.6-32"     
#> rbibutils          "/usr/local/lib/R/site-library" "2.4.1"      
#> rcmdcheck          "/usr/local/lib/R/site-library" "1.4.0"      
#> readODS            "/usr/local/lib/R/site-library" "2.3.5"      
#> readr              "/usr/local/lib/R/site-library" "2.2.0"      
#> readxl             "/usr/local/lib/R/site-library" "1.4.5"      
#> reformulas         "/usr/local/lib/R/site-library" "0.4.4"      
#> rematch            "/usr/local/lib/R/site-library" "2.0.0"      
#> remind2            "/usr/local/lib/R/site-library" "2.0.17"     
#> remulator          "/usr/local/lib/R/site-library" "1.22.0"     
#> renv               "/usr/local/lib/R/site-library" "1.2.2"      
#> reporttransport    "/usr/local/lib/R/site-library" "1.3.1"      
#> reshape            "/usr/local/lib/R/site-library" "0.8.10"     
#> reshape2           "/usr/local/lib/R/site-library" "1.4.5"      
#> rex                "/usr/local/lib/R/site-library" "1.2.2"      
#> rlang              "/usr/local/lib/R/site-library" "1.2.0"      
#> rmarkdown          "/usr/local/lib/R/site-library" "2.31"       
#> rmndt              "/usr/local/lib/R/site-library" "0.6.0"      
#> rootSolve          "/usr/local/lib/R/site-library" "1.8.2.4"    
#> roxygen2           "/usr/local/lib/R/site-library" "8.0.0"      
#> rprojroot          "/usr/local/lib/R/site-library" "2.1.1"      
#> rstudioapi         "/usr/local/lib/R/site-library" "0.18.0"     
#> rversions          "/usr/local/lib/R/site-library" "3.0.0"      
#> rworldmap          "/usr/local/lib/R/site-library" "1.3-8"      
#> sass               "/usr/local/lib/R/site-library" "0.4.10"     
#> scales             "/usr/local/lib/R/site-library" "1.4.0"      
#> sessioninfo        "/usr/local/lib/R/site-library" "1.2.3"      
#> shiny              "/usr/local/lib/R/site-library" "1.13.0"     
#> sourcetools        "/usr/local/lib/R/site-library" "0.1.7-2"    
#> sp                 "/usr/local/lib/R/site-library" "2.2-1"      
#> spam               "/usr/local/lib/R/site-library" "2.11-3"     
#> stringi            "/usr/local/lib/R/site-library" "1.8.7"      
#> stringr            "/usr/local/lib/R/site-library" "1.6.0"      
#> svglite            "/usr/local/lib/R/site-library" "2.2.2"      
#> sys                "/usr/local/lib/R/site-library" "3.4.3"      
#> systemfonts        "/usr/local/lib/R/site-library" "1.3.2"      
#> terra              "/usr/local/lib/R/site-library" "1.9-27"     
#> testthat           "/usr/local/lib/R/site-library" "3.3.2"      
#> textshaping        "/usr/local/lib/R/site-library" "1.0.5"      
#> tibble             "/usr/local/lib/R/site-library" "3.3.1"      
#> tidyr              "/usr/local/lib/R/site-library" "1.3.2"      
#> tidyselect         "/usr/local/lib/R/site-library" "1.2.1"      
#> tidytemplate       "/usr/local/lib/R/site-library" "1.0.0"      
#> timeDate           "/usr/local/lib/R/site-library" "4052.112"   
#> timechange         "/usr/local/lib/R/site-library" "0.4.0"      
#> tinytex            "/usr/local/lib/R/site-library" "0.59"       
#> trafficlight       "/usr/local/lib/R/site-library" "1.15.1"     
#> tzdb               "/usr/local/lib/R/site-library" "0.5.0"      
#> urca               "/usr/local/lib/R/site-library" "1.3-4"      
#> urlchecker         "/usr/local/lib/R/site-library" "1.0.1"      
#> usethis            "/usr/local/lib/R/site-library" "3.2.1"      
#> utf8               "/usr/local/lib/R/site-library" "1.2.6"      
#> vctrs              "/usr/local/lib/R/site-library" "0.7.3"      
#> viridisLite        "/usr/local/lib/R/site-library" "0.4.3"      
#> vroom              "/usr/local/lib/R/site-library" "1.7.1"      
#> waldo              "/usr/local/lib/R/site-library" "0.6.2"      
#> whisker            "/usr/local/lib/R/site-library" "0.4.1"      
#> whoami             "/usr/local/lib/R/site-library" "1.3.0"      
#> withr              "/usr/local/lib/R/site-library" "3.0.2"      
#> writexl            "/usr/local/lib/R/site-library" "1.5.4"      
#> xfun               "/usr/local/lib/R/site-library" "0.57"       
#> xml2               "/usr/local/lib/R/site-library" "1.5.2"      
#> xmlparsedata       "/usr/local/lib/R/site-library" "1.0.5"      
#> xopen              "/usr/local/lib/R/site-library" "1.0.1"      
#> xtable             "/usr/local/lib/R/site-library" "1.8-8"      
#> yaml               "/usr/local/lib/R/site-library" "2.3.12"     
#> ymlthis            "/usr/local/lib/R/site-library" "1.0.0"      
#> zip                "/usr/local/lib/R/site-library" "2.3.3"      
#> zoo                "/usr/local/lib/R/site-library" "1.8-15"     
#> KernSmooth         "/usr/local/lib/R/library"      "2.23-26"    
#> MASS               "/usr/local/lib/R/library"      "7.3-65"     
#> Matrix             "/usr/local/lib/R/library"      "1.7-4"      
#> base               "/usr/local/lib/R/library"      "4.5.3"      
#> boot               "/usr/local/lib/R/library"      "1.3-32"     
#> class              "/usr/local/lib/R/library"      "7.3-23"     
#> cluster            "/usr/local/lib/R/library"      "2.1.8.2"    
#> codetools          "/usr/local/lib/R/library"      "0.2-20"     
#> compiler           "/usr/local/lib/R/library"      "4.5.3"      
#> datasets           "/usr/local/lib/R/library"      "4.5.3"      
#> foreign            "/usr/local/lib/R/library"      "0.8-91"     
#> grDevices          "/usr/local/lib/R/library"      "4.5.3"      
#> graphics           "/usr/local/lib/R/library"      "4.5.3"      
#> grid               "/usr/local/lib/R/library"      "4.5.3"      
#> lattice            "/usr/local/lib/R/library"      "0.22-9"     
#> methods            "/usr/local/lib/R/library"      "4.5.3"      
#> mgcv               "/usr/local/lib/R/library"      "1.9-4"      
#> nlme               "/usr/local/lib/R/library"      "3.1-168"    
#> nnet               "/usr/local/lib/R/library"      "7.3-20"     
#> parallel           "/usr/local/lib/R/library"      "4.5.3"      
#> rpart              "/usr/local/lib/R/library"      "4.1.24"     
#> spatial            "/usr/local/lib/R/library"      "7.3-18"     
#> splines            "/usr/local/lib/R/library"      "4.5.3"      
#> stats              "/usr/local/lib/R/library"      "4.5.3"      
#> stats4             "/usr/local/lib/R/library"      "4.5.3"      
#> survival           "/usr/local/lib/R/library"      "3.8-6"      
#> tcltk              "/usr/local/lib/R/library"      "4.5.3"      
#> tools              "/usr/local/lib/R/library"      "4.5.3"      
#> utils              "/usr/local/lib/R/library"      "4.5.3"      
#>                    Priority     
#> Deriv              NA           
#> Formula            NA           
#> GDPuc              NA           
#> Hmisc              NA           
#> Lmoments           NA           
#> MatrixModels       NA           
#> R.methodsS3        NA           
#> R.oo               NA           
#> R.utils            NA           
#> R6                 NA           
#> RColorBrewer       NA           
#> Rcpp               NA           
#> Rdpack             NA           
#> S7                 NA           
#> SPEI               NA           
#> SparseM            NA           
#> TLMoments          NA           
#> abind              NA           
#> askpass            NA           
#> assertr            NA           
#> backports          NA           
#> base64enc          NA           
#> bit                NA           
#> bit64              NA           
#> brew               NA           
#> brio               NA           
#> broom              NA           
#> bslib              NA           
#> cachem             NA           
#> callr              NA           
#> car                NA           
#> carData            NA           
#> cellranger         NA           
#> checkmate          NA           
#> citation           NA           
#> cli                NA           
#> clipr              NA           
#> collections        NA           
#> colorspace         NA           
#> commonmark         NA           
#> contfrac           NA           
#> countrycode        NA           
#> covr               NA           
#> cowplot            NA           
#> crayon             NA           
#> credentials        NA           
#> crosstalk          NA           
#> curl               NA           
#> data.table         NA           
#> deSolve            NA           
#> desc               NA           
#> devtools           NA           
#> diffobj            NA           
#> digest             NA           
#> doBy               NA           
#> doParallel         NA           
#> docopt             NA           
#> dotCall64          NA           
#> downlit            NA           
#> dplyr              NA           
#> edgeTransport      NA           
#> ellipsis           NA           
#> elliptic           NA           
#> evaluate           NA           
#> fansi              NA           
#> farver             NA           
#> fastmap            NA           
#> fields             NA           
#> filelock           NA           
#> fontawesome        NA           
#> forcats            NA           
#> foreach            NA           
#> forecast           NA           
#> fracdiff           NA           
#> fs                 NA           
#> gamstransfer       NA           
#> gdx                NA           
#> gdx2               NA           
#> gdxdt              NA           
#> gdxrrw             NA           
#> generics           NA           
#> geometry           NA           
#> gert               NA           
#> ggplot2            NA           
#> gh                 NA           
#> gitcreds           NA           
#> glue               NA           
#> gms                NA           
#> goftest            NA           
#> gridExtra          NA           
#> gtable             NA           
#> hdf5r              NA           
#> highr              NA           
#> hms                NA           
#> htmlTable          NA           
#> htmltools          NA           
#> htmlwidgets        NA           
#> httpuv             NA           
#> httr               NA           
#> httr2              NA           
#> hypergeo           NA           
#> igraph             NA           
#> ini                NA           
#> isoband            NA           
#> iterators          NA           
#> jquerylib          NA           
#> jsonlite           NA           
#> kableExtra         NA           
#> knitr              NA           
#> labeling           NA           
#> later              NA           
#> lazyeval           NA           
#> lifecycle          NA           
#> linprog            NA           
#> lintr              NA           
#> littler            NA           
#> lme4               NA           
#> lmom               NA           
#> lmomco             NA           
#> lmtest             NA           
#> lpSolve            NA           
#> lpjclass           NA           
#> lpjmlkit           NA           
#> lubridate          NA           
#> lucode2            NA           
#> luplot             NA           
#> luscale            NA           
#> lusweave           NA           
#> madrat             NA           
#> magclass           NA           
#> magic              NA           
#> magpie4            NA           
#> magpiesets         NA           
#> magrittr           NA           
#> maps               NA           
#> memoise            NA           
#> microbenchmark     NA           
#> mime               NA           
#> miniUI             NA           
#> minqa              NA           
#> minty              NA           
#> mip                NA           
#> modelr             NA           
#> mrcommons          NA           
#> mrdownscale        NA           
#> mrdrivers          NA           
#> mrfaocore          NA           
#> mrindustry         NA           
#> mrlandcore         NA           
#> mrremind           NA           
#> mrtransport        NA           
#> mstools            NA           
#> ncdf4              NA           
#> nleqslv            NA           
#> nloptr             NA           
#> nnls               NA           
#> nonparaeff         NA           
#> numDeriv           NA           
#> openssl            NA           
#> openxlsx           NA           
#> otel               NA           
#> pak                NA           
#> pbkrtest           NA           
#> piamInterfaces     NA           
#> piamPlotComparison NA           
#> piamutils          NA           
#> pillar             NA           
#> pkgbuild           NA           
#> pkgconfig          NA           
#> pkgdown            NA           
#> pkgload            NA           
#> plotly             NA           
#> plyr               NA           
#> praise             NA           
#> prettyunits        NA           
#> processx           NA           
#> profvis            NA           
#> promises           NA           
#> ps                 NA           
#> purrr              NA           
#> qualV              NA           
#> quantreg           NA           
#> quitte             NA           
#> ragg               NA           
#> rappdirs           NA           
#> raster             NA           
#> rbibutils          NA           
#> rcmdcheck          NA           
#> readODS            NA           
#> readr              NA           
#> readxl             NA           
#> reformulas         NA           
#> rematch            NA           
#> remind2            NA           
#> remulator          NA           
#> renv               NA           
#> reporttransport    NA           
#> reshape            NA           
#> reshape2           NA           
#> rex                NA           
#> rlang              NA           
#> rmarkdown          NA           
#> rmndt              NA           
#> rootSolve          NA           
#> roxygen2           NA           
#> rprojroot          NA           
#> rstudioapi         NA           
#> rversions          NA           
#> rworldmap          NA           
#> sass               NA           
#> scales             NA           
#> sessioninfo        NA           
#> shiny              NA           
#> sourcetools        NA           
#> sp                 NA           
#> spam               NA           
#> stringi            NA           
#> stringr            NA           
#> svglite            NA           
#> sys                NA           
#> systemfonts        NA           
#> terra              NA           
#> testthat           NA           
#> textshaping        NA           
#> tibble             NA           
#> tidyr              NA           
#> tidyselect         NA           
#> tidytemplate       NA           
#> timeDate           NA           
#> timechange         NA           
#> tinytex            NA           
#> trafficlight       NA           
#> tzdb               NA           
#> urca               NA           
#> urlchecker         NA           
#> usethis            NA           
#> utf8               NA           
#> vctrs              NA           
#> viridisLite        NA           
#> vroom              NA           
#> waldo              NA           
#> whisker            NA           
#> whoami             NA           
#> withr              NA           
#> writexl            NA           
#> xfun               NA           
#> xml2               NA           
#> xmlparsedata       NA           
#> xopen              NA           
#> xtable             NA           
#> yaml               NA           
#> ymlthis            NA           
#> zip                NA           
#> zoo                NA           
#> KernSmooth         "recommended"
#> MASS               "recommended"
#> Matrix             "recommended"
#> base               "base"       
#> boot               "recommended"
#> class              "recommended"
#> cluster            "recommended"
#> codetools          "recommended"
#> compiler           "base"       
#> datasets           "base"       
#> foreign            "recommended"
#> grDevices          "base"       
#> graphics           "base"       
#> grid               "base"       
#> lattice            "recommended"
#> methods            "base"       
#> mgcv               "recommended"
#> nlme               "recommended"
#> nnet               "recommended"
#> parallel           "base"       
#> rpart              "recommended"
#> spatial            "recommended"
#> splines            "base"       
#> stats              "base"       
#> stats4             "base"       
#> survival           "recommended"
#> tcltk              "base"       
#> tools              "base"       
#> utils              "base"       
#>                    Depends                                                                                                                                                                    
#> Deriv              NA                                                                                                                                                                         
#> Formula            "R (>= 2.0.0), stats"                                                                                                                                                      
#> GDPuc              "R (>= 2.10)"                                                                                                                                                              
#> Hmisc              "R (>= 4.2.0)"                                                                                                                                                             
#> Lmoments           NA                                                                                                                                                                         
#> MatrixModels       "R (>= 3.6.0)"                                                                                                                                                             
#> R.methodsS3        "R (>= 2.13.0)"                                                                                                                                                            
#> R.oo               "R (>= 2.13.0), R.methodsS3 (>= 1.8.2)"                                                                                                                                    
#> R.utils            "R (>= 2.14.0), R.oo"                                                                                                                                                      
#> R6                 "R (>= 3.6)"                                                                                                                                                               
#> RColorBrewer       "R (>= 2.0.0)"                                                                                                                                                             
#> Rcpp               "R (>= 3.5.0)"                                                                                                                                                             
#> Rdpack             "R (>= 2.15.0), methods"                                                                                                                                                   
#> S7                 "R (>= 3.5.0)"                                                                                                                                                             
#> SPEI               "R (>= 3.5.0)"                                                                                                                                                             
#> SparseM            "R (>= 2.15), methods"                                                                                                                                                     
#> TLMoments          "R (>= 2.10), Rcpp (>= 0.12.12)"                                                                                                                                           
#> abind              "R (>= 1.5.0)"                                                                                                                                                             
#> askpass            NA                                                                                                                                                                         
#> assertr            "R (>= 3.1.0)"                                                                                                                                                             
#> backports          "R (>= 3.0.0)"                                                                                                                                                             
#> base64enc          "R (>= 2.9.0)"                                                                                                                                                             
#> bit                "R (>= 3.4.0)"                                                                                                                                                             
#> bit64              "R (>= 3.5.0)"                                                                                                                                                             
#> brew               NA                                                                                                                                                                         
#> brio               "R (>= 3.6)"                                                                                                                                                               
#> broom              "R (>= 4.1)"                                                                                                                                                               
#> bslib              "R (>= 2.10)"                                                                                                                                                              
#> cachem             NA                                                                                                                                                                         
#> callr              "R (>= 3.4)"                                                                                                                                                               
#> car                "R (>= 3.5.0), carData (>= 3.0-0)"                                                                                                                                         
#> carData            "R (>= 3.5.0)"                                                                                                                                                             
#> cellranger         "R (>= 3.0.0)"                                                                                                                                                             
#> checkmate          "R (>= 3.0.0)"                                                                                                                                                             
#> citation           NA                                                                                                                                                                         
#> cli                "R (>= 3.4)"                                                                                                                                                               
#> clipr              NA                                                                                                                                                                         
#> collections        NA                                                                                                                                                                         
#> colorspace         "R (>= 3.0.0), methods"                                                                                                                                                    
#> commonmark         NA                                                                                                                                                                         
#> contfrac           NA                                                                                                                                                                         
#> countrycode        "R (>= 2.10)"                                                                                                                                                              
#> covr               "R (>= 3.1.0), methods"                                                                                                                                                    
#> cowplot            "R (>= 3.5.0)"                                                                                                                                                             
#> crayon             NA                                                                                                                                                                         
#> credentials        NA                                                                                                                                                                         
#> crosstalk          NA                                                                                                                                                                         
#> curl               "R (>= 3.0.0)"                                                                                                                                                             
#> data.table         "R (>= 3.4.0)"                                                                                                                                                             
#> deSolve            "R (>= 3.3.0)"                                                                                                                                                             
#> desc               "R (>= 3.4)"                                                                                                                                                               
#> devtools           "R (>= 4.1), usethis (>= 3.2.1)"                                                                                                                                           
#> diffobj            "R (>= 3.1.0)"                                                                                                                                                             
#> digest             "R (>= 3.3.0)"                                                                                                                                                             
#> doBy               "R (>= 4.2.0), methods"                                                                                                                                                    
#> doParallel         "R (>= 2.14.0), foreach (>= 1.2.0), iterators (>= 1.0.0),\nparallel, utils"                                                                                                
#> docopt             NA                                                                                                                                                                         
#> dotCall64          "R (>= 4.0)"                                                                                                                                                               
#> downlit            "R (>= 4.0.0)"                                                                                                                                                             
#> dplyr              "R (>= 4.1.0)"                                                                                                                                                             
#> edgeTransport      "R (>= 4.1.0), data.table (>= 1.11.0), mrtransport (>= 0.12.0)"                                                                                                            
#> ellipsis           "R (>= 3.2)"                                                                                                                                                               
#> elliptic           "R (>= 2.5.0)"                                                                                                                                                             
#> evaluate           "R (>= 3.6.0)"                                                                                                                                                             
#> fansi              "R (>= 3.1.0)"                                                                                                                                                             
#> farver             NA                                                                                                                                                                         
#> fastmap            NA                                                                                                                                                                         
#> fields             "R (>= 4.0.0), methods, spam, viridisLite, RColorBrewer"                                                                                                                   
#> filelock           "R (>= 3.4)"                                                                                                                                                               
#> fontawesome        "R (>= 3.3.0)"                                                                                                                                                             
#> forcats            "R (>= 4.1)"                                                                                                                                                               
#> foreach            "R (>= 2.5.0)"                                                                                                                                                             
#> forecast           "R (>= 4.1.0)"                                                                                                                                                             
#> fracdiff           NA                                                                                                                                                                         
#> fs                 "R (>= 4.1)"                                                                                                                                                               
#> gamstransfer       NA                                                                                                                                                                         
#> gdx                "gdxrrw (>= 1.0.2), magclass (>= 2.43)"                                                                                                                                    
#> gdx2               NA                                                                                                                                                                         
#> gdxdt              "R (>= 3.1), data.table (>= 1.11.0),"                                                                                                                                      
#> gdxrrw             "R (>= 3.0)"                                                                                                                                                               
#> generics           "R (>= 3.6)"                                                                                                                                                               
#> geometry           "R (>= 3.5.0)"                                                                                                                                                             
#> gert               NA                                                                                                                                                                         
#> ggplot2            "R (>= 4.1)"                                                                                                                                                               
#> gh                 "R (>= 4.1)"                                                                                                                                                               
#> gitcreds           "R (>= 3.4)"                                                                                                                                                               
#> glue               "R (>= 4.1)"                                                                                                                                                               
#> gms                NA                                                                                                                                                                         
#> goftest            "R (>= 3.3)"                                                                                                                                                               
#> gridExtra          NA                                                                                                                                                                         
#> gtable             "R (>= 4.0)"                                                                                                                                                               
#> hdf5r              "R (>= 3.2.2), methods"                                                                                                                                                    
#> highr              "R (>= 3.3.0)"                                                                                                                                                             
#> hms                NA                                                                                                                                                                         
#> htmlTable          "R (>= 4.1)"                                                                                                                                                               
#> htmltools          "R (>= 2.14.1)"                                                                                                                                                            
#> htmlwidgets        NA                                                                                                                                                                         
#> httpuv             "R (>= 2.15.1)"                                                                                                                                                            
#> httr               "R (>= 3.6)"                                                                                                                                                               
#> httr2              "R (>= 4.1)"                                                                                                                                                               
#> hypergeo           "R (>= 3.1.0),"                                                                                                                                                            
#> igraph             "methods, R (>= 3.5.0)"                                                                                                                                                    
#> ini                NA                                                                                                                                                                         
#> isoband            NA                                                                                                                                                                         
#> iterators          "R (>= 2.5.0), utils"                                                                                                                                                      
#> jquerylib          NA                                                                                                                                                                         
#> jsonlite           "methods"                                                                                                                                                                  
#> kableExtra         "R (>= 3.1.0)"                                                                                                                                                             
#> knitr              "R (>= 3.6.0)"                                                                                                                                                             
#> labeling           NA                                                                                                                                                                         
#> later              "R (>= 3.5)"                                                                                                                                                               
#> lazyeval           "R (>= 3.1.0)"                                                                                                                                                             
#> lifecycle          "R (>= 3.6)"                                                                                                                                                               
#> linprog            "R (>= 2.4.0), lpSolve"                                                                                                                                                    
#> lintr              "R (>= 4.0)"                                                                                                                                                               
#> littler            NA                                                                                                                                                                         
#> lme4               "R (>= 3.6), Matrix, methods, stats"                                                                                                                                       
#> lmom               "R (>= 3.0.0)"                                                                                                                                                             
#> lmomco             "R (>= 3.5.0), utils"                                                                                                                                                      
#> lmtest             "R (>= 3.0.0), stats, zoo"                                                                                                                                                 
#> lpSolve            NA                                                                                                                                                                         
#> lpjclass           "R (>= 2.10), methods"                                                                                                                                                     
#> lpjmlkit           "R (>= 4.1.0)"                                                                                                                                                             
#> lubridate          "methods, R (>= 3.2)"                                                                                                                                                      
#> lucode2            NA                                                                                                                                                                         
#> luplot             "magclass, R (>= 2.15.1)"                                                                                                                                                  
#> luscale            "magclass, methods, R (>= 2.10.0)"                                                                                                                                         
#> lusweave           "methods, R (>= 2.10.0)"                                                                                                                                                   
#> madrat             "R (>= 4.1.0), magclass (>= 7.2.0)"                                                                                                                                        
#> magclass           "methods, R (>= 2.10.0)"                                                                                                                                                   
#> magic              "R (>= 2.10), abind"                                                                                                                                                       
#> magpie4            "R (>= 4.1.0), magclass (>= 2.40)"                                                                                                                                         
#> magpiesets         "magclass (>= 6.0.3)"                                                                                                                                                      
#> magrittr           "R (>= 3.4.0)"                                                                                                                                                             
#> maps               "R (>= 3.5.0)"                                                                                                                                                             
#> memoise            NA                                                                                                                                                                         
#> microbenchmark     "R (>= 3.2.0)"                                                                                                                                                             
#> mime               NA                                                                                                                                                                         
#> miniUI             NA                                                                                                                                                                         
#> minqa              NA                                                                                                                                                                         
#> minty              "R (>= 4.0)"                                                                                                                                                               
#> mip                "R (>= 2.10.0), magclass, quitte (>= 0.3072)"                                                                                                                              
#> modelr             "R (>= 3.2)"                                                                                                                                                               
#> mrcommons          "R (>= 4.1.0), GDPuc (>= 1.3.0), madrat (>= 3.10.0), magclass\n(>= 3.17), mrdrivers (>= 7.1.1), mrfaocore (>= 1.0.0),\nmrlandcore (>= 1.0.0), mstools (>= 0.6.0)"          
#> mrdownscale        "madrat (>= 3.27.0), magclass (>= 7.0.0), mstools (>= 0.8.0), R\n(>= 4.0.0)"                                                                                               
#> mrdrivers          "madrat (>= 2.5.1), magclass (>= 6.0.3)"                                                                                                                                   
#> mrfaocore          "madrat (>= 2.20.9), magclass (>= 3.17), mrdrivers (>= 1.0.0),\nmstools (>= 0.6.0), GDPuc (>= 1.3.0), R (>= 4.1.0)"                                                        
#> mrindustry         "R (>= 4.1.0)"                                                                                                                                                             
#> mrlandcore         "madrat (>= 2.20.9), magclass (>= 3.17), mrdownscale (>=\n0.43.0), mrdrivers (>= 1.0.0), mrfaocore (>= 1.0.0), mstools\n(>= 0.6.0), R (>= 2.10.0)"                         
#> mrremind           "R (>= 4.1.0), edgeTransport (>= 3.5.0), GDPuc (>= 1.3.0),\nmadrat (>= 3.7.1), magclass (>= 6.16.1), mrcommons (>=\n1.44.12), mrdrivers (>= 6.0.0), mrindustry (>= 0.18.5)"
#> mrtransport        "R (>= 4.1.0), madrat (>= 3.7.1), mrcommons, mrdrivers (>=\n2.0.0)"                                                                                                        
#> mstools            "madrat, magclass (>= 2.40)"                                                                                                                                               
#> ncdf4              NA                                                                                                                                                                         
#> nleqslv            NA                                                                                                                                                                         
#> nloptr             NA                                                                                                                                                                         
#> nnls               NA                                                                                                                                                                         
#> nonparaeff         NA                                                                                                                                                                         
#> numDeriv           "R (>= 2.11.1)"                                                                                                                                                            
#> openssl            NA                                                                                                                                                                         
#> openxlsx           "R (>= 3.3.0)"                                                                                                                                                             
#> otel               "R (>= 3.6.0)"                                                                                                                                                             
#> pak                "R (>= 3.5)"                                                                                                                                                               
#> pbkrtest           "R (>= 4.2.0), lme4 (>= 1.1.31)"                                                                                                                                           
#> piamInterfaces     NA                                                                                                                                                                         
#> piamPlotComparison NA                                                                                                                                                                         
#> piamutils          NA                                                                                                                                                                         
#> pillar             NA                                                                                                                                                                         
#> pkgbuild           "R (>= 3.5)"                                                                                                                                                               
#> pkgconfig          NA                                                                                                                                                                         
#> pkgdown            "R (>= 4.1)"                                                                                                                                                               
#> pkgload            "R (>= 3.4.0)"                                                                                                                                                             
#> plotly             "R (>= 3.5.0), ggplot2 (>= 3.0.0)"                                                                                                                                         
#> plyr               "R (>= 3.1.0)"                                                                                                                                                             
#> praise             NA                                                                                                                                                                         
#> prettyunits        "R(>= 2.10)"                                                                                                                                                               
#> processx           "R (>= 3.4.0)"                                                                                                                                                             
#> profvis            "R (>= 4.0)"                                                                                                                                                               
#> promises           "R (>= 4.1.0)"                                                                                                                                                             
#> ps                 "R (>= 3.4)"                                                                                                                                                               
#> purrr              "R (>= 4.1)"                                                                                                                                                               
#> qualV              "R (>= 2.0.0), KernSmooth"                                                                                                                                                 
#> quantreg           "R (>= 3.5), stats, SparseM"                                                                                                                                               
#> quitte             "R (>= 4.1.0)"                                                                                                                                                             
#> ragg               NA                                                                                                                                                                         
#> rappdirs           "R (>= 4.1)"                                                                                                                                                               
#> raster             "sp (>= 1.4-5), R (>= 3.5.0)"                                                                                                                                              
#> rbibutils          "R (>= 2.10)"                                                                                                                                                              
#> rcmdcheck          NA                                                                                                                                                                         
#> readODS            "R (>= 4.1)"                                                                                                                                                               
#> readr              "R (>= 4.1)"                                                                                                                                                               
#> readxl             "R (>= 3.6)"                                                                                                                                                               
#> reformulas         NA                                                                                                                                                                         
#> rematch            NA                                                                                                                                                                         
#> remind2            "R (>= 4.1.0), magclass (>= 6.17.2)"                                                                                                                                       
#> remulator          "R(>= 2.10.0)"                                                                                                                                                             
#> renv               NA                                                                                                                                                                         
#> reporttransport    "R (>= 3.5.0)"                                                                                                                                                             
#> reshape            "R (>= 2.6.1)"                                                                                                                                                             
#> reshape2           "R (>= 3.1)"                                                                                                                                                               
#> rex                NA                                                                                                                                                                         
#> rlang              "R (>= 4.0.0)"                                                                                                                                                             
#> rmarkdown          "R (>= 3.0)"                                                                                                                                                               
#> rmndt              "R (>= 3.1), data.table (>= 1.11.0)"                                                                                                                                       
#> rootSolve          "R (>= 2.01)"                                                                                                                                                              
#> roxygen2           "R (>= 4.1)"                                                                                                                                                               
#> rprojroot          "R (>= 3.0.0)"                                                                                                                                                             
#> rstudioapi         NA                                                                                                                                                                         
#> rversions          NA                                                                                                                                                                         
#> rworldmap          "R (>= 2.10.0), sp"                                                                                                                                                        
#> sass               NA                                                                                                                                                                         
#> scales             "R (>= 4.1)"                                                                                                                                                               
#> sessioninfo        "R (>= 3.4)"                                                                                                                                                               
#> shiny              "methods, R (>= 3.1.2)"                                                                                                                                                    
#> sourcetools        "R (>= 3.0.2)"                                                                                                                                                             
#> sp                 "R (>= 3.5.0), methods"                                                                                                                                                    
#> spam               "R (>= 4.0)"                                                                                                                                                               
#> stringi            "R (>= 3.4)"                                                                                                                                                               
#> stringr            "R (>= 4.1.0)"                                                                                                                                                             
#> svglite            "R (>= 4.1)"                                                                                                                                                               
#> sys                NA                                                                                                                                                                         
#> systemfonts        "R (>= 3.2.0)"                                                                                                                                                             
#> terra              "R (>= 3.5.0), methods"                                                                                                                                                    
#> testthat           "R (>= 4.1.0)"                                                                                                                                                             
#> textshaping        "R (>= 3.2.0)"                                                                                                                                                             
#> tibble             "R (>= 3.4.0)"                                                                                                                                                             
#> tidyr              "R (>= 4.1.0)"                                                                                                                                                             
#> tidyselect         "R (>= 3.4)"                                                                                                                                                               
#> tidytemplate       NA                                                                                                                                                                         
#> timeDate           "R (>= 3.6.0), methods"                                                                                                                                                    
#> timechange         "R (>= 3.3)"                                                                                                                                                               
#> tinytex            NA                                                                                                                                                                         
#> trafficlight       "R(>= 2.10.0)"                                                                                                                                                             
#> tzdb               "R (>= 4.0.0)"                                                                                                                                                             
#> urca               "R (>= 2.0.0), methods"                                                                                                                                                    
#> urlchecker         "R (>= 3.3)"                                                                                                                                                               
#> usethis            "R (>= 4.1)"                                                                                                                                                               
#> utf8               "R (>= 2.10)"                                                                                                                                                              
#> vctrs              "R (>= 4.0.0)"                                                                                                                                                             
#> viridisLite        "R (>= 2.10)"                                                                                                                                                              
#> vroom              "R (>= 4.1)"                                                                                                                                                               
#> waldo              "R (>= 4.0)"                                                                                                                                                               
#> whisker            NA                                                                                                                                                                         
#> whoami             NA                                                                                                                                                                         
#> withr              "R (>= 3.6.0)"                                                                                                                                                             
#> writexl            NA                                                                                                                                                                         
#> xfun               "R (>= 3.2.0)"                                                                                                                                                             
#> xml2               "R (>= 3.6.0)"                                                                                                                                                             
#> xmlparsedata       "R (>= 3.0.0)"                                                                                                                                                             
#> xopen              "R (>= 3.1)"                                                                                                                                                               
#> xtable             "R (>= 2.10.0)"                                                                                                                                                            
#> yaml               NA                                                                                                                                                                         
#> ymlthis            "R (>= 3.2)"                                                                                                                                                               
#> zip                NA                                                                                                                                                                         
#> zoo                "R (>= 3.1.0), stats"                                                                                                                                                      
#> KernSmooth         "R (>= 2.5.0), stats"                                                                                                                                                      
#> MASS               "R (>= 4.4.0), grDevices, graphics, stats, utils"                                                                                                                          
#> Matrix             "R (>= 4.4), methods"                                                                                                                                                      
#> base               NA                                                                                                                                                                         
#> boot               "R (>= 3.0.0), graphics, stats"                                                                                                                                            
#> class              "R (>= 3.0.0), stats, utils"                                                                                                                                               
#> cluster            "R (>= 3.5.0)"                                                                                                                                                             
#> codetools          "R (>= 2.1)"                                                                                                                                                               
#> compiler           NA                                                                                                                                                                         
#> datasets           NA                                                                                                                                                                         
#> foreign            "R (>= 4.0.0)"                                                                                                                                                             
#> grDevices          NA                                                                                                                                                                         
#> graphics           NA                                                                                                                                                                         
#> grid               NA                                                                                                                                                                         
#> lattice            "R (>= 4.0.0)"                                                                                                                                                             
#> methods            NA                                                                                                                                                                         
#> mgcv               "R (>= 4.4.0), nlme (>= 3.1-64)"                                                                                                                                           
#> nlme               "R (>= 3.6.0)"                                                                                                                                                             
#> nnet               "R (>= 3.0.0), stats, utils"                                                                                                                                               
#> parallel           NA                                                                                                                                                                         
#> rpart              "R (>= 2.15.0), graphics, stats, grDevices"                                                                                                                                
#> spatial            "R (>= 3.0.0), graphics, stats, utils"                                                                                                                                     
#> splines            NA                                                                                                                                                                         
#> stats              NA                                                                                                                                                                         
#> stats4             NA                                                                                                                                                                         
#> survival           "R (>= 3.5.0)"                                                                                                                                                             
#> tcltk              NA                                                                                                                                                                         
#> tools              NA                                                                                                                                                                         
#> utils              NA                                                                                                                                                                         
#>                    Imports                                                                                                                                                                                                                                                                                                                                                                                                           
#> Deriv              "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> Formula            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> GDPuc              "cli (>= 2.4.0), crayon, dplyr, glue, magrittr, rlang (>=\n1.0.0), tibble, tidyr, tidyselect, withr"                                                                                                                                                                                                                                                                                                              
#> Hmisc              "methods, ggplot2, cluster, rpart, nnet, foreign, gtable, grid,\ngridExtra, data.table, htmlTable (>= 1.11.0), viridisLite,\nhtmltools, base64enc, colorspace, rmarkdown, knitr, Formula"                                                                                                                                                                                                                         
#> Lmoments           "stats, Rcpp"                                                                                                                                                                                                                                                                                                                                                                                                     
#> MatrixModels       "stats, methods, Matrix (>= 1.6-0), Matrix(< 1.8-0)"                                                                                                                                                                                                                                                                                                                                                              
#> R.methodsS3        "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> R.oo               "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> R.utils            "methods, utils, tools, R.methodsS3"                                                                                                                                                                                                                                                                                                                                                                              
#> R6                 NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> RColorBrewer       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> Rcpp               "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> Rdpack             "tools, utils, rbibutils (> 2.4)"                                                                                                                                                                                                                                                                                                                                                                                 
#> S7                 "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> SPEI               "lmomco, lmom, TLMoments, reshape, ggplot2, checkmate, zoo,\nlubridate"                                                                                                                                                                                                                                                                                                                                           
#> SparseM            "graphics, stats, utils"                                                                                                                                                                                                                                                                                                                                                                                          
#> TLMoments          "hypergeo, ggplot2, stats, lmomco"                                                                                                                                                                                                                                                                                                                                                                                
#> abind              "methods, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> askpass            "sys (>= 2.1)"                                                                                                                                                                                                                                                                                                                                                                                                    
#> assertr            "dplyr (>= 0.7.0), MASS, methods, stats, utils, rlang (>=\n0.3.0)"                                                                                                                                                                                                                                                                                                                                                
#> backports          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> base64enc          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bit                NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> bit64              "bit (>= 4.0.0), graphics, methods, stats, utils"                                                                                                                                                                                                                                                                                                                                                                 
#> brew               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> brio               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> broom              "backports, cli, dplyr (>= 1.0.0), generics (>= 0.0.2), glue,\nlifecycle, purrr, rlang (>= 1.1.0), stringr, tibble (>= 3.0.0),\ntidyr (>= 1.0.0)"                                                                                                                                                                                                                                                                 
#> bslib              "base64enc, cachem, fastmap (>= 1.1.1), grDevices, htmltools\n(>= 0.5.8), jquerylib (>= 0.1.3), jsonlite, lifecycle, memoise\n(>= 2.0.1), mime, rlang, sass (>= 0.4.9)"                                                                                                                                                                                                                                           
#> cachem             "rlang, fastmap (>= 1.2.0)"                                                                                                                                                                                                                                                                                                                                                                                       
#> callr              "processx (>= 3.6.1), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                  
#> car                "abind, Formula, MASS, mgcv, nnet, pbkrtest (>= 0.4-4),\nquantreg, grDevices, utils, stats, graphics, lme4 (>=\n1.1-27.1), nlme, scales"                                                                                                                                                                                                                                                                          
#> carData            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> cellranger         "rematch, tibble"                                                                                                                                                                                                                                                                                                                                                                                                 
#> checkmate          "backports (>= 1.1.0), utils"                                                                                                                                                                                                                                                                                                                                                                                     
#> citation           "desc, jsonlite, utils, withr, yaml"                                                                                                                                                                                                                                                                                                                                                                              
#> cli                "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> clipr              "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> collections        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> colorspace         "graphics, grDevices, stats"                                                                                                                                                                                                                                                                                                                                                                                      
#> commonmark         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> contfrac           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> countrycode        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> covr               "digest, stats, utils, jsonlite, rex, httr, cli, withr (>=\n1.0.2), yaml"                                                                                                                                                                                                                                                                                                                                         
#> cowplot            "ggplot2 (>= 3.5.2), grid, gtable, grDevices, methods, rlang,\nscales"                                                                                                                                                                                                                                                                                                                                            
#> crayon             "grDevices, methods, utils"                                                                                                                                                                                                                                                                                                                                                                                       
#> credentials        "openssl (>= 1.3), sys (>= 2.1), curl, jsonlite, askpass"                                                                                                                                                                                                                                                                                                                                                         
#> crosstalk          "htmltools (>= 0.3.6), jsonlite, lazyeval, R6"                                                                                                                                                                                                                                                                                                                                                                    
#> curl               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> data.table         "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> deSolve            "methods, graphics, grDevices, stats"                                                                                                                                                                                                                                                                                                                                                                             
#> desc               "cli, R6, utils"                                                                                                                                                                                                                                                                                                                                                                                                  
#> devtools           "cli (>= 3.6.6), desc (>= 1.4.3), ellipsis (>= 0.3.3), fs (>=\n2.1.0), lifecycle (>= 1.0.5), memoise (>= 2.0.1), miniUI (>=\n0.1.2), pak (>= 0.9.5), pkgbuild (>= 1.4.8), pkgdown (>=\n2.2.0), pkgload (>= 1.5.2), profvis (>= 0.4.0), rcmdcheck (>=\n1.4.0), rlang (>= 1.2.0), roxygen2 (>= 7.3.3), rversions (>=\n3.0.0), sessioninfo (>= 1.2.3), testthat (>= 3.3.2), urlchecker\n(>= 1.0.1), withr (>= 3.0.2)"
#> diffobj            "crayon (>= 1.3.2), tools, methods, utils, stats"                                                                                                                                                                                                                                                                                                                                                                 
#> digest             "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> doBy               "boot, broom, cowplot, Deriv, dplyr, forecast, ggplot2, MASS,\nMatrix, modelr, microbenchmark, rlang, purrr, tibble, tidyr,"                                                                                                                                                                                                                                                                                      
#> doParallel         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> docopt             "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> dotCall64          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> downlit            "brio, desc, digest, evaluate, fansi, memoise, rlang, vctrs,\nwithr, yaml"                                                                                                                                                                                                                                                                                                                                        
#> dplyr              "cli (>= 3.6.2), generics, glue (>= 1.3.2), lifecycle (>=\n1.0.5), magrittr (>= 1.5), methods, pillar (>= 1.9.0), R6,\nrlang (>= 1.1.7), tibble (>= 3.2.0), tidyselect (>= 1.2.0),\nutils, vctrs (>= 0.7.1)"                                                                                                                                                                                                      
#> edgeTransport      "rmndt, magclass, rootSolve, madrat, magrittr, gdx, gdxrrw,\nzoo, gdxdt, mrdrivers, reporttransport (>= 1.1.0)"                                                                                                                                                                                                                                                                                                   
#> ellipsis           "rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                                                                
#> elliptic           "MASS"                                                                                                                                                                                                                                                                                                                                                                                                            
#> evaluate           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fansi              "grDevices, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> farver             NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fastmap            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fields             "maps"                                                                                                                                                                                                                                                                                                                                                                                                            
#> filelock           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> fontawesome        "rlang (>= 1.0.6), htmltools (>= 0.5.1.1)"                                                                                                                                                                                                                                                                                                                                                                        
#> forcats            "cli (>= 3.4.0), glue, lifecycle, magrittr, rlang (>= 1.0.0),\ntibble"                                                                                                                                                                                                                                                                                                                                            
#> foreach            "codetools, utils, iterators"                                                                                                                                                                                                                                                                                                                                                                                     
#> forecast           "colorspace, fracdiff, generics (>= 0.1.2), ggplot2 (>= 3.4.0),\ngraphics, lmtest, magrittr, nnet, parallel, Rcpp (>= 0.12.4),\nstats, timeDate, urca, withr, zoo"                                                                                                                                                                                                                                                
#> fracdiff           "stats"                                                                                                                                                                                                                                                                                                                                                                                                           
#> fs                 "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gamstransfer       "Rcpp (>= 1.0.6), R6 (>= 2.5.1), R.utils (>= 2.11.0),\ncollections(>= 0.3.6)"                                                                                                                                                                                                                                                                                                                                     
#> gdx                "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> gdx2               "gamstransfer, magclass (>= 6.0)"                                                                                                                                                                                                                                                                                                                                                                                 
#> gdxdt              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> gdxrrw             "reshape2"                                                                                                                                                                                                                                                                                                                                                                                                        
#> generics           "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> geometry           "magic, Rcpp, lpSolve, linprog"                                                                                                                                                                                                                                                                                                                                                                                   
#> gert               "askpass, credentials (>= 1.2.1), openssl (>= 2.0.3),\nrstudioapi (>= 0.11), sys, zip (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                 
#> ggplot2            "cli, grDevices, grid, gtable (>= 0.3.6), isoband, lifecycle (>\n1.0.1), rlang (>= 1.1.0), S7, scales (>= 1.4.0), stats, vctrs\n(>= 0.6.0), withr (>= 2.5.0)"                                                                                                                                                                                                                                                     
#> gh                 "cli (>= 3.0.1), gitcreds, glue, httr2 (>= 1.0.6), ini,\njsonlite, lifecycle, rlang (>= 1.0.0)"                                                                                                                                                                                                                                                                                                                   
#> gitcreds           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> glue               "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> gms                "dplyr, rlang, stringr, withr, yaml, filelock, stats"                                                                                                                                                                                                                                                                                                                                                             
#> goftest            "stats"                                                                                                                                                                                                                                                                                                                                                                                                           
#> gridExtra          "gtable, grid, grDevices, graphics, utils"                                                                                                                                                                                                                                                                                                                                                                        
#> gtable             "cli, glue, grid, lifecycle, rlang (>= 1.1.0), stats"                                                                                                                                                                                                                                                                                                                                                             
#> hdf5r              "R6, bit64, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> highr              "xfun (>= 0.18)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> hms                "cli, lifecycle, methods, pkgconfig, rlang (>= 1.0.2), vctrs\n(>= 0.3.8)"                                                                                                                                                                                                                                                                                                                                         
#> htmlTable          "stringr, knitr (>= 1.6), magrittr (>= 1.5), methods,\ncheckmate, htmlwidgets, htmltools, rstudioapi (>= 0.6)"                                                                                                                                                                                                                                                                                                    
#> htmltools          "base64enc, digest, fastmap (>= 1.1.0), grDevices, rlang (>=\n1.0.0), utils"                                                                                                                                                                                                                                                                                                                                      
#> htmlwidgets        "grDevices, htmltools (>= 0.5.7), jsonlite (>= 0.9.16), knitr\n(>= 1.8), rmarkdown, yaml"                                                                                                                                                                                                                                                                                                                         
#> httpuv             "later (>= 0.8.0), promises, R6, Rcpp (>= 1.0.7), utils"                                                                                                                                                                                                                                                                                                                                                          
#> httr               "curl (>= 5.1.0), jsonlite, mime, openssl (>= 0.8), R6"                                                                                                                                                                                                                                                                                                                                                           
#> httr2              "cli (>= 3.0.0), curl (>= 6.4.0), glue, lifecycle, magrittr,\nopenssl, R6, rappdirs, rlang (>= 1.1.0), vctrs (>= 0.6.3),\nwithr"                                                                                                                                                                                                                                                                                  
#> hypergeo           "elliptic (>= 1.3-5), contfrac (>= 1.1-9), deSolve"                                                                                                                                                                                                                                                                                                                                                               
#> igraph             "cli, graphics, grDevices, lifecycle, magrittr, Matrix,\npkgconfig (>= 2.0.0), rlang (>= 1.1.0), stats, utils, vctrs"                                                                                                                                                                                                                                                                                             
#> ini                NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> isoband            "cli, grid, rlang, utils"                                                                                                                                                                                                                                                                                                                                                                                         
#> iterators          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> jquerylib          "htmltools"                                                                                                                                                                                                                                                                                                                                                                                                       
#> jsonlite           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> kableExtra         "knitr (>= 1.33), magrittr, stringr (>= 1.0), xml2 (>= 1.1.1),\nrmarkdown (>= 1.6.0), scales, viridisLite, stats, grDevices,\nhtmltools, rstudioapi, tools, digest, graphics, svglite"                                                                                                                                                                                                                            
#> knitr              "evaluate (>= 0.15), highr (>= 0.11), methods, tools, xfun (>=\n0.52), yaml (>= 2.1.19)"                                                                                                                                                                                                                                                                                                                          
#> labeling           "stats, graphics"                                                                                                                                                                                                                                                                                                                                                                                                 
#> later              "Rcpp (>= 1.0.10), rlang"                                                                                                                                                                                                                                                                                                                                                                                         
#> lazyeval           "rlang"                                                                                                                                                                                                                                                                                                                                                                                                           
#> lifecycle          "cli (>= 3.4.0), rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                
#> linprog            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> lintr              "backports (>= 1.5.0), cli (>= 3.4.0), codetools, digest, glue,\nknitr, rex, stats, utils, xfun, xml2 (>= 1.0.0), xmlparsedata\n(>= 1.0.5)"                                                                                                                                                                                                                                                                       
#> littler            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> lme4               "MASS, Rdpack, boot, graphics, grid, lattice, minqa (>=\n1.1.15), nlme (>= 3.1-123), nloptr (>= 1.0.4), parallel,\nreformulas (>= 0.4.3.1), rlang, splines, utils"                                                                                                                                                                                                                                                
#> lmom               "stats, graphics"                                                                                                                                                                                                                                                                                                                                                                                                 
#> lmomco             "goftest, Lmoments, MASS"                                                                                                                                                                                                                                                                                                                                                                                         
#> lmtest             "graphics"                                                                                                                                                                                                                                                                                                                                                                                                        
#> lpSolve            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> lpjclass           "magclass, utils"                                                                                                                                                                                                                                                                                                                                                                                                 
#> lpjmlkit           "magrittr, dplyr, processx, tibble, jsonlite, doParallel,\nforeach, utils, methods, abind, rlang, withr, grDevices, cli,\nstringi"                                                                                                                                                                                                                                                                                
#> lubridate          "generics, timechange (>= 0.4.0)"                                                                                                                                                                                                                                                                                                                                                                                 
#> lucode2            "callr, citation (>= 0.11.3), data.table, desc, devtools,\ndplyr, lintr (>= 3.1.0), pak, rlang, tools, usethis (>= 2.1.0),\nwithr, yaml"                                                                                                                                                                                                                                                                          
#> luplot             "data.table, ggplot2, graphics, grDevices, gridExtra, lusweave,\nmip, mstools (>= 0.6.0), quitte, RColorBrewer, reshape2, rlang,\nutils"                                                                                                                                                                                                                                                                          
#> luscale            "madrat, spam, utils"                                                                                                                                                                                                                                                                                                                                                                                             
#> lusweave           "knitr (>= 1.38), withr, xtable"                                                                                                                                                                                                                                                                                                                                                                                  
#> madrat             "callr, digest, filelock (>= 1.0.3), igraph (>= 2.1.1), Matrix,\nmethods, pkgload, renv, stringi, tools, utils, withr, yaml"                                                                                                                                                                                                                                                                                      
#> magclass           "abind, data.table, rlang, stats"                                                                                                                                                                                                                                                                                                                                                                                 
#> magic              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> magpie4            "dplyr, gdx2 (>= 0.1.6), gms, luplot, luscale, lusweave, madrat\n(>= 3.10.0), magpiesets, methods, memoise, mip, mstools (>=\n0.12.1), nonparaeff, piamutils, reshape2, rlang, R.utils,\nrworldmap (>= 1.3.8), stats, utils, tidyr, GDPuc, MASS"                                                                                                                                                                  
#> magpiesets         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> magrittr           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> maps               "graphics, utils"                                                                                                                                                                                                                                                                                                                                                                                                 
#> memoise            "rlang (>= 0.4.10), cachem"                                                                                                                                                                                                                                                                                                                                                                                       
#> microbenchmark     "graphics, stats"                                                                                                                                                                                                                                                                                                                                                                                                 
#> mime               "tools"                                                                                                                                                                                                                                                                                                                                                                                                           
#> miniUI             "shiny (>= 0.13), htmltools (>= 0.3), utils"                                                                                                                                                                                                                                                                                                                                                                      
#> minqa              "Rcpp (>= 0.9.10)"                                                                                                                                                                                                                                                                                                                                                                                                
#> minty              "tzdb"                                                                                                                                                                                                                                                                                                                                                                                                            
#> mip                "data.table, dplyr, ggplot2, gridExtra, htmltools, lusweave (>=\n1.43.2), piamutils, plotly, RColorBrewer, reshape2, rlang,\nshiny, stringr, tidyr, trafficlight, withr,"                                                                                                                                                                                                                                         
#> modelr             "broom, magrittr, purrr (>= 0.2.2), rlang (>= 1.0.6), tibble,\ntidyr (>= 0.8.0), tidyselect, vctrs"                                                                                                                                                                                                                                                                                                               
#> mrcommons          "countrycode, data.table, dplyr, hdf5r, luscale, magpiesets (>=\n0.44.2), methods, ncdf4, openxlsx, purrr, quitte, raster,\nreadxl, reshape2, rlang, stringr, terra, tibble, tidyr, withr,\nzoo"                                                                                                                                                                                                                  
#> mrdownscale        "gdx2, luscale (>= 3.1.4), magpie4 (>= 2.34.1), mip, ncdf4,\nterra, utils, withr"                                                                                                                                                                                                                                                                                                                                 
#> mrdrivers          "countrycode, dplyr, GDPuc (>= 1.3.0), glue, magrittr, purrr,\nreadr, readxl, rlang, tibble, tidyr, tidyselect"                                                                                                                                                                                                                                                                                                   
#> mrfaocore          "data.table, dplyr, magpiesets (>= 0.44.2), tidyr, withr"                                                                                                                                                                                                                                                                                                                                                         
#> mrindustry         "assertr, broom, car, countrycode, dplyr, GDPuc (>= 1.3.0),\nggplot2, Hmisc, madrat (>= 3.7.1), magclass (>= 6.16.1),\nmagpiesets, magrittr, mrdrivers (>= 4.0.0), purrr, quitte (>=\n0.3105.0), readODS, readr, readxl, rlang, tibble, tidyr,\ntidyselect, zoo,"                                                                                                                                                 
#> mrlandcore         "dplyr, lpjclass, lpjmlkit, luscale, magpiesets (>= 0.44.2),\nncdf4, nleqslv, raster, SPEI, stringr, terra, withr"                                                                                                                                                                                                                                                                                                
#> mrremind           "countrycode, data.table, dplyr, glue, ggplot2, magrittr, nnls,\npurrr, quitte (>= 0.3105.0), R.utils, readr, readxl, reshape2,\nrlang, rmndt, tibble, tidyr, tidyselect, zoo"                                                                                                                                                                                                                                    
#> mrtransport        "data.table, dplyr, GDPuc (>= 1.3.0), gdx, magclass, magrittr,\nreadxl, rlang, rmndt, stringr, tidyr, tidyselect, utils, zoo"                                                                                                                                                                                                                                                                                     
#> mstools            "magpiesets, stringr, yaml"                                                                                                                                                                                                                                                                                                                                                                                       
#> ncdf4              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> nleqslv            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> nloptr             NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> nnls               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> nonparaeff         "geometry, Hmisc, lpSolve"                                                                                                                                                                                                                                                                                                                                                                                        
#> numDeriv           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> openssl            "askpass"                                                                                                                                                                                                                                                                                                                                                                                                         
#> openxlsx           "grDevices, methods, Rcpp, stats, stringi, utils, zip"                                                                                                                                                                                                                                                                                                                                                            
#> otel               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> pak                "tools, utils"                                                                                                                                                                                                                                                                                                                                                                                                    
#> pbkrtest           "broom, dplyr, MASS, methods, numDeriv, Matrix (>= 1.2.3), doBy\n(>= 4.6.22)"                                                                                                                                                                                                                                                                                                                                     
#> piamInterfaces     "dplyr (>= 1.1.1), GDPuc, gms, jsonlite, magclass (>= 6.17.2),\nmip (>= 0.150.0), readxl, quitte (>= 0.3137.1), piamutils (>=\n0.0.12), rlang, stringr, tibble, tidyr, tidyselect, yaml"                                                                                                                                                                                                                          
#> piamPlotComparison "dplyr, jsonlite, kableExtra, knitr, magclass, mip (>=\n0.148.11), piamutils, quitte, rlang, rmarkdown, tidyr, yaml,\nymlthis"                                                                                                                                                                                                                                                                                    
#> piamutils          "dplyr, magclass, magrittr, pkgload, quitte, rlang"                                                                                                                                                                                                                                                                                                                                                               
#> pillar             "cli (>= 2.3.0), glue, lifecycle, rlang (>= 1.0.2), utf8 (>=\n1.1.0), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                                                    
#> pkgbuild           "callr (>= 3.2.0), cli (>= 3.4.0), desc, processx, R6"                                                                                                                                                                                                                                                                                                                                                            
#> pkgconfig          "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> pkgdown            "bslib (>= 0.5.1), callr (>= 3.7.3), cli (>= 3.6.1), desc (>=\n1.4.0), downlit (>= 0.4.4), fontawesome, fs (>= 1.4.0), httr2\n(>= 1.0.2), jsonlite, lifecycle, openssl, purrr (>= 1.0.0),\nragg (>= 1.4.0), rlang (>= 1.1.4), rmarkdown (>= 2.27), tibble,\nwhisker, withr (>= 2.4.3), xml2 (>= 1.3.1), yaml (>= 2.3.9)"                                                                                          
#> pkgload            "cli (>= 3.3.0), desc, fs, glue, lifecycle, methods, pkgbuild,\nprocessx, rlang (>= 1.1.1), rprojroot, utils"                                                                                                                                                                                                                                                                                                     
#> plotly             "tools, scales, httr (>= 1.3.0), jsonlite (>= 1.6), magrittr,\ndigest, viridisLite, base64enc, htmltools (>= 0.3.6),\nhtmlwidgets (>= 1.5.2.9001), tidyr (>= 1.0.0), RColorBrewer,\ndplyr, vctrs, tibble, lazyeval (>= 0.2.0), rlang (>= 1.0.0),\ncrosstalk, purrr, data.table, promises"                                                                                                                         
#> plyr               "Rcpp (>= 0.11.0)"                                                                                                                                                                                                                                                                                                                                                                                                
#> praise             NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> prettyunits        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> processx           "ps (>= 1.9.3), R6, utils"                                                                                                                                                                                                                                                                                                                                                                                        
#> profvis            "htmlwidgets (>= 0.3.2), rlang (>= 1.1.0), vctrs"                                                                                                                                                                                                                                                                                                                                                                 
#> promises           "fastmap (>= 1.1.0), later, lifecycle, magrittr (>= 1.5), otel\n(>= 0.2.0), R6, rlang"                                                                                                                                                                                                                                                                                                                            
#> ps                 "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> purrr              "cli (>= 3.6.1), lifecycle (>= 1.0.3), magrittr (>= 1.5.0),\nrlang (>= 1.1.1), vctrs (>= 0.6.3)"                                                                                                                                                                                                                                                                                                                  
#> qualV              "graphics, grDevices, stats, utils"                                                                                                                                                                                                                                                                                                                                                                               
#> quantreg           "methods, graphics, Matrix, MatrixModels, survival, MASS"                                                                                                                                                                                                                                                                                                                                                         
#> quitte             "cli, countrycode, dplyr (>= 1.1.1), forcats (>= 1.0.0),\ngamstransfer, ggplot2 (>= 4.0.0), gms (>= 0.17.0), glue,\nlazyeval, lifecycle, lubridate, magclass, magrittr, methods,\nplyr, purrr, readr, readxl, reshape2, rlang, stats, stringr,\ntibble, tidyr, tidyselect, writexl, zoo,"                                                                                                                         
#> ragg               "systemfonts (>= 1.0.3), textshaping (>= 0.3.0)"                                                                                                                                                                                                                                                                                                                                                                  
#> rappdirs           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> raster             "Rcpp, methods, terra (>= 1.8-5)"                                                                                                                                                                                                                                                                                                                                                                                 
#> rbibutils          "utils, tools"                                                                                                                                                                                                                                                                                                                                                                                                    
#> rcmdcheck          "callr (>= 3.1.1.9000), cli (>= 3.0.0), curl, desc (>= 1.2.0),\ndigest, pkgbuild, prettyunits, R6, rprojroot, sessioninfo (>=\n1.1.1), utils, withr, xopen"                                                                                                                                                                                                                                                       
#> readODS            "cellranger, minty (>= 0.0.5), stringi, tibble, vctrs (>=\n0.4.2), zip, tools, withr"                                                                                                                                                                                                                                                                                                                             
#> readr              "cli, clipr, crayon, glue, hms (>= 0.4.1), lifecycle, methods,\nR6, rlang, tibble, utils, vroom (>= 1.7.0), withr"                                                                                                                                                                                                                                                                                                
#> readxl             "cellranger, tibble (>= 2.0.1), utils"                                                                                                                                                                                                                                                                                                                                                                            
#> reformulas         "stats, methods, Matrix, Rdpack"                                                                                                                                                                                                                                                                                                                                                                                  
#> rematch            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> remind2            "abind, assertr, data.table, dplyr (>= 1.1.1), GDPuc, gdx (>=\n1.53.0), gdxrrw, ggplot2, gms, lucode2 (>= 0.43.0), lusweave,\nmadrat (>= 3.13.0), mip (>= 0.149.2), openxlsx, piamInterfaces\n(>= 0.33.0), piamPlotComparison (>= 0.0.10), piamutils, plotly\n(>= 4.10.4), quitte (>= 0.3132.0), readr, remulator, reshape2,\nrlang, rmarkdown, tibble, tidyr, tidyselect, withr, digest"                         
#> remulator          "gms, ggplot2, luplot, lusweave, madrat, magclass(>= 4.40),\nmagpie4,"                                                                                                                                                                                                                                                                                                                                            
#> renv               "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> reporttransport    "data.table, gdx, gdxrrw, piamPlotComparison, quitte, rmndt,\nutils"                                                                                                                                                                                                                                                                                                                                              
#> reshape            "plyr"                                                                                                                                                                                                                                                                                                                                                                                                            
#> reshape2           "plyr (>= 1.8.1), Rcpp, stringr"                                                                                                                                                                                                                                                                                                                                                                                  
#> rex                "withr"                                                                                                                                                                                                                                                                                                                                                                                                           
#> rlang              "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> rmarkdown          "bslib (>= 0.2.5.1), evaluate (>= 0.13), fontawesome (>=\n0.5.0), htmltools (>= 0.5.1), jquerylib, jsonlite, knitr (>=\n1.43), methods, tinytex (>= 0.31), tools, utils, xfun (>=\n0.36), yaml (>= 2.1.19)"                                                                                                                                                                                                       
#> rmndt              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rootSolve          "stats, graphics, grDevices"                                                                                                                                                                                                                                                                                                                                                                                      
#> roxygen2           "brew, cli (>= 3.3.0), commonmark, desc (>= 1.2.0), knitr,\nlifecycle, methods, pkgload (>= 1.5.2), R6 (>= 2.1.2), rlang\n(>= 1.1.0), utils, withr, xml2"                                                                                                                                                                                                                                                         
#> rprojroot          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rstudioapi         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> rversions          "curl"                                                                                                                                                                                                                                                                                                                                                                                                            
#> rworldmap          "fields, methods, raster, terra"                                                                                                                                                                                                                                                                                                                                                                                  
#> sass               "fs (>= 1.2.4), rlang (>= 0.4.10), htmltools (>= 0.5.1), R6,\nrappdirs"                                                                                                                                                                                                                                                                                                                                           
#> scales             "cli, farver (>= 2.0.3), glue, labeling, lifecycle, R6,\nRColorBrewer, rlang (>= 1.1.0), viridisLite"                                                                                                                                                                                                                                                                                                             
#> sessioninfo        "cli (>= 3.1.0), tools, utils"                                                                                                                                                                                                                                                                                                                                                                                    
#> shiny              "bslib (>= 0.6.0), cachem (>= 1.1.0), cli, commonmark (>=\n2.0.0), fastmap (>= 1.1.1), fontawesome (>= 0.4.0), glue (>=\n1.3.2), grDevices, htmltools (>= 0.5.4), httpuv (>= 1.5.2),\njsonlite (>= 0.9.16), later (>= 1.0.0), lifecycle (>= 0.2.0),\nmime (>= 0.3), otel, promises (>= 1.5.0), R6 (>= 2.0), rlang\n(>= 0.4.10), sourcetools, tools, utils, withr, xtable"                                         
#> sourcetools        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> sp                 "utils, stats, graphics, grDevices, lattice, grid"                                                                                                                                                                                                                                                                                                                                                                
#> spam               "dotCall64, grid, methods, Rcpp (>= 1.0.8.3)"                                                                                                                                                                                                                                                                                                                                                                     
#> stringi            "tools, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                             
#> stringr            "cli, glue (>= 1.6.1), lifecycle (>= 1.0.3), magrittr, rlang\n(>= 1.0.0), stringi (>= 1.5.3), vctrs (>= 0.4.0)"                                                                                                                                                                                                                                                                                                   
#> svglite            "base64enc, cli, lifecycle, rlang (>= 1.1.0), systemfonts (>=\n1.3.0), textshaping (>= 0.3.0)"                                                                                                                                                                                                                                                                                                                    
#> sys                NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> systemfonts        "base64enc, grid, jsonlite, lifecycle, tools, utils"                                                                                                                                                                                                                                                                                                                                                              
#> terra              "Rcpp (>= 1.0-10)"                                                                                                                                                                                                                                                                                                                                                                                                
#> testthat           "brio (>= 1.1.5), callr (>= 3.7.6), cli (>= 3.6.5), desc (>=\n1.4.3), evaluate (>= 1.0.4), jsonlite (>= 2.0.0), lifecycle (>=\n1.0.4), magrittr (>= 2.0.3), methods, pkgload (>= 1.4.0),\npraise (>= 1.0.0), processx (>= 3.8.6), ps (>= 1.9.1), R6 (>=\n2.6.1), rlang (>= 1.1.6), utils, waldo (>= 0.6.2), withr (>=\n3.0.2)"                                                                                    
#> textshaping        "lifecycle, stats, stringi, systemfonts (>= 1.3.0), utils"                                                                                                                                                                                                                                                                                                                                                        
#> tibble             "cli, lifecycle (>= 1.0.0), magrittr, methods, pillar (>=\n1.8.1), pkgconfig, rlang (>= 1.0.2), utils, vctrs (>= 0.5.0)"                                                                                                                                                                                                                                                                                          
#> tidyr              "cli (>= 3.4.1), dplyr (>= 1.1.0), glue, lifecycle (>= 1.0.3),\nmagrittr, purrr (>= 1.0.1), rlang (>= 1.1.1), stringr (>=\n1.5.0), tibble (>= 2.1.1), tidyselect (>= 1.2.1), utils, vctrs\n(>= 0.5.2)"                                                                                                                                                                                                            
#> tidyselect         "cli (>= 3.3.0), glue (>= 1.3.0), lifecycle (>= 1.0.3), rlang\n(>= 1.0.4), vctrs (>= 0.5.2), withr"                                                                                                                                                                                                                                                                                                               
#> tidytemplate       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> timeDate           "graphics, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                          
#> timechange         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> tinytex            "xfun (>= 0.48)"                                                                                                                                                                                                                                                                                                                                                                                                  
#> trafficlight       "magclass, ggplot2, qualV"                                                                                                                                                                                                                                                                                                                                                                                        
#> tzdb               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> urca               "nlme, graphics, stats"                                                                                                                                                                                                                                                                                                                                                                                           
#> urlchecker         "cli, curl, tools, xml2"                                                                                                                                                                                                                                                                                                                                                                                          
#> usethis            "cli (>= 3.0.1), clipr (>= 0.3.0), crayon, curl (>= 2.7), desc\n(>= 1.4.2), fs (>= 1.3.0), gert (>= 1.4.1), gh (>= 1.2.1), glue\n(>= 1.3.0), jsonlite, lifecycle (>= 1.0.0), purrr, rappdirs,\nrlang (>= 1.1.0), rprojroot (>= 2.1.1), rstudioapi, stats,\ntools, utils, whisker, withr (>= 2.3.0), yaml"                                                                                                         
#> utf8               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> vctrs              "cli (>= 3.4.0), glue, lifecycle (>= 1.0.3), rlang (>= 1.1.7)"                                                                                                                                                                                                                                                                                                                                                    
#> viridisLite        NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> vroom              "bit64, cli (>= 3.2.0), crayon, glue, hms, lifecycle (>=\n1.0.3), methods, rlang (>= 1.1.0), stats, tibble (>= 2.0.0),\ntidyselect, tzdb (>= 0.1.1), vctrs (>= 0.2.0), withr"                                                                                                                                                                                                                                     
#> waldo              "cli, diffobj (>= 0.3.4), glue, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                        
#> whisker            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> whoami             "httr, jsonlite, utils"                                                                                                                                                                                                                                                                                                                                                                                           
#> withr              "graphics, grDevices"                                                                                                                                                                                                                                                                                                                                                                                             
#> writexl            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> xfun               "grDevices, stats, tools"                                                                                                                                                                                                                                                                                                                                                                                         
#> xml2               "cli, methods, rlang (>= 1.1.0)"                                                                                                                                                                                                                                                                                                                                                                                  
#> xmlparsedata       NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> xopen              "processx"                                                                                                                                                                                                                                                                                                                                                                                                        
#> xtable             "stats, utils, methods"                                                                                                                                                                                                                                                                                                                                                                                           
#> yaml               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> ymlthis            "crayon, fs, glue, magrittr, purrr (>= 0.3.2), rlang (>=\n0.4.10), rmarkdown (>= 2.10), rstudioapi, stringr, usethis (>=\n1.5.0), whoami, withr, yaml"                                                                                                                                                                                                                                                            
#> zip                NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> zoo                "utils, graphics, grDevices, lattice (>= 0.20-27)"                                                                                                                                                                                                                                                                                                                                                                
#> KernSmooth         NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> MASS               "methods"                                                                                                                                                                                                                                                                                                                                                                                                         
#> Matrix             "grDevices, graphics, grid, lattice, stats, utils"                                                                                                                                                                                                                                                                                                                                                                
#> base               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> boot               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> class              "MASS"                                                                                                                                                                                                                                                                                                                                                                                                            
#> cluster            "graphics, grDevices, stats, utils"                                                                                                                                                                                                                                                                                                                                                                               
#> codetools          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> compiler           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> datasets           NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> foreign            "methods, utils, stats"                                                                                                                                                                                                                                                                                                                                                                                           
#> grDevices          NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> graphics           "grDevices"                                                                                                                                                                                                                                                                                                                                                                                                       
#> grid               "grDevices, utils"                                                                                                                                                                                                                                                                                                                                                                                                
#> lattice            "grid, grDevices, graphics, stats, utils"                                                                                                                                                                                                                                                                                                                                                                         
#> methods            "utils, stats"                                                                                                                                                                                                                                                                                                                                                                                                    
#> mgcv               "methods, stats, graphics, Matrix, splines, utils"                                                                                                                                                                                                                                                                                                                                                                
#> nlme               "graphics, stats, utils, lattice"                                                                                                                                                                                                                                                                                                                                                                                 
#> nnet               NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> parallel           "tools, compiler"                                                                                                                                                                                                                                                                                                                                                                                                 
#> rpart              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> spatial            NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> splines            "graphics, stats"                                                                                                                                                                                                                                                                                                                                                                                                 
#> stats              "utils, grDevices, graphics"                                                                                                                                                                                                                                                                                                                                                                                      
#> stats4             "graphics, methods, stats"                                                                                                                                                                                                                                                                                                                                                                                        
#> survival           "graphics, Matrix, methods, splines, stats, utils"                                                                                                                                                                                                                                                                                                                                                                
#> tcltk              "utils"                                                                                                                                                                                                                                                                                                                                                                                                           
#> tools              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#> utils              NA                                                                                                                                                                                                                                                                                                                                                                                                                
#>                    LinkingTo                                                       
#> Deriv              NA                                                              
#> Formula            NA                                                              
#> GDPuc              NA                                                              
#> Hmisc              NA                                                              
#> Lmoments           "Rcpp, RcppArmadillo"                                           
#> MatrixModels       NA                                                              
#> R.methodsS3        NA                                                              
#> R.oo               NA                                                              
#> R.utils            NA                                                              
#> R6                 NA                                                              
#> RColorBrewer       NA                                                              
#> Rcpp               NA                                                              
#> Rdpack             NA                                                              
#> S7                 NA                                                              
#> SPEI               NA                                                              
#> SparseM            NA                                                              
#> TLMoments          "Rcpp"                                                          
#> abind              NA                                                              
#> askpass            NA                                                              
#> assertr            NA                                                              
#> backports          NA                                                              
#> base64enc          NA                                                              
#> bit                NA                                                              
#> bit64              NA                                                              
#> brew               NA                                                              
#> brio               NA                                                              
#> broom              NA                                                              
#> bslib              NA                                                              
#> cachem             NA                                                              
#> callr              NA                                                              
#> car                NA                                                              
#> carData            NA                                                              
#> cellranger         NA                                                              
#> checkmate          NA                                                              
#> citation           NA                                                              
#> cli                NA                                                              
#> clipr              NA                                                              
#> collections        NA                                                              
#> colorspace         NA                                                              
#> commonmark         NA                                                              
#> contfrac           NA                                                              
#> countrycode        NA                                                              
#> covr               NA                                                              
#> cowplot            NA                                                              
#> crayon             NA                                                              
#> credentials        NA                                                              
#> crosstalk          NA                                                              
#> curl               NA                                                              
#> data.table         NA                                                              
#> deSolve            NA                                                              
#> desc               NA                                                              
#> devtools           NA                                                              
#> diffobj            NA                                                              
#> digest             NA                                                              
#> doBy               NA                                                              
#> doParallel         NA                                                              
#> docopt             NA                                                              
#> dotCall64          NA                                                              
#> downlit            NA                                                              
#> dplyr              NA                                                              
#> edgeTransport      NA                                                              
#> ellipsis           NA                                                              
#> elliptic           NA                                                              
#> evaluate           NA                                                              
#> fansi              NA                                                              
#> farver             NA                                                              
#> fastmap            NA                                                              
#> fields             NA                                                              
#> filelock           NA                                                              
#> fontawesome        NA                                                              
#> forcats            NA                                                              
#> foreach            NA                                                              
#> forecast           "Rcpp (>= 0.12.4), RcppArmadillo (>= 0.2.35)"                   
#> fracdiff           NA                                                              
#> fs                 NA                                                              
#> gamstransfer       "Rcpp"                                                          
#> gdx                NA                                                              
#> gdx2               NA                                                              
#> gdxdt              NA                                                              
#> gdxrrw             NA                                                              
#> generics           NA                                                              
#> geometry           "Rcpp, RcppProgress"                                            
#> gert               NA                                                              
#> ggplot2            NA                                                              
#> gh                 NA                                                              
#> gitcreds           NA                                                              
#> glue               NA                                                              
#> gms                NA                                                              
#> goftest            NA                                                              
#> gridExtra          NA                                                              
#> gtable             NA                                                              
#> hdf5r              NA                                                              
#> highr              NA                                                              
#> hms                NA                                                              
#> htmlTable          NA                                                              
#> htmltools          NA                                                              
#> htmlwidgets        NA                                                              
#> httpuv             "later, Rcpp"                                                   
#> httr               NA                                                              
#> httr2              NA                                                              
#> hypergeo           NA                                                              
#> igraph             "cpp11 (>= 0.5.0)"                                              
#> ini                NA                                                              
#> isoband            "cpp11"                                                         
#> iterators          NA                                                              
#> jquerylib          NA                                                              
#> jsonlite           NA                                                              
#> kableExtra         NA                                                              
#> knitr              NA                                                              
#> labeling           NA                                                              
#> later              "Rcpp"                                                          
#> lazyeval           NA                                                              
#> lifecycle          NA                                                              
#> linprog            NA                                                              
#> lintr              NA                                                              
#> littler            NA                                                              
#> lme4               "Matrix (>= 1.5-0), Rcpp (>= 0.10.5), RcppEigen (>=\n0.3.3.9.4)"
#> lmom               NA                                                              
#> lmomco             NA                                                              
#> lmtest             NA                                                              
#> lpSolve            NA                                                              
#> lpjclass           NA                                                              
#> lpjmlkit           NA                                                              
#> lubridate          NA                                                              
#> lucode2            NA                                                              
#> luplot             NA                                                              
#> luscale            NA                                                              
#> lusweave           NA                                                              
#> madrat             NA                                                              
#> magclass           NA                                                              
#> magic              NA                                                              
#> magpie4            NA                                                              
#> magpiesets         NA                                                              
#> magrittr           NA                                                              
#> maps               NA                                                              
#> memoise            NA                                                              
#> microbenchmark     NA                                                              
#> mime               NA                                                              
#> miniUI             NA                                                              
#> minqa              "Rcpp"                                                          
#> minty              "cpp11 (>= 0.5.3), tzdb (>= 0.5.0)"                             
#> mip                NA                                                              
#> modelr             NA                                                              
#> mrcommons          NA                                                              
#> mrdownscale        NA                                                              
#> mrdrivers          NA                                                              
#> mrfaocore          NA                                                              
#> mrindustry         NA                                                              
#> mrlandcore         NA                                                              
#> mrremind           NA                                                              
#> mrtransport        NA                                                              
#> mstools            NA                                                              
#> ncdf4              NA                                                              
#> nleqslv            NA                                                              
#> nloptr             NA                                                              
#> nnls               NA                                                              
#> nonparaeff         NA                                                              
#> numDeriv           NA                                                              
#> openssl            NA                                                              
#> openxlsx           "Rcpp"                                                          
#> otel               NA                                                              
#> pak                NA                                                              
#> pbkrtest           NA                                                              
#> piamInterfaces     NA                                                              
#> piamPlotComparison NA                                                              
#> piamutils          NA                                                              
#> pillar             NA                                                              
#> pkgbuild           NA                                                              
#> pkgconfig          NA                                                              
#> pkgdown            NA                                                              
#> pkgload            NA                                                              
#> plotly             NA                                                              
#> plyr               "Rcpp"                                                          
#> praise             NA                                                              
#> prettyunits        NA                                                              
#> processx           NA                                                              
#> profvis            NA                                                              
#> promises           NA                                                              
#> ps                 NA                                                              
#> purrr              "cli"                                                           
#> qualV              NA                                                              
#> quantreg           NA                                                              
#> quitte             NA                                                              
#> ragg               "systemfonts, textshaping"                                      
#> rappdirs           NA                                                              
#> raster             "Rcpp"                                                          
#> rbibutils          NA                                                              
#> rcmdcheck          NA                                                              
#> readODS            "cpp11 (>= 0.5.0)"                                              
#> readr              "cpp11, tzdb (>= 0.1.1)"                                        
#> readxl             "cpp11 (>= 0.4.0), progress"                                    
#> reformulas         NA                                                              
#> rematch            NA                                                              
#> remind2            NA                                                              
#> remulator          NA                                                              
#> renv               NA                                                              
#> reporttransport    NA                                                              
#> reshape            NA                                                              
#> reshape2           "Rcpp"                                                          
#> rex                NA                                                              
#> rlang              NA                                                              
#> rmarkdown          NA                                                              
#> rmndt              NA                                                              
#> rootSolve          NA                                                              
#> roxygen2           "cpp11"                                                         
#> rprojroot          NA                                                              
#> rstudioapi         NA                                                              
#> rversions          NA                                                              
#> rworldmap          NA                                                              
#> sass               NA                                                              
#> scales             NA                                                              
#> sessioninfo        NA                                                              
#> shiny              NA                                                              
#> sourcetools        NA                                                              
#> sp                 NA                                                              
#> spam               "Rcpp"                                                          
#> stringi            NA                                                              
#> stringr            NA                                                              
#> svglite            "cpp11, systemfonts, textshaping"                               
#> sys                NA                                                              
#> systemfonts        "cpp11 (>= 0.2.1)"                                              
#> terra              "Rcpp"                                                          
#> testthat           NA                                                              
#> textshaping        "cpp11 (>= 0.2.1), systemfonts (>= 1.0.0)"                      
#> tibble             NA                                                              
#> tidyr              "cpp11 (>= 0.4.0)"                                              
#> tidyselect         NA                                                              
#> tidytemplate       NA                                                              
#> timeDate           NA                                                              
#> timechange         "cpp11 (>= 0.2.7)"                                              
#> tinytex            NA                                                              
#> trafficlight       NA                                                              
#> tzdb               "cpp11 (>= 0.5.2)"                                              
#> urca               NA                                                              
#> urlchecker         NA                                                              
#> usethis            NA                                                              
#> utf8               NA                                                              
#> vctrs              NA                                                              
#> viridisLite        NA                                                              
#> vroom              "cpp11 (>= 0.2.0), progress (>= 1.2.3), tzdb (>= 0.1.1)"        
#> waldo              NA                                                              
#> whisker            NA                                                              
#> whoami             NA                                                              
#> withr              NA                                                              
#> writexl            NA                                                              
#> xfun               NA                                                              
#> xml2               NA                                                              
#> xmlparsedata       NA                                                              
#> xopen              NA                                                              
#> xtable             NA                                                              
#> yaml               NA                                                              
#> ymlthis            NA                                                              
#> zip                NA                                                              
#> zoo                NA                                                              
#> KernSmooth         NA                                                              
#> MASS               NA                                                              
#> Matrix             NA                                                              
#> base               NA                                                              
#> boot               NA                                                              
#> class              NA                                                              
#> cluster            NA                                                              
#> codetools          NA                                                              
#> compiler           NA                                                              
#> datasets           NA                                                              
#> foreign            NA                                                              
#> grDevices          NA                                                              
#> graphics           NA                                                              
#> grid               NA                                                              
#> lattice            NA                                                              
#> methods            NA                                                              
#> mgcv               NA                                                              
#> nlme               NA                                                              
#> nnet               NA                                                              
#> parallel           NA                                                              
#> rpart              NA                                                              
#> spatial            NA                                                              
#> splines            NA                                                              
#> stats              NA                                                              
#> stats4             NA                                                              
#> survival           NA                                                              
#> tcltk              NA                                                              
#> tools              NA                                                              
#> utils              NA                                                              
#>                    Suggests                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> Deriv              "testthat (>= 0.11.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> Formula            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> GDPuc              "countrycode, covr, knitr, magclass, madrat (>= 3.6.4), purrr,\nrmarkdown, stringr, testthat (>= 3.0.0), usethis, WDI, zoo"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> Hmisc              "survival, qreport, acepack, chron, rms, mice, rstudioapi,\ntables, plotly (>= 4.5.6), rlang, VGAM, leaps, pcaPP, digest,\nparallel, polspline, abind, kableExtra, rio, lattice,\nlatticeExtra, gt, sparkline, jsonlite, htmlwidgets, qs,\ngetPass, keyring, safer, htm2txt, boot"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> Lmoments           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> MatrixModels       NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> R.methodsS3        "codetools"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> R.oo               "tools"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> R.utils            "datasets, digest (>= 0.6.10)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> R6                 "lobstr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> RColorBrewer       NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> Rcpp               "tinytest, inline, rbenchmark, pkgKitten (>= 0.1.2)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> Rdpack             "grDevices, testthat, rstudioapi, rprojroot, gbRd"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> S7                 "bench, callr, covr, knitr, methods, rmarkdown, testthat (>=\n3.2.0), tibble"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> SPEI               "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> SparseM            "knitr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> TLMoments          "evd, knitr, magrittr, lmom, Lmoments, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> abind              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> askpass            "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> assertr            "knitr, testthat (>= 3.0.0), magrittr, rmarkdown, tibble, covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> backports          NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> base64enc          NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> bit                "testthat (>= 3.0.0), roxygen2, knitr, markdown, rmarkdown,\nmicrobenchmark, bit64 (>= 4.0.0), ff (>= 4.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> bit64              "patrick (>= 0.3.0), testthat (>= 3.3.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> brew               "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> brio               "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> broom              "AER, AUC, bbmle, betareg (>= 3.2-1), biglm, binGroup, boot,\nbtergm (>= 1.10.6), car (>= 3.1-2), carData, caret, cluster,\ncmprsk, coda, covr, drc, e1071, emmeans, epiR (>= 2.0.85), ergm\n(>= 3.10.4), fixest (>= 0.9.0), gam (>= 1.15), gee, geepack,\nggplot2, glmnet, glmnetUtils, gmm, Hmisc, interp, irlba,\njoineRML, Kendall, knitr, ks, Lahman, lavaan (>= 0.6.18),\nleaps, lfe, lm.beta, lme4, lmodel2, lmtest (>= 0.9.38),\nlsmeans, maps, margins, MASS, mclust, mediation, metafor, mfx,\nmgcv, mlogit, modeldata, modeltests (>= 0.1.6), muhaz,\nmultcomp, network, nnet, ordinal, plm, poLCA, psych, quantreg,\nrmarkdown, robust, robustbase, rsample, sandwich, spatialreg,\nspdep (>= 1.1), speedglm, spelling, stats4, survey, survival\n(>= 3.6-4), systemfit, testthat (>= 3.0.0), tseries, vars, zoo"
#> bslib              "brand.yml, bsicons, curl, fontawesome, future, ggplot2,\nknitr, lattice, magrittr, rappdirs, rmarkdown (>= 2.7), shiny\n(>= 1.11.1), testthat, thematic, tools, utils, withr, yaml"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> cachem             "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> callr              "asciicast (>= 2.3.1), cli (>= 1.1.0), mockery, ps, rprojroot,\nspelling, testthat (>= 3.2.0), withr (>= 2.3.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> car                "alr4, boot, coxme, effects, knitr, leaps, lmtest, Matrix,\nMatrixModels, ordinal, plotrix, mvtnorm, rgl (>= 0.111.3), rio,\nsandwich, SparseM, survival, survey"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> carData            "car (>= 3.0-0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> cellranger         "covr, testthat (>= 1.0.0), knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> checkmate          "R6, fastmatch, data.table (>= 1.9.8), devtools, ggplot2,\nknitr, magrittr, microbenchmark, rmarkdown, testthat (>=\n3.0.4), tinytest (>= 1.1.0), tibble"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> citation           "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> cli                "callr, covr, crayon, digest, glue (>= 1.6.0), grDevices,\nhtmltools, htmlwidgets, knitr, methods, processx, ps (>=\n1.3.4.9000), rlang (>= 1.0.2.9003), rmarkdown, rprojroot,\nrstudioapi, testthat (>= 3.2.0), tibble, whoami, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> clipr              "covr, knitr, rmarkdown, rstudioapi (>= 0.5), testthat (>=\n2.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> collections        "testthat (>= 2.3.1)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> colorspace         "datasets, utils, KernSmooth, MASS, kernlab, mvtnorm, vcd,\ntcltk, shiny, shinyjs, ggplot2, dplyr, scales, grid, png, jpeg,\nknitr, rmarkdown, RColorBrewer, rcartocolor, scico, viridis,\nwesanderson"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> commonmark         "curl, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> contfrac           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> countrycode        "altdoc, eurostat, testthat, tibble, ISOcodes, utf8"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> covr               "R6, S7 (>= 0.2.0), curl, knitr, rmarkdown, htmltools, DT (>=\n0.2), testthat (>= 3.0.0), rlang, rstudioapi (>= 0.2), xml2 (>=\n1.0.0), parallel, memoise, covr, box (>= 1.2.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> cowplot            "Cairo, covr, dplyr, forcats, gridGraphics (>= 0.4-0), knitr,\nlattice, magick, maps, PASWR, patchwork, rmarkdown, ragg,\ntestthat (>= 1.0.0), tidyr, vdiffr (>= 0.3.0), VennDiagram"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> crayon             "mockery, rstudioapi, testthat, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> credentials        "testthat, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> crosstalk          "bslib, ggplot2, sass, shiny, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> curl               "spelling, testthat (>= 1.0.0), knitr, jsonlite, later,\nrmarkdown, httpuv (>= 1.4.4), webutils"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> data.table         "bit64 (>= 4.0.0), bit (>= 4.0.4), R.utils (>= 2.13.0), xts,\nzoo (>= 1.8-1), yaml, knitr, markdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> deSolve            "scatterplot3d, nlme, FME"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> desc               "callr, covr, gh, spelling, testthat, whoami, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> devtools           "BiocManager (>= 1.30.18), callr (>= 3.7.1), covr (>= 3.5.1),\ncurl (>= 4.3.2), digest (>= 0.6.29), DT (>= 0.23), foghorn (>=\n1.4.2), gh (>= 1.3.0), httr2 (>= 1.0.0), knitr (>= 1.39), lintr\n(>= 3.0.0), quarto (>= 1.5.1), remotes (>= 2.5.0), rmarkdown\n(>= 2.14), rstudioapi (>= 0.13), spelling (>= 2.2), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> diffobj            "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> digest             "tinytest, simplermarkdown, rbenchmark"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> doBy               "geepack, knitr, lme4, markdown, rmarkdown, multcomp, pbkrtest\n(>= 0.5.2), survival, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> doParallel         "caret, mlbench, rpart, RUnit"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> docopt             "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> dotCall64          "microbenchmark, RhpcBLASctl, RColorBrewer, roxygen2, spam,\ntestthat,"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> downlit            "covr, htmltools, jsonlite, MASS, MassSpecWavelet, pkgload,\nrmarkdown, testthat (>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> dplyr              "broom, covr, DBI, dbplyr (>= 2.2.1), ggplot2, knitr, Lahman,\nlobstr, nycflights13, purrr, rmarkdown, RSQLite, stringi (>=\n1.7.6), testthat (>= 3.1.5), tidyr (>= 1.3.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> edgeTransport      "testthat (>= 3.0.0), knitr, rmarkdown, covr,\npiamPlotComparison, renv"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> ellipsis           "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> elliptic           "emulator, calibrator (>= 1.2-8), testthat, hypergeo"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> evaluate           "callr, covr, ggplot2 (>= 3.3.6), lattice, methods, pkgload,\nragg (>= 1.4.0), rlang (>= 1.1.5), knitr, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> fansi              "unitizer, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> farver             "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> fastmap            "testthat (>= 2.1.1)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> fields             "mapproj"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> filelock           "callr (>= 2.0.0), covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> fontawesome        "covr, dplyr (>= 1.0.8), gt (>= 0.9.0), knitr (>= 1.31),\ntestthat (>= 3.0.0), rsvg"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> forcats            "covr, dplyr, ggplot2, knitr, readr, rmarkdown, testthat (>=\n3.0.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> foreach            "randomForest, doMC, doParallel, testthat, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> forecast           "forecTheta, knitr, methods, rmarkdown, rticles, scales,\nseasonal, testthat (>= 3.3.0), uroot"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> fracdiff           "longmemo, forecast, urca"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> fs                 "covr, crayon, knitr, pillar (>= 1.0.0), rmarkdown, spelling,\ntestthat (>= 3.0.0), tibble (>= 1.1.0), vctrs (>= 0.3.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> gamstransfer       "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> gdx                "covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> gdx2               "covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> gdxdt              "gdxrrw, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> gdxrrw             NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> generics           "covr, pkgload, testthat (>= 3.0.0), tibble, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> geometry           "spelling, testthat, rgl, R.matlab, interp"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> gert               "spelling, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> ggplot2            "broom, covr, dplyr, ggplot2movies, hexbin, Hmisc, hms, knitr,\nmapproj, maps, MASS, mgcv, multcomp, munsell, nlme, profvis,\nquantreg, quarto, ragg (>= 1.2.6), RColorBrewer, roxygen2,\nrpart, sf (>= 0.7-3), svglite (>= 2.1.2), testthat (>= 3.1.5),\ntibble, vdiffr (>= 1.0.6), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> gh                 "connectcreds, covr, knitr, rmarkdown, rprojroot, spelling,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> gitcreds           "codetools, covr, knitr, mockery, oskeyring, rmarkdown,\ntestthat (>= 3.0.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> glue               "crayon, DBI (>= 1.2.0), dplyr, knitr, rlang, rmarkdown,\nRSQLite, testthat (>= 3.2.0), vctrs (>= 0.3.0), waldo (>=\n0.5.3), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> gms                "covr, curl, magclass, qgraph, testthat, callr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> goftest            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> gridExtra          "ggplot2, egg, lattice, knitr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> gtable             "covr, ggplot2, knitr, profvis, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> hdf5r              "testthat, knitr, rmarkdown, nycflights13, reshape2, formatR,"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> highr              "knitr, markdown, testit"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> hms                "crayon, lubridate, pillar (>= 1.1.0), testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> htmlTable          "testthat, XML, xml2, Hmisc, rmarkdown, chron, lubridate,\ntibble, purrr, tidyselect, glue, rlang, tidyr (>= 0.7.2), dplyr\n(>= 0.7.4)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> htmltools          "Cairo, markdown, ragg, shiny, testthat, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> htmlwidgets        "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> httpuv             "callr, curl, jsonlite, testthat (>= 3.0.0), websocket"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> httr               "covr, httpuv, jpeg, knitr, png, readr, rmarkdown, testthat\n(>= 0.8.0), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> httr2              "askpass, bench, clipr, covr, docopt, httpuv, jose, jsonlite,\nknitr, later (>= 1.4.0), nanonext, otel (>= 0.2.0), otelsdk (>=\n0.2.0), paws.common (>= 0.8.0), promises, rmarkdown, testthat\n(>= 3.1.8), tibble, webfakes (>= 1.4.0), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> hypergeo           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> igraph             "ape (>= 5.7-0.1), callr, decor, digest, igraphdata, knitr,\nrgl (>= 1.3.14), rmarkdown, scales, stats4, tcltk, testthat,\nvdiffr, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> ini                "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> isoband            "covr, ggplot2, knitr, magick, bench, rmarkdown, sf, testthat\n(>= 3.0.0), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> iterators          "RUnit, foreach"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> jquerylib          "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> jsonlite           "httr, vctrs, testthat, knitr, rmarkdown, R.rsp, sf"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> kableExtra         "testthat, magick, tinytex, formattable, sparkline, webshot2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> knitr              "bslib, DBI (>= 0.4-1), digest, formatR, gifski, gridSVG,\nhtmlwidgets (>= 0.7), jpeg, JuliaCall (>= 0.11.1), magick,\nlitedown, markdown (>= 1.3), otel, otelsdk, png, ragg,\nreticulate (>= 1.4), rgl (>= 0.95.1201), rlang, rmarkdown,\nsass, showtext, styler (>= 1.2.0), targets (>= 0.6.0), testit,\ntibble, tikzDevice (>= 0.10), tinytex (>= 0.56), webshot,\nrstudioapi, svglite"                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> labeling           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> later              "knitr, nanonext, promises, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> lazyeval           "knitr, rmarkdown (>= 0.2.65), testthat, covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> lifecycle          "covr, knitr, lintr (>= 3.1.0), rmarkdown, testthat (>=\n3.0.1), tibble, tidyverse, tools, vctrs, withr, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> linprog            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> lintr              "bookdown, cyclocomp, jsonlite, patrick (>= 0.2.0), rlang,\nrmarkdown, rstudioapi (>= 0.2), testthat (>= 3.2.1), tibble,\ntufte, withr (>= 2.5.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> littler            "simplermarkdown, docopt, rcmdcheck, whoami"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> lme4               "HSAUR3, MEMSS, car, dfoptim, gamm4, ggplot2, glmmTMB, knitr,\nmerDeriv, mgcv, mlmRev, numDeriv, optimx (>= 2013.8.6),\npbkrtest, rmarkdown, rr2, semEff, statmod, testthat (>= 0.8.1),\ntibble"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> lmom               NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> lmomco             "copBasic"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> lmtest             "car, strucchange, sandwich, dynlm, stats4, survival, AER"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> lpSolve            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> lpjclass           "covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> lpjmlkit           "rmarkdown, knitr, testthat (>= 3.0.0), terra, raster,\nreshape2, maps, sf, ncdf4, CFtime, R6"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> lubridate          "covr, knitr, rmarkdown, testthat (>= 2.1.0), vctrs (>= 0.6.5)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> lucode2            "covr, gdx, gdxrrw, gert, ggplot2, knitr, lusweave, magclass,\npoorman, renv, rmarkdown, styler, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> luplot             "covr, rworldmap (>= 1.3.8), lemon"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> luscale            "covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> lusweave           "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> madrat             "covr, ggplot2, graphics, grDevices, knitr, rmarkdown, terra,\ntestthat, tibble"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> magclass           "covr, ggplot2, knitr, lpjmlkit, ncdf4, pkgconfig, quitte,\nraster, rmarkdown, terra, testthat (>= 3.1.5), tibble, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> magic              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> magpie4            "covr, filelock, FRACTION, ncdf4, terra, testthat (>= 3.3.2),\nwithr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> magpiesets         "covr, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> magrittr           "covr, knitr, rlang, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> maps               "mapproj (>= 1.2-0), mapdata (>= 2.3.0), sf, rnaturalearth"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> memoise            "digest, aws.s3, covr, googleAuthR, googleCloudStorageR, httr,\ntestthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> microbenchmark     "ggplot2, multcomp, RUnit"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> mime               NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> miniUI             NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> minqa              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> minty              "knitr, stringi, testthat, withr, hms, readr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> mip                "gdxrrw, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> modelr             "compiler, covr, ggplot2, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> mrcommons          "covr, HARr, rmarkdown, testthat, XML"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> mrdownscale        "covr, knitr, mrlandcore, rmarkdown, testthat, usethis"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> mrdrivers          "covr, crayon, knitr, rmarkdown, testthat (>= 3.0.0), WDI,\nwithr (>= 2.4.2), yaml, zoo"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> mrfaocore          "testthat, XML"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> mrindustry         "mrremind (>= 0.220.0), testthat,"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> mrlandcore         "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> mrremind           "covr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> mrtransport        NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> mstools            "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> ncdf4              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> nleqslv            "testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> nloptr             "knitr, rmarkdown, covr, tinytest"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> nnls               "bvls, quadprog"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> nonparaeff         "pwt, psych"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> numDeriv           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> openssl            "curl, testthat (>= 2.1.0), digest, knitr, rmarkdown,\njsonlite, jose, sodium"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> openxlsx           "curl, formula.tools, knitr, rmarkdown, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> otel               "callr, cli, glue, jsonlite, otelsdk, processx, shiny,\nspelling, testthat (>= 3.0.0), utils, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> pak                "callr (>= 3.7.0), cli (>= 3.2.0), covr, curl (>= 4.3.2), desc\n(>= 1.4.1), filelock (>= 1.0.2), gitcreds, glue (>= 1.6.2),\njsonlite (>= 1.8.0), keyring (>= 1.4.0), pingr, pkgbuild (>=\n1.4.2), pkgcache (>= 2.2.4), pkgdepends (>= 0.9.0), pkgload,\npkgsearch (>= 3.1.0), processx (>= 3.8.1), ps (>= 1.6.0),\nrstudioapi, testthat (>= 3.2.0), webfakes, withr, yaml"                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> pbkrtest           "nlme, markdown, knitr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> piamInterfaces     "covr, testthat (>= 3.2.3), withr, writexl"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> piamPlotComparison "gridExtra, testthat, tidyverse"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> piamutils          "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> pillar             "bit64, DBI, debugme, DiagrammeR, dplyr, formattable, ggplot2,\nknitr, lubridate, nanotime, nycflights13, palmerpenguins,\nrmarkdown, scales, stringi, survival, testthat (>= 3.1.1),\ntibble, units (>= 0.7.2), vdiffr, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> pkgbuild           "covr, cpp11, knitr, Rcpp, rmarkdown, testthat (>= 3.2.0),\nwithr (>= 2.3.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> pkgconfig          "covr, testthat, disposables (>= 1.0.3)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> pkgdown            "covr, diffviewer, evaluate (>= 0.24.0), gert, gt, htmltools,\nhtmlwidgets, knitr (>= 1.50), magick, methods, pkgload (>=\n1.0.2), quarto, rsconnect, rstudioapi, rticles, sass, testthat\n(>= 3.1.3), tools"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> pkgload            "bitops, jsonlite, mathjaxr, pak, Rcpp, remotes, rstudioapi,\ntestthat (>= 3.2.1.1), usethis, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> plotly             "MASS, maps, hexbin, ggthemes, GGally, ggalluvial, testthat,\nknitr, shiny (>= 1.1.0), shinytest2, curl, rmarkdown, Cairo,\nbroom, webshot, listviewer, dendextend, sf, png, IRdisplay,\nprocessx, plotlyGeoAssets, forcats, withr, palmerpenguins,\nrversions, reticulate, rsvg, ggridges"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> plyr               "abind, covr, doParallel, foreach, iterators, itertools,\ntcltk, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> praise             "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> prettyunits        "codetools, covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> processx           "callr (>= 3.7.3), cli (>= 3.3.0), codetools, covr, curl,\ndebugme, parallel, rlang (>= 1.0.2), testthat (>= 3.0.0),\nwebfakes, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> profvis            "htmltools, knitr, rmarkdown, shiny, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> promises           "future (>= 1.21.0), knitr, mirai, otelsdk (>= 0.2.0), purrr,\nRcpp, rmarkdown, spelling, testthat (>= 3.0.0), vembedr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> ps                 "callr, covr, curl, pillar, pingr, processx (>= 3.1.0), R6,\nrlang, testthat (>= 3.0.0), webfakes, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> purrr              "carrier (>= 0.3.0), covr, dplyr (>= 0.7.8), httr, knitr,\nlubridate, mirai (>= 2.5.1), rmarkdown, testthat (>= 3.0.0),\ntibble, tidyselect"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> qualV              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> quantreg           "interp, rgl, logspline, nor1mix, Formula, zoo, R.rsp, conquer"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> quitte             "covr, gdxrrw, knitr, mip, rmarkdown, testthat (>= 3.2.0),\ntidyverse, withr,"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> ragg               "covr, graphics, grid, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> rappdirs           "covr, roxygen2, testthat (>= 3.2.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> raster             "ncdf4, igraph, tcltk, parallel, rasterVis, MASS, sf,\ntinytest, gstat, fields, exactextractr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> rbibutils          "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> rcmdcheck          "covr, knitr, mockery, processx, ps, rmarkdown, svglite,\ntestthat, webfakes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> readODS            "spelling, testthat, datasets, covr, knitr, rmarkdown, readr\n(>= 1.2.1)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> readr              "covr, curl, datasets, knitr, rmarkdown, spelling, stringi,\ntestthat (>= 3.2.0), tzdb (>= 0.1.1), waldo, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> readxl             "covr, knitr, rmarkdown, testthat (>= 3.1.6), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> reformulas         "lme4, tinytest, glmmTMB, Formula"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> rematch            "covr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> remind2            "covr, gridExtra, htmltools, kableExtra, knitr, scales,\ntestthat, tidyverse"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> remulator          "covr, knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> renv               "BiocManager, cli, compiler, covr, cpp11, curl, devtools,\ngenerics, gitcreds, jsonlite, jsonvalidate, knitr, miniUI,\nmodules, packrat, pak, R6, remotes, reticulate, rmarkdown,\nrstudioapi, shiny, testthat, uuid, waldo, yaml, webfakes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> reporttransport    "knitr, sf, testthat, edgeTransport"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> reshape            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> reshape2           "covr, lattice, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> rex                "covr, dplyr, ggplot2, Hmisc, knitr, magrittr, rmarkdown,\nroxygen2, rvest, stringr, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> rlang              "cli (>= 3.1.0), covr, crayon, desc, fs, glue, knitr,\nmagrittr, methods, pillar, pkgload, rmarkdown, stats, testthat\n(>= 3.3.2), tibble, usethis, vctrs (>= 0.2.3), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> rmarkdown          "digest, dygraphs, fs, rsconnect, downlit (>= 0.4.0), katex\n(>= 1.4.0), sass (>= 0.4.0), shiny (>= 1.6.0), testthat (>=\n3.0.3), tibble, vctrs, cleanrmd, withr (>= 2.4.2), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> rmndt              "testthat, magclass, covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> rootSolve          NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> roxygen2           "covr, R.methodsS3, S7, R.oo, rmarkdown (>= 2.16), testthat\n(>= 3.1.2), yaml"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> rprojroot          "covr, knitr, lifecycle, rlang, rmarkdown, testthat (>=\n3.2.0), withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> rstudioapi         "testthat, knitr, rmarkdown, clipr, covr, curl, jsonlite,\nwithr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> rversions          "pillar, testthat (>= 3.0.0), webfakes, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> rworldmap          "rworldxtra, RColorBrewer, classInt, sf"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> sass               "testthat, knitr, rmarkdown, withr, shiny, curl"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> scales             "bit64, covr, dichromat, ggplot2, hms (>= 0.5.0), stringi,\ntestthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> sessioninfo        "callr, covr, gh, reticulate, rmarkdown, testthat (>= 3.2.0),\nwithr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> shiny              "Cairo (>= 1.5-5), coro (>= 1.1.0), datasets, DT, dygraphs,\nfuture, ggplot2, knitr (>= 1.6), magrittr, markdown, mirai,\notelsdk (>= 0.2.0), ragg, reactlog (>= 1.0.0), rmarkdown, sass,\nshowtext, testthat (>= 3.2.1), watcher, yaml"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> sourcetools        "testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> sp                 "RColorBrewer, gstat, deldir, knitr, maps, mapview, rmarkdown,\nsf, terra, raster"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> spam               "spam64, fields, Matrix, testthat, R.rsp, truncdist, knitr,\nrmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> stringi            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> stringr            "covr, dplyr, gt, htmltools, htmlwidgets, knitr, rmarkdown,\ntestthat (>= 3.0.0), tibble"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> svglite            "covr, fontquiver (>= 0.2.0), htmltools, knitr, rmarkdown,\ntestthat (>= 3.0.0), xml2 (>= 1.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> sys                "unix (>= 1.4), spelling, testthat"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> systemfonts        "covr, farver, ggplot2, graphics, knitr, ragg, rmarkdown,\nsvglite, testthat (>= 2.1.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> terra              "parallel, tinytest, ncdf4, sf (>= 0.9-8), deldir, XML,\nleaflet (>= 2.2.1), htmlwidgets, future, future.apply"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> testthat           "covr, curl (>= 0.9.5), diffviewer (>= 0.1.0), digest (>=\n0.6.33), gh, knitr, otel, otelsdk, rmarkdown, rstudioapi, S7,\nshiny, usethis, vctrs (>= 0.1.0), xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> textshaping        "covr, grDevices, grid, knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> tibble             "bench, bit64, blob, brio, callr, DiagrammeR, dplyr, evaluate,\nformattable, ggplot2, here, hms, htmltools, knitr, lubridate,\nnycflights13, pkgload, purrr, rmarkdown, stringi, testthat (>=\n3.0.2), tidyr, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> tidyr              "covr, data.table, knitr, readr, repurrrsive (>= 1.1.0),\nrmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> tidyselect         "covr, crayon, dplyr, knitr, magrittr, rmarkdown, stringr,\ntestthat (>= 3.1.1), tibble (>= 2.1.3)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> tidytemplate       "knitr, rmarkdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> timeDate           "RUnit"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> timechange         "testthat (>= 0.7.1.99), knitr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> tinytex            "testit, rstudioapi"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> trafficlight       NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> tzdb               "covr, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> urca               NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> urlchecker         "covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> usethis            "covr, knitr, magick, pkgload (>= 1.3.2.1), quarto (>= 1.5.1),\nrmarkdown, roxygen2 (>= 7.1.2), spelling (>= 1.2), testthat (>=\n3.1.8)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> utf8               "cli, covr, knitr, rlang, rmarkdown, testthat (>= 3.0.0),\nwithr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> vctrs              "bit64, covr, crayon, dplyr (>= 0.8.5), generics, knitr,\npillar (>= 1.4.4), pkgdown (>= 2.0.1), rmarkdown, testthat (>=\n3.0.0), tibble (>= 3.1.3), waldo (>= 0.2.0), withr, xml2,\nzeallot"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> viridisLite        "hexbin (>= 1.27.0), ggplot2 (>= 1.0.1), testthat, covr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> vroom              "archive, bench (>= 1.1.0), covr, curl, dplyr, forcats, fs,\nggplot2, knitr, patchwork, prettyunits, purrr, rmarkdown,\nrstudioapi, scales, spelling, testthat (>= 2.1.0), tidyr,\nutils, waldo, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> waldo              "bit64, R6, S7, testthat (>= 3.0.0), withr, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> whisker            "markdown"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> whoami             "covr, mockery, testthat, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> withr              "callr, DBI, knitr, methods, rlang, rmarkdown (>= 2.12),\nRSQLite, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> writexl            "spelling, readxl, nycflights13, testthat, bit64"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> xfun               "testit, parallel, codetools, methods, rstudioapi, tinytex (>=\n0.30), mime, litedown (>= 0.6), commonmark, knitr (>= 1.50),\nremotes, pak, curl, xml2, jsonlite, magick, yaml, data.table,\nqs2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> xml2               "covr, curl, httr, knitr, mockery, rmarkdown, testthat (>=\n3.2.0), xslt"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> xmlparsedata       "covr, testthat, xml2"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> xopen              "ps, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> xtable             "knitr, zoo, survival, glue, tinytex"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> yaml               "knitr, rmarkdown, testthat (>= 3.0.0)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> ymlthis            "blogdown, bookdown, covr, knitr, miniUI, pkgdown, prettydoc,\nroxygen2 (>= 7.0.0), shiny, shinyBS, spelling, testthat (>=\n3.0.0), xaringan"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> zip                "covr, pillar, processx, R6, testthat, withr"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> zoo                "AER, coda, chron, ggplot2 (>= 3.5.0), mondate, scales,\nstinepack, strucchange, timeDate, timeSeries, tinyplot, tis,\ntseries, xts"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> KernSmooth         "MASS, carData"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> MASS               "lattice, nlme, nnet, survival"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> Matrix             "MASS, datasets, sfsmisc, tools"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> base               "methods"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> boot               "MASS, survival"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> class              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> cluster            "MASS, Matrix"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> codetools          NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> compiler           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> datasets           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> foreign            NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> grDevices          "KernSmooth"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> graphics           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> grid               NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> lattice            "KernSmooth, MASS, latticeExtra, colorspace"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> methods            "codetools"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> mgcv               "parallel, survival, MASS"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> nlme               "MASS, SASmixed"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> nnet               "MASS"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> parallel           "methods"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> rpart              "survival"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> spatial            "MASS"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> splines            "Matrix, methods"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> stats              "MASS, Matrix, SuppDists, methods, stats4"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> stats4             NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> survival           NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> tcltk              NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> tools              "codetools, methods, xml2, curl, commonmark, knitr, xfun, mathjaxr, V8"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> utils              "methods, xml2, commonmark, knitr, jsonlite"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#>                    Enhances                                               
#> Deriv              NA                                                     
#> Formula            NA                                                     
#> GDPuc              NA                                                     
#> Hmisc              NA                                                     
#> Lmoments           NA                                                     
#> MatrixModels       NA                                                     
#> R.methodsS3        NA                                                     
#> R.oo               NA                                                     
#> R.utils            NA                                                     
#> R6                 NA                                                     
#> RColorBrewer       NA                                                     
#> Rcpp               NA                                                     
#> Rdpack             NA                                                     
#> S7                 NA                                                     
#> SPEI               NA                                                     
#> SparseM            NA                                                     
#> TLMoments          NA                                                     
#> abind              NA                                                     
#> askpass            NA                                                     
#> assertr            NA                                                     
#> backports          NA                                                     
#> base64enc          "png"                                                  
#> bit                NA                                                     
#> bit64              NA                                                     
#> brew               NA                                                     
#> brio               NA                                                     
#> broom              NA                                                     
#> bslib              NA                                                     
#> cachem             NA                                                     
#> callr              NA                                                     
#> car                NA                                                     
#> carData            NA                                                     
#> cellranger         NA                                                     
#> checkmate          NA                                                     
#> citation           NA                                                     
#> cli                NA                                                     
#> clipr              NA                                                     
#> collections        NA                                                     
#> colorspace         NA                                                     
#> commonmark         NA                                                     
#> contfrac           NA                                                     
#> countrycode        NA                                                     
#> covr               NA                                                     
#> cowplot            NA                                                     
#> crayon             NA                                                     
#> credentials        NA                                                     
#> crosstalk          NA                                                     
#> curl               NA                                                     
#> data.table         NA                                                     
#> deSolve            NA                                                     
#> desc               NA                                                     
#> devtools           NA                                                     
#> diffobj            NA                                                     
#> digest             NA                                                     
#> doBy               NA                                                     
#> doParallel         "compiler"                                             
#> docopt             NA                                                     
#> dotCall64          NA                                                     
#> downlit            NA                                                     
#> dplyr              NA                                                     
#> edgeTransport      NA                                                     
#> ellipsis           NA                                                     
#> elliptic           NA                                                     
#> evaluate           NA                                                     
#> fansi              NA                                                     
#> farver             NA                                                     
#> fastmap            NA                                                     
#> fields             NA                                                     
#> filelock           NA                                                     
#> fontawesome        NA                                                     
#> forcats            NA                                                     
#> foreach            NA                                                     
#> forecast           NA                                                     
#> fracdiff           NA                                                     
#> fs                 NA                                                     
#> gamstransfer       NA                                                     
#> gdx                NA                                                     
#> gdx2               NA                                                     
#> gdxdt              NA                                                     
#> gdxrrw             NA                                                     
#> generics           NA                                                     
#> geometry           NA                                                     
#> gert               NA                                                     
#> ggplot2            "sp"                                                   
#> gh                 NA                                                     
#> gitcreds           NA                                                     
#> glue               NA                                                     
#> gms                NA                                                     
#> goftest            NA                                                     
#> gridExtra          NA                                                     
#> gtable             NA                                                     
#> hdf5r              NA                                                     
#> highr              NA                                                     
#> hms                NA                                                     
#> htmlTable          NA                                                     
#> htmltools          "knitr"                                                
#> htmlwidgets        "shiny (>= 1.1)"                                       
#> httpuv             NA                                                     
#> httr               NA                                                     
#> httr2              NA                                                     
#> hypergeo           NA                                                     
#> igraph             "graph"                                                
#> ini                NA                                                     
#> isoband            NA                                                     
#> iterators          NA                                                     
#> jquerylib          NA                                                     
#> jsonlite           NA                                                     
#> kableExtra         NA                                                     
#> knitr              NA                                                     
#> labeling           NA                                                     
#> later              NA                                                     
#> lazyeval           NA                                                     
#> lifecycle          NA                                                     
#> linprog            NA                                                     
#> lintr              "data.table"                                           
#> littler            NA                                                     
#> lme4               "DHARMa, performance"                                  
#> lmom               NA                                                     
#> lmomco             NA                                                     
#> lmtest             NA                                                     
#> lpSolve            NA                                                     
#> lpjclass           NA                                                     
#> lpjmlkit           NA                                                     
#> lubridate          "chron, data.table, timeDate, tis, zoo"                
#> lucode2            NA                                                     
#> luplot             NA                                                     
#> luscale            NA                                                     
#> lusweave           NA                                                     
#> madrat             NA                                                     
#> magclass           NA                                                     
#> magic              NA                                                     
#> magpie4            NA                                                     
#> magpiesets         NA                                                     
#> magrittr           NA                                                     
#> maps               NA                                                     
#> memoise            NA                                                     
#> microbenchmark     NA                                                     
#> mime               NA                                                     
#> miniUI             NA                                                     
#> minqa              NA                                                     
#> minty              NA                                                     
#> mip                NA                                                     
#> modelr             NA                                                     
#> mrcommons          NA                                                     
#> mrdownscale        NA                                                     
#> mrdrivers          NA                                                     
#> mrfaocore          NA                                                     
#> mrindustry         NA                                                     
#> mrlandcore         NA                                                     
#> mrremind           NA                                                     
#> mrtransport        NA                                                     
#> mstools            NA                                                     
#> ncdf4              NA                                                     
#> nleqslv            NA                                                     
#> nloptr             NA                                                     
#> nnls               NA                                                     
#> nonparaeff         NA                                                     
#> numDeriv           NA                                                     
#> openssl            NA                                                     
#> openxlsx           NA                                                     
#> otel               NA                                                     
#> pak                NA                                                     
#> pbkrtest           NA                                                     
#> piamInterfaces     NA                                                     
#> piamPlotComparison NA                                                     
#> piamutils          NA                                                     
#> pillar             NA                                                     
#> pkgbuild           NA                                                     
#> pkgconfig          NA                                                     
#> pkgdown            NA                                                     
#> pkgload            NA                                                     
#> plotly             NA                                                     
#> plyr               NA                                                     
#> praise             NA                                                     
#> prettyunits        NA                                                     
#> processx           NA                                                     
#> profvis            NA                                                     
#> promises           NA                                                     
#> ps                 NA                                                     
#> purrr              NA                                                     
#> qualV              NA                                                     
#> quantreg           NA                                                     
#> quitte             NA                                                     
#> ragg               NA                                                     
#> rappdirs           NA                                                     
#> raster             NA                                                     
#> rbibutils          NA                                                     
#> rcmdcheck          NA                                                     
#> readODS            NA                                                     
#> readr              NA                                                     
#> readxl             NA                                                     
#> reformulas         NA                                                     
#> rematch            NA                                                     
#> remind2            NA                                                     
#> remulator          NA                                                     
#> renv               NA                                                     
#> reporttransport    NA                                                     
#> reshape            NA                                                     
#> reshape2           NA                                                     
#> rex                NA                                                     
#> rlang              "winch"                                                
#> rmarkdown          NA                                                     
#> rmndt              NA                                                     
#> rootSolve          NA                                                     
#> roxygen2           NA                                                     
#> rprojroot          NA                                                     
#> rstudioapi         NA                                                     
#> rversions          NA                                                     
#> rworldmap          NA                                                     
#> sass               NA                                                     
#> scales             NA                                                     
#> sessioninfo        NA                                                     
#> shiny              NA                                                     
#> sourcetools        NA                                                     
#> sp                 NA                                                     
#> spam               NA                                                     
#> stringi            NA                                                     
#> stringr            NA                                                     
#> svglite            NA                                                     
#> sys                NA                                                     
#> systemfonts        NA                                                     
#> terra              NA                                                     
#> testthat           NA                                                     
#> textshaping        NA                                                     
#> tibble             NA                                                     
#> tidyr              NA                                                     
#> tidyselect         NA                                                     
#> tidytemplate       NA                                                     
#> timeDate           NA                                                     
#> timechange         NA                                                     
#> tinytex            NA                                                     
#> trafficlight       NA                                                     
#> tzdb               NA                                                     
#> urca               NA                                                     
#> urlchecker         NA                                                     
#> usethis            NA                                                     
#> utf8               NA                                                     
#> vctrs              NA                                                     
#> viridisLite        NA                                                     
#> vroom              NA                                                     
#> waldo              NA                                                     
#> whisker            NA                                                     
#> whoami             NA                                                     
#> withr              NA                                                     
#> writexl            NA                                                     
#> xfun               NA                                                     
#> xml2               NA                                                     
#> xmlparsedata       NA                                                     
#> xopen              NA                                                     
#> xtable             NA                                                     
#> yaml               NA                                                     
#> ymlthis            NA                                                     
#> zip                NA                                                     
#> zoo                NA                                                     
#> KernSmooth         NA                                                     
#> MASS               NA                                                     
#> Matrix             "SparseM, graph"                                       
#> base               "chron, date, round"                                   
#> boot               NA                                                     
#> class              NA                                                     
#> cluster            "mvoutlier, fpc, ellipse, sfsmisc"                     
#> codetools          NA                                                     
#> compiler           NA                                                     
#> datasets           NA                                                     
#> foreign            NA                                                     
#> grDevices          NA                                                     
#> graphics           "vcd"                                                  
#> grid               NA                                                     
#> lattice            "chron, zoo"                                           
#> methods            NA                                                     
#> mgcv               NA                                                     
#> nlme               NA                                                     
#> nnet               NA                                                     
#> parallel           "snow, Rmpi, mirai"                                    
#> rpart              NA                                                     
#> spatial            NA                                                     
#> splines            NA                                                     
#> stats              "Kendall, coin, multcomp, pcaPP, pspearman, robustbase"
#> stats4             NA                                                     
#> survival           NA                                                     
#> tcltk              NA                                                     
#> tools              NA                                                     
#> utils              NA                                                     
#>                    License                                         
#> Deriv              "GPL (>= 3)"                                    
#> Formula            "GPL-2 | GPL-3"                                 
#> GDPuc              "GPL (>= 3)"                                    
#> Hmisc              "GPL (>= 2)"                                    
#> Lmoments           "GPL-2"                                         
#> MatrixModels       "GPL (>= 2)"                                    
#> R.methodsS3        "LGPL (>= 2.1)"                                 
#> R.oo               "LGPL (>= 2.1)"                                 
#> R.utils            "LGPL (>= 2.1)"                                 
#> R6                 "MIT + file LICENSE"                            
#> RColorBrewer       "Apache License 2.0"                            
#> Rcpp               "GPL (>= 2)"                                    
#> Rdpack             "GPL (>= 2)"                                    
#> S7                 "MIT + file LICENSE"                            
#> SPEI               "GPL-2"                                         
#> SparseM            "GPL (>= 2)"                                    
#> TLMoments          "GPL (>= 2)"                                    
#> abind              "MIT + file LICENSE"                            
#> askpass            "MIT + file LICENSE"                            
#> assertr            "MIT + file LICENSE"                            
#> backports          "GPL-2 | GPL-3"                                 
#> base64enc          "GPL-2 | GPL-3"                                 
#> bit                "GPL-2 | GPL-3"                                 
#> bit64              "GPL-2 | GPL-3"                                 
#> brew               "GPL (>= 2)"                                    
#> brio               "MIT + file LICENSE"                            
#> broom              "MIT + file LICENSE"                            
#> bslib              "MIT + file LICENSE"                            
#> cachem             "MIT + file LICENSE"                            
#> callr              "MIT + file LICENSE"                            
#> car                "GPL (>= 2)"                                    
#> carData            "GPL (>= 2)"                                    
#> cellranger         "MIT + file LICENSE"                            
#> checkmate          "BSD_3_clause + file LICENSE"                   
#> citation           "BSD_2_clause + file LICENSE"                   
#> cli                "MIT + file LICENSE"                            
#> clipr              "GPL-3"                                         
#> collections        "MIT + file LICENSE"                            
#> colorspace         "BSD_3_clause + file LICENSE"                   
#> commonmark         "BSD_2_clause + file LICENSE"                   
#> contfrac           "GPL-2"                                         
#> countrycode        "GPL-3"                                         
#> covr               "MIT + file LICENSE"                            
#> cowplot            "GPL-2"                                         
#> crayon             "MIT + file LICENSE"                            
#> credentials        "MIT + file LICENSE"                            
#> crosstalk          "MIT + file LICENSE"                            
#> curl               "MIT + file LICENSE"                            
#> data.table         "MPL-2.0 | file LICENSE"                        
#> deSolve            "GPL (>= 2)"                                    
#> desc               "MIT + file LICENSE"                            
#> devtools           "MIT + file LICENSE"                            
#> diffobj            "GPL-2 | GPL-3"                                 
#> digest             "GPL (>= 2)"                                    
#> doBy               "GPL (>= 2)"                                    
#> doParallel         "GPL-2"                                         
#> docopt             "MIT + file LICENSE"                            
#> dotCall64          "GPL (>= 2)"                                    
#> downlit            "MIT + file LICENSE"                            
#> dplyr              "MIT + file LICENSE"                            
#> edgeTransport      "GPL-3"                                         
#> ellipsis           "MIT + file LICENSE"                            
#> elliptic           "GPL-2"                                         
#> evaluate           "MIT + file LICENSE"                            
#> fansi              "GPL-2 | GPL-3"                                 
#> farver             "MIT + file LICENSE"                            
#> fastmap            "MIT + file LICENSE"                            
#> fields             "GPL (>= 2)"                                    
#> filelock           "MIT + file LICENSE"                            
#> fontawesome        "MIT + file LICENSE"                            
#> forcats            "MIT + file LICENSE"                            
#> foreach            "Apache License (== 2.0)"                       
#> forecast           "GPL-3"                                         
#> fracdiff           "GPL (>= 2)"                                    
#> fs                 "MIT + file LICENSE"                            
#> gamstransfer       "MIT + file LICENSE"                            
#> gdx                "BSD_2_clause + file LICENSE"                   
#> gdx2               "BSD_2_clause + file LICENSE"                   
#> gdxdt              "MIT + file LICENCE"                            
#> gdxrrw             "EPL2 with Secondary License GPL-2.0 or greater"
#> generics           "MIT + file LICENSE"                            
#> geometry           "GPL (>= 3)"                                    
#> gert               "MIT + file LICENSE"                            
#> ggplot2            "MIT + file LICENSE"                            
#> gh                 "MIT + file LICENSE"                            
#> gitcreds           "MIT + file LICENSE"                            
#> glue               "MIT + file LICENSE"                            
#> gms                "BSD_2_clause + file LICENSE"                   
#> goftest            "GPL (>= 2)"                                    
#> gridExtra          "GPL (>= 2)"                                    
#> gtable             "MIT + file LICENSE"                            
#> hdf5r              "Apache License 2.0 | file LICENSE"             
#> highr              "GPL"                                           
#> hms                "MIT + file LICENSE"                            
#> htmlTable          "GPL (>= 3)"                                    
#> htmltools          "GPL (>= 2)"                                    
#> htmlwidgets        "MIT + file LICENSE"                            
#> httpuv             "GPL (>= 2) | file LICENSE"                     
#> httr               "MIT + file LICENSE"                            
#> httr2              "MIT + file LICENSE"                            
#> hypergeo           "GPL-2"                                         
#> igraph             "GPL (>= 2)"                                    
#> ini                "GPL-3"                                         
#> isoband            "MIT + file LICENSE"                            
#> iterators          "Apache License (== 2.0)"                       
#> jquerylib          "MIT + file LICENSE"                            
#> jsonlite           "MIT + file LICENSE"                            
#> kableExtra         "MIT + file LICENSE"                            
#> knitr              "GPL"                                           
#> labeling           "MIT + file LICENSE | Unlimited"                
#> later              "MIT + file LICENSE"                            
#> lazyeval           "GPL-3"                                         
#> lifecycle          "MIT + file LICENSE"                            
#> linprog            "GPL (>= 2)"                                    
#> lintr              "MIT + file LICENSE"                            
#> littler            "GPL (>= 2)"                                    
#> lme4               "GPL (>= 2)"                                    
#> lmom               "Common Public License Version 1.0"             
#> lmomco             "GPL"                                           
#> lmtest             "GPL-2 | GPL-3"                                 
#> lpSolve            "LGPL-2"                                        
#> lpjclass           "LGPL-3 | file LICENSE"                         
#> lpjmlkit           "AGPL-3"                                        
#> lubridate          "MIT + file LICENSE"                            
#> lucode2            "BSD_2_clause + file LICENSE"                   
#> luplot             "LGPL-3 | file LICENSE"                         
#> luscale            "BSD_2_clause + file LICENSE"                   
#> lusweave           "BSD_2_clause + file LICENSE"                   
#> madrat             "BSD_2_clause + file LICENSE"                   
#> magclass           "LGPL-3 | file LICENSE"                         
#> magic              "GPL-2"                                         
#> magpie4            "LGPL-3 | file LICENSE"                         
#> magpiesets         "LGPL-3 | file LICENSE"                         
#> magrittr           "MIT + file LICENSE"                            
#> maps               "GPL-2"                                         
#> memoise            "MIT + file LICENSE"                            
#> microbenchmark     "BSD_2_clause + file LICENSE"                   
#> mime               "GPL"                                           
#> miniUI             "GPL-3"                                         
#> minqa              "GPL-2"                                         
#> minty              "MIT + file LICENSE"                            
#> mip                "BSD_2_clause + file LICENSE"                   
#> modelr             "GPL-3"                                         
#> mrcommons          "LGPL-3 | file LICENSE"                         
#> mrdownscale        "LGPL (>= 3)"                                   
#> mrdrivers          "LGPL (>= 3)"                                   
#> mrfaocore          "LGPL-3"                                        
#> mrindustry         "LGPL-3"                                        
#> mrlandcore         "LGPL-3"                                        
#> mrremind           "LGPL-3 | file LICENSE"                         
#> mrtransport        "LGPL-3 | file LICENSE"                         
#> mstools            "LGPL-3 | file LICENSE"                         
#> ncdf4              "GPL (>= 3)"                                    
#> nleqslv            "GPL (>= 2)"                                    
#> nloptr             "LGPL (>= 3)"                                   
#> nnls               "GPL (>= 2)"                                    
#> nonparaeff         "GPL (>= 2)"                                    
#> numDeriv           "GPL-2"                                         
#> openssl            "MIT + file LICENSE"                            
#> openxlsx           "MIT + file LICENSE"                            
#> otel               "MIT + file LICENSE"                            
#> pak                "GPL-3"                                         
#> pbkrtest           "GPL (>= 2)"                                    
#> piamInterfaces     "LGPL-3"                                        
#> piamPlotComparison "LGPL-3"                                        
#> piamutils          "LGPL-3"                                        
#> pillar             "MIT + file LICENSE"                            
#> pkgbuild           "MIT + file LICENSE"                            
#> pkgconfig          "MIT + file LICENSE"                            
#> pkgdown            "MIT + file LICENSE"                            
#> pkgload            "MIT + file LICENSE"                            
#> plotly             "MIT + file LICENSE"                            
#> plyr               "MIT + file LICENSE"                            
#> praise             "MIT + file LICENSE"                            
#> prettyunits        "MIT + file LICENSE"                            
#> processx           "MIT + file LICENSE"                            
#> profvis            "MIT + file LICENSE"                            
#> promises           "MIT + file LICENSE"                            
#> ps                 "MIT + file LICENSE"                            
#> purrr              "MIT + file LICENSE"                            
#> qualV              "GPL (>= 2)"                                    
#> quantreg           "GPL (>= 2)"                                    
#> quitte             "GPL-2"                                         
#> ragg               "MIT + file LICENSE"                            
#> rappdirs           "MIT + file LICENSE"                            
#> raster             "GPL (>= 3)"                                    
#> rbibutils          "GPL-2"                                         
#> rcmdcheck          "MIT + file LICENSE"                            
#> readODS            "GPL-3"                                         
#> readr              "MIT + file LICENSE"                            
#> readxl             "MIT + file LICENSE"                            
#> reformulas         "GPL-3"                                         
#> rematch            "MIT + file LICENSE"                            
#> remind2            "LGPL-3"                                        
#> remulator          "LGPL-3 | file LICENSE"                         
#> renv               "MIT + file LICENSE"                            
#> reporttransport    "LGPL-3"                                        
#> reshape            "MIT + file LICENSE"                            
#> reshape2           "MIT + file LICENSE"                            
#> rex                "MIT + file LICENSE"                            
#> rlang              "MIT + file LICENSE"                            
#> rmarkdown          "GPL-3"                                         
#> rmndt              "GPL-3"                                         
#> rootSolve          "GPL (>= 2)"                                    
#> roxygen2           "MIT + file LICENSE"                            
#> rprojroot          "MIT + file LICENSE"                            
#> rstudioapi         "MIT + file LICENSE"                            
#> rversions          "MIT + file LICENSE"                            
#> rworldmap          "GPL (>= 2)"                                    
#> sass               "MIT + file LICENSE"                            
#> scales             "MIT + file LICENSE"                            
#> sessioninfo        "GPL-2"                                         
#> shiny              "MIT + file LICENSE"                            
#> sourcetools        "MIT + file LICENSE"                            
#> sp                 "GPL (>= 2)"                                    
#> spam               "LGPL-2 | BSD_3_clause + file LICENSE"          
#> stringi            "file LICENSE"                                  
#> stringr            "MIT + file LICENSE"                            
#> svglite            "GPL (>= 2)"                                    
#> sys                "MIT + file LICENSE"                            
#> systemfonts        "MIT + file LICENSE"                            
#> terra              "GPL (>= 3)"                                    
#> testthat           "MIT + file LICENSE"                            
#> textshaping        "MIT + file LICENSE"                            
#> tibble             "MIT + file LICENSE"                            
#> tidyr              "MIT + file LICENSE"                            
#> tidyselect         "MIT + file LICENSE"                            
#> tidytemplate       "MIT + file LICENSE"                            
#> timeDate           "GPL (>= 2)"                                    
#> timechange         "GPL (>= 3)"                                    
#> tinytex            "MIT + file LICENSE"                            
#> trafficlight       "LGPL-3"                                        
#> tzdb               "MIT + file LICENSE"                            
#> urca               "GPL (>= 2)"                                    
#> urlchecker         "GPL-3"                                         
#> usethis            "MIT + file LICENSE"                            
#> utf8               "Apache License (== 2.0) | file LICENSE"        
#> vctrs              "MIT + file LICENSE"                            
#> viridisLite        "MIT + file LICENSE"                            
#> vroom              "MIT + file LICENSE"                            
#> waldo              "MIT + file LICENSE"                            
#> whisker            "GPL-3"                                         
#> whoami             "MIT + file LICENSE"                            
#> withr              "MIT + file LICENSE"                            
#> writexl            "BSD_2_clause + file LICENSE"                   
#> xfun               "MIT + file LICENSE"                            
#> xml2               "MIT + file LICENSE"                            
#> xmlparsedata       "MIT + file LICENSE"                            
#> xopen              "MIT + file LICENSE"                            
#> xtable             "GPL (>= 2)"                                    
#> yaml               "BSD_3_clause + file LICENSE"                   
#> ymlthis            "MIT + file LICENSE"                            
#> zip                "MIT + file LICENSE"                            
#> zoo                "GPL-2 | GPL-3"                                 
#> KernSmooth         "Unlimited"                                     
#> MASS               "GPL-2 | GPL-3"                                 
#> Matrix             "GPL (>= 2) | file LICENCE"                     
#> base               "Part of R 4.5.3"                               
#> boot               "Unlimited"                                     
#> class              "GPL-2 | GPL-3"                                 
#> cluster            "GPL (>= 2)"                                    
#> codetools          "GPL"                                           
#> compiler           "Part of R 4.5.3"                               
#> datasets           "Part of R 4.5.3"                               
#> foreign            "GPL (>= 2)"                                    
#> grDevices          "Part of R 4.5.3"                               
#> graphics           "Part of R 4.5.3"                               
#> grid               "Part of R 4.5.3"                               
#> lattice            "GPL (>= 2)"                                    
#> methods            "Part of R 4.5.3"                               
#> mgcv               "GPL (>= 2)"                                    
#> nlme               "GPL (>= 2)"                                    
#> nnet               "GPL-2 | GPL-3"                                 
#> parallel           "Part of R 4.5.3"                               
#> rpart              "GPL-2 | GPL-3"                                 
#> spatial            "GPL-2 | GPL-3"                                 
#> splines            "Part of R 4.5.3"                               
#> stats              "Part of R 4.5.3"                               
#> stats4             "Part of R 4.5.3"                               
#> survival           "LGPL (>= 2)"                                   
#> tcltk              "Part of R 4.5.3"                               
#> tools              "Part of R 4.5.3"                               
#> utils              "Part of R 4.5.3"                               
#>                    License_is_FOSS License_restricts_use OS_type
#> Deriv              NA              NA                    NA     
#> Formula            NA              NA                    NA     
#> GDPuc              NA              NA                    NA     
#> Hmisc              NA              NA                    NA     
#> Lmoments           NA              NA                    NA     
#> MatrixModels       NA              NA                    NA     
#> R.methodsS3        NA              NA                    NA     
#> R.oo               NA              NA                    NA     
#> R.utils            NA              NA                    NA     
#> R6                 NA              NA                    NA     
#> RColorBrewer       NA              NA                    NA     
#> Rcpp               NA              NA                    NA     
#> Rdpack             NA              NA                    NA     
#> S7                 NA              NA                    NA     
#> SPEI               NA              NA                    NA     
#> SparseM            NA              NA                    NA     
#> TLMoments          NA              NA                    NA     
#> abind              NA              NA                    NA     
#> askpass            NA              NA                    NA     
#> assertr            NA              NA                    NA     
#> backports          NA              NA                    NA     
#> base64enc          NA              NA                    NA     
#> bit                NA              NA                    NA     
#> bit64              NA              NA                    NA     
#> brew               NA              NA                    NA     
#> brio               NA              NA                    NA     
#> broom              NA              NA                    NA     
#> bslib              NA              NA                    NA     
#> cachem             NA              NA                    NA     
#> callr              NA              NA                    NA     
#> car                NA              NA                    NA     
#> carData            NA              NA                    NA     
#> cellranger         NA              NA                    NA     
#> checkmate          NA              NA                    NA     
#> citation           NA              NA                    NA     
#> cli                NA              NA                    NA     
#> clipr              NA              NA                    NA     
#> collections        NA              NA                    NA     
#> colorspace         NA              NA                    NA     
#> commonmark         NA              NA                    NA     
#> contfrac           NA              NA                    NA     
#> countrycode        NA              NA                    NA     
#> covr               NA              NA                    NA     
#> cowplot            NA              NA                    NA     
#> crayon             NA              NA                    NA     
#> credentials        NA              NA                    NA     
#> crosstalk          NA              NA                    NA     
#> curl               NA              NA                    NA     
#> data.table         NA              NA                    NA     
#> deSolve            NA              NA                    NA     
#> desc               NA              NA                    NA     
#> devtools           NA              NA                    NA     
#> diffobj            NA              NA                    NA     
#> digest             NA              NA                    NA     
#> doBy               NA              NA                    NA     
#> doParallel         NA              NA                    NA     
#> docopt             NA              NA                    NA     
#> dotCall64          NA              NA                    NA     
#> downlit            NA              NA                    NA     
#> dplyr              NA              NA                    NA     
#> edgeTransport      NA              NA                    NA     
#> ellipsis           NA              NA                    NA     
#> elliptic           NA              NA                    NA     
#> evaluate           NA              NA                    NA     
#> fansi              NA              NA                    NA     
#> farver             NA              NA                    NA     
#> fastmap            NA              NA                    NA     
#> fields             NA              NA                    NA     
#> filelock           NA              NA                    NA     
#> fontawesome        NA              NA                    NA     
#> forcats            NA              NA                    NA     
#> foreach            NA              NA                    NA     
#> forecast           NA              NA                    NA     
#> fracdiff           NA              NA                    NA     
#> fs                 NA              NA                    NA     
#> gamstransfer       NA              NA                    NA     
#> gdx                NA              NA                    NA     
#> gdx2               NA              NA                    NA     
#> gdxdt              NA              NA                    NA     
#> gdxrrw             NA              NA                    NA     
#> generics           NA              NA                    NA     
#> geometry           NA              NA                    NA     
#> gert               NA              NA                    NA     
#> ggplot2            NA              NA                    NA     
#> gh                 NA              NA                    NA     
#> gitcreds           NA              NA                    NA     
#> glue               NA              NA                    NA     
#> gms                NA              NA                    NA     
#> goftest            NA              NA                    NA     
#> gridExtra          NA              NA                    NA     
#> gtable             NA              NA                    NA     
#> hdf5r              NA              NA                    NA     
#> highr              NA              NA                    NA     
#> hms                NA              NA                    NA     
#> htmlTable          NA              NA                    NA     
#> htmltools          NA              NA                    NA     
#> htmlwidgets        NA              NA                    NA     
#> httpuv             NA              NA                    NA     
#> httr               NA              NA                    NA     
#> httr2              NA              NA                    NA     
#> hypergeo           NA              NA                    NA     
#> igraph             NA              NA                    NA     
#> ini                NA              NA                    NA     
#> isoband            NA              NA                    NA     
#> iterators          NA              NA                    NA     
#> jquerylib          NA              NA                    NA     
#> jsonlite           NA              NA                    NA     
#> kableExtra         NA              NA                    NA     
#> knitr              NA              NA                    NA     
#> labeling           NA              NA                    NA     
#> later              NA              NA                    NA     
#> lazyeval           NA              NA                    NA     
#> lifecycle          NA              NA                    NA     
#> linprog            NA              NA                    NA     
#> lintr              NA              NA                    NA     
#> littler            NA              NA                    "unix" 
#> lme4               NA              NA                    NA     
#> lmom               NA              NA                    NA     
#> lmomco             NA              NA                    NA     
#> lmtest             NA              NA                    NA     
#> lpSolve            NA              NA                    NA     
#> lpjclass           NA              NA                    NA     
#> lpjmlkit           NA              NA                    NA     
#> lubridate          NA              NA                    NA     
#> lucode2            NA              NA                    NA     
#> luplot             NA              NA                    NA     
#> luscale            NA              NA                    NA     
#> lusweave           NA              NA                    NA     
#> madrat             NA              NA                    NA     
#> magclass           NA              NA                    NA     
#> magic              NA              NA                    NA     
#> magpie4            NA              NA                    NA     
#> magpiesets         NA              NA                    NA     
#> magrittr           NA              NA                    NA     
#> maps               NA              NA                    NA     
#> memoise            NA              NA                    NA     
#> microbenchmark     NA              NA                    NA     
#> mime               NA              NA                    NA     
#> miniUI             NA              NA                    NA     
#> minqa              NA              NA                    NA     
#> minty              NA              NA                    NA     
#> mip                NA              NA                    NA     
#> modelr             NA              NA                    NA     
#> mrcommons          NA              NA                    NA     
#> mrdownscale        NA              NA                    NA     
#> mrdrivers          NA              NA                    NA     
#> mrfaocore          NA              NA                    NA     
#> mrindustry         NA              NA                    NA     
#> mrlandcore         NA              NA                    NA     
#> mrremind           NA              NA                    NA     
#> mrtransport        NA              NA                    NA     
#> mstools            NA              NA                    NA     
#> ncdf4              NA              NA                    NA     
#> nleqslv            NA              NA                    NA     
#> nloptr             NA              NA                    NA     
#> nnls               NA              NA                    NA     
#> nonparaeff         NA              NA                    NA     
#> numDeriv           NA              NA                    NA     
#> openssl            NA              NA                    NA     
#> openxlsx           NA              NA                    NA     
#> otel               NA              NA                    NA     
#> pak                NA              NA                    NA     
#> pbkrtest           NA              NA                    NA     
#> piamInterfaces     NA              NA                    NA     
#> piamPlotComparison NA              NA                    NA     
#> piamutils          NA              NA                    NA     
#> pillar             NA              NA                    NA     
#> pkgbuild           NA              NA                    NA     
#> pkgconfig          NA              NA                    NA     
#> pkgdown            NA              NA                    NA     
#> pkgload            NA              NA                    NA     
#> plotly             NA              NA                    NA     
#> plyr               NA              NA                    NA     
#> praise             NA              NA                    NA     
#> prettyunits        NA              NA                    NA     
#> processx           NA              NA                    NA     
#> profvis            NA              NA                    NA     
#> promises           NA              NA                    NA     
#> ps                 NA              NA                    NA     
#> purrr              NA              NA                    NA     
#> qualV              NA              NA                    NA     
#> quantreg           NA              NA                    NA     
#> quitte             NA              NA                    NA     
#> ragg               NA              NA                    NA     
#> rappdirs           NA              NA                    NA     
#> raster             NA              NA                    NA     
#> rbibutils          NA              NA                    NA     
#> rcmdcheck          NA              NA                    NA     
#> readODS            NA              NA                    NA     
#> readr              NA              NA                    NA     
#> readxl             NA              NA                    NA     
#> reformulas         NA              NA                    NA     
#> rematch            NA              NA                    NA     
#> remind2            NA              NA                    NA     
#> remulator          NA              NA                    NA     
#> renv               NA              NA                    NA     
#> reporttransport    NA              NA                    NA     
#> reshape            NA              NA                    NA     
#> reshape2           NA              NA                    NA     
#> rex                NA              NA                    NA     
#> rlang              NA              NA                    NA     
#> rmarkdown          NA              NA                    NA     
#> rmndt              NA              NA                    NA     
#> rootSolve          NA              NA                    NA     
#> roxygen2           NA              NA                    NA     
#> rprojroot          NA              NA                    NA     
#> rstudioapi         NA              NA                    NA     
#> rversions          NA              NA                    NA     
#> rworldmap          NA              NA                    NA     
#> sass               NA              NA                    NA     
#> scales             NA              NA                    NA     
#> sessioninfo        NA              NA                    NA     
#> shiny              NA              NA                    NA     
#> sourcetools        NA              NA                    NA     
#> sp                 NA              NA                    NA     
#> spam               NA              NA                    NA     
#> stringi            "yes"           NA                    NA     
#> stringr            NA              NA                    NA     
#> svglite            NA              NA                    NA     
#> sys                NA              NA                    NA     
#> systemfonts        NA              NA                    NA     
#> terra              NA              NA                    NA     
#> testthat           NA              NA                    NA     
#> textshaping        NA              NA                    NA     
#> tibble             NA              NA                    NA     
#> tidyr              NA              NA                    NA     
#> tidyselect         NA              NA                    NA     
#> tidytemplate       NA              NA                    NA     
#> timeDate           NA              NA                    NA     
#> timechange         NA              NA                    NA     
#> tinytex            NA              NA                    NA     
#> trafficlight       NA              NA                    NA     
#> tzdb               NA              NA                    NA     
#> urca               NA              NA                    NA     
#> urlchecker         NA              NA                    NA     
#> usethis            NA              NA                    NA     
#> utf8               NA              NA                    NA     
#> vctrs              NA              NA                    NA     
#> viridisLite        NA              NA                    NA     
#> vroom              NA              NA                    NA     
#> waldo              NA              NA                    NA     
#> whisker            NA              NA                    NA     
#> whoami             NA              NA                    NA     
#> withr              NA              NA                    NA     
#> writexl            NA              NA                    NA     
#> xfun               NA              NA                    NA     
#> xml2               NA              NA                    NA     
#> xmlparsedata       NA              NA                    NA     
#> xopen              NA              NA                    NA     
#> xtable             NA              NA                    NA     
#> yaml               NA              NA                    NA     
#> ymlthis            NA              NA                    NA     
#> zip                NA              NA                    NA     
#> zoo                NA              NA                    NA     
#> KernSmooth         NA              NA                    NA     
#> MASS               NA              NA                    NA     
#> Matrix             NA              NA                    NA     
#> base               NA              NA                    NA     
#> boot               NA              NA                    NA     
#> class              NA              NA                    NA     
#> cluster            NA              NA                    NA     
#> codetools          NA              NA                    NA     
#> compiler           NA              NA                    NA     
#> datasets           NA              NA                    NA     
#> foreign            NA              NA                    NA     
#> grDevices          NA              NA                    NA     
#> graphics           NA              NA                    NA     
#> grid               NA              NA                    NA     
#> lattice            NA              NA                    NA     
#> methods            NA              NA                    NA     
#> mgcv               NA              NA                    NA     
#> nlme               NA              NA                    NA     
#> nnet               NA              NA                    NA     
#> parallel           NA              NA                    NA     
#> rpart              NA              NA                    NA     
#> spatial            NA              NA                    NA     
#> splines            NA              NA                    NA     
#> stats              NA              NA                    NA     
#> stats4             NA              NA                    NA     
#> survival           NA              NA                    NA     
#> tcltk              NA              NA                    NA     
#> tools              NA              NA                    NA     
#> utils              NA              NA                    NA     
#>                    MD5sum NeedsCompilation Built  
#> Deriv              NA     "no"             "4.5.0"
#> Formula            NA     "no"             "4.5.0"
#> GDPuc              NA     "no"             "4.5.0"
#> Hmisc              NA     "yes"            "4.5.0"
#> Lmoments           NA     "yes"            "4.5.0"
#> MatrixModels       NA     "no"             "4.5.0"
#> R.methodsS3        NA     "no"             "4.5.0"
#> R.oo               NA     "no"             "4.5.0"
#> R.utils            NA     "no"             "4.5.0"
#> R6                 NA     "no"             "4.5.0"
#> RColorBrewer       NA     "no"             "4.5.0"
#> Rcpp               NA     "yes"            "4.5.0"
#> Rdpack             NA     "no"             "4.5.0"
#> S7                 NA     "yes"            "4.5.0"
#> SPEI               NA     "no"             "4.5.0"
#> SparseM            NA     "yes"            "4.5.0"
#> TLMoments          NA     "yes"            "4.5.0"
#> abind              NA     "no"             "4.5.0"
#> askpass            NA     "yes"            "4.5.0"
#> assertr            NA     "no"             "4.5.0"
#> backports          NA     "yes"            "4.5.0"
#> base64enc          NA     "yes"            "4.5.0"
#> bit                NA     "yes"            "4.5.0"
#> bit64              NA     "yes"            "4.5.0"
#> brew               NA     "no"             "4.5.0"
#> brio               NA     "yes"            "4.5.0"
#> broom              NA     "no"             "4.5.0"
#> bslib              NA     "no"             "4.5.0"
#> cachem             NA     "yes"            "4.5.0"
#> callr              NA     "no"             "4.5.0"
#> car                NA     "no"             "4.5.0"
#> carData            NA     "no"             "4.5.0"
#> cellranger         NA     "no"             "4.5.0"
#> checkmate          NA     "yes"            "4.5.0"
#> citation           NA     "no"             "4.5.0"
#> cli                NA     "yes"            "4.5.0"
#> clipr              NA     "no"             "4.5.0"
#> collections        NA     "yes"            "4.5.0"
#> colorspace         NA     "yes"            "4.5.0"
#> commonmark         NA     "yes"            "4.5.0"
#> contfrac           NA     "yes"            "4.5.0"
#> countrycode        NA     "no"             "4.5.0"
#> covr               NA     "yes"            "4.5.0"
#> cowplot            NA     "no"             "4.5.0"
#> crayon             NA     "no"             "4.5.0"
#> credentials        NA     "no"             "4.5.0"
#> crosstalk          NA     "no"             "4.5.0"
#> curl               NA     "yes"            "4.5.0"
#> data.table         NA     "yes"            "4.5.0"
#> deSolve            NA     "yes"            "4.5.0"
#> desc               NA     "no"             "4.5.0"
#> devtools           NA     "no"             "4.5.0"
#> diffobj            NA     "yes"            "4.5.0"
#> digest             NA     "yes"            "4.5.0"
#> doBy               NA     "no"             "4.5.0"
#> doParallel         NA     "no"             "4.5.0"
#> docopt             NA     "no"             "4.5.3"
#> dotCall64          NA     "yes"            "4.5.0"
#> downlit            NA     "no"             "4.5.0"
#> dplyr              NA     "yes"            "4.5.0"
#> edgeTransport      NA     "no"             "4.5.3"
#> ellipsis           NA     "no"             "4.5.0"
#> elliptic           NA     "no"             "4.5.0"
#> evaluate           NA     "no"             "4.5.0"
#> fansi              NA     "yes"            "4.5.0"
#> farver             NA     "yes"            "4.5.0"
#> fastmap            NA     "yes"            "4.5.0"
#> fields             NA     "yes"            "4.5.0"
#> filelock           NA     "yes"            "4.5.0"
#> fontawesome        NA     "no"             "4.5.0"
#> forcats            NA     "no"             "4.5.0"
#> foreach            NA     "no"             "4.5.0"
#> forecast           NA     "yes"            "4.5.0"
#> fracdiff           NA     "yes"            "4.5.0"
#> fs                 NA     "yes"            "4.5.0"
#> gamstransfer       NA     "yes"            "4.5.0"
#> gdx                NA     "no"             "4.5.3"
#> gdx2               NA     "no"             "4.5.3"
#> gdxdt              NA     "no"             "4.5.0"
#> gdxrrw             NA     "yes"            "4.5.3"
#> generics           NA     "no"             "4.5.0"
#> geometry           NA     "yes"            "4.5.0"
#> gert               NA     "yes"            "4.5.0"
#> ggplot2            NA     "no"             "4.5.0"
#> gh                 NA     "no"             "4.5.0"
#> gitcreds           NA     "no"             "4.5.0"
#> glue               NA     "yes"            "4.5.0"
#> gms                NA     "no"             "4.5.0"
#> goftest            NA     "yes"            "4.5.0"
#> gridExtra          NA     "no"             "4.5.0"
#> gtable             NA     "no"             "4.5.0"
#> hdf5r              NA     "yes"            "4.5.0"
#> highr              NA     "no"             "4.5.0"
#> hms                NA     "no"             "4.5.0"
#> htmlTable          NA     "no"             "4.5.0"
#> htmltools          NA     "yes"            "4.5.0"
#> htmlwidgets        NA     "no"             "4.5.0"
#> httpuv             NA     "yes"            "4.5.0"
#> httr               NA     "no"             "4.5.0"
#> httr2              NA     "no"             "4.5.0"
#> hypergeo           NA     "no"             "4.5.0"
#> igraph             NA     "yes"            "4.5.0"
#> ini                NA     "no"             "4.5.0"
#> isoband            NA     "yes"            "4.5.0"
#> iterators          NA     "no"             "4.5.0"
#> jquerylib          NA     "no"             "4.5.0"
#> jsonlite           NA     "yes"            "4.5.0"
#> kableExtra         NA     "no"             "4.5.0"
#> knitr              NA     "no"             "4.5.0"
#> labeling           NA     "no"             "4.5.0"
#> later              NA     "yes"            "4.5.0"
#> lazyeval           NA     "yes"            "4.5.0"
#> lifecycle          NA     "no"             "4.5.0"
#> linprog            NA     "no"             "4.5.0"
#> lintr              NA     "no"             "4.5.0"
#> littler            NA     "yes"            "4.5.3"
#> lme4               NA     "yes"            "4.5.0"
#> lmom               NA     "yes"            "4.5.0"
#> lmomco             NA     "no"             "4.5.0"
#> lmtest             NA     "yes"            "4.5.0"
#> lpSolve            NA     "yes"            "4.5.0"
#> lpjclass           NA     "no"             "4.5.3"
#> lpjmlkit           NA     "no"             "4.5.3"
#> lubridate          NA     "yes"            "4.5.0"
#> lucode2            NA     "no"             "4.5.3"
#> luplot             NA     "no"             "4.5.3"
#> luscale            NA     "no"             "4.5.3"
#> lusweave           NA     "no"             "4.5.3"
#> madrat             NA     "no"             "4.5.3"
#> magclass           NA     "no"             "4.5.0"
#> magic              NA     "no"             "4.5.0"
#> magpie4            NA     "no"             "4.5.3"
#> magpiesets         NA     "no"             "4.5.3"
#> magrittr           NA     "yes"            "4.5.0"
#> maps               NA     "yes"            "4.5.0"
#> memoise            NA     "no"             "4.5.0"
#> microbenchmark     NA     "yes"            "4.5.0"
#> mime               NA     "yes"            "4.5.0"
#> miniUI             NA     "no"             "4.5.0"
#> minqa              NA     "yes"            "4.5.0"
#> minty              NA     "yes"            "4.5.0"
#> mip                NA     "no"             "4.5.3"
#> modelr             NA     "no"             "4.5.0"
#> mrcommons          NA     "no"             "4.5.3"
#> mrdownscale        NA     "no"             "4.5.3"
#> mrdrivers          NA     "no"             "4.5.3"
#> mrfaocore          NA     "no"             "4.5.3"
#> mrindustry         NA     "no"             "4.5.3"
#> mrlandcore         NA     "no"             "4.5.3"
#> mrremind           NA     "no"             "4.5.3"
#> mrtransport        NA     "no"             "4.5.3"
#> mstools            NA     "no"             "4.5.3"
#> ncdf4              NA     "yes"            "4.5.0"
#> nleqslv            NA     "yes"            "4.5.0"
#> nloptr             NA     "yes"            "4.5.0"
#> nnls               NA     "yes"            "4.5.0"
#> nonparaeff         NA     "no"             "4.5.0"
#> numDeriv           NA     "no"             "4.5.0"
#> openssl            NA     "yes"            "4.5.0"
#> openxlsx           NA     "yes"            "4.5.0"
#> otel               NA     "no"             "4.5.0"
#> pak                NA     "yes"            "4.5.0"
#> pbkrtest           NA     "no"             "4.5.0"
#> piamInterfaces     NA     "no"             "4.5.3"
#> piamPlotComparison NA     "no"             "4.5.3"
#> piamutils          NA     "no"             "4.5.3"
#> pillar             NA     "no"             "4.5.0"
#> pkgbuild           NA     "no"             "4.5.0"
#> pkgconfig          NA     "no"             "4.5.0"
#> pkgdown            NA     "no"             "4.5.0"
#> pkgload            NA     "no"             "4.5.0"
#> plotly             NA     "no"             "4.5.0"
#> plyr               NA     "yes"            "4.5.0"
#> praise             NA     "no"             "4.5.0"
#> prettyunits        NA     "no"             "4.5.0"
#> processx           NA     "yes"            "4.5.0"
#> profvis            NA     "yes"            "4.5.0"
#> promises           NA     "no"             "4.5.0"
#> ps                 NA     "yes"            "4.5.0"
#> purrr              NA     "yes"            "4.5.0"
#> qualV              NA     "yes"            "4.5.0"
#> quantreg           NA     "yes"            "4.5.0"
#> quitte             NA     "no"             "4.5.3"
#> ragg               NA     "yes"            "4.5.0"
#> rappdirs           NA     "yes"            "4.5.0"
#> raster             NA     "yes"            "4.5.0"
#> rbibutils          NA     "yes"            "4.5.0"
#> rcmdcheck          NA     "no"             "4.5.0"
#> readODS            NA     "yes"            "4.5.0"
#> readr              NA     "yes"            "4.5.0"
#> readxl             NA     "yes"            "4.5.0"
#> reformulas         NA     "no"             "4.5.0"
#> rematch            NA     "no"             "4.5.0"
#> remind2            NA     "no"             "4.5.3"
#> remulator          NA     "no"             "4.5.3"
#> renv               NA     "no"             "4.5.0"
#> reporttransport    NA     "no"             "4.5.3"
#> reshape            NA     "no"             "4.5.0"
#> reshape2           NA     "yes"            "4.5.0"
#> rex                NA     "no"             "4.5.0"
#> rlang              NA     "yes"            "4.5.0"
#> rmarkdown          NA     "no"             "4.5.0"
#> rmndt              NA     "no"             "4.5.3"
#> rootSolve          NA     "yes"            "4.5.0"
#> roxygen2           NA     "yes"            "4.5.0"
#> rprojroot          NA     "no"             "4.5.0"
#> rstudioapi         NA     "no"             "4.5.0"
#> rversions          NA     "no"             "4.5.0"
#> rworldmap          NA     "no"             "4.5.0"
#> sass               NA     "yes"            "4.5.0"
#> scales             NA     "no"             "4.5.0"
#> sessioninfo        NA     "no"             "4.5.0"
#> shiny              NA     "no"             "4.5.0"
#> sourcetools        NA     "yes"            "4.5.0"
#> sp                 NA     "yes"            "4.5.0"
#> spam               NA     "yes"            "4.5.0"
#> stringi            NA     "yes"            "4.5.0"
#> stringr            NA     "no"             "4.5.0"
#> svglite            NA     "yes"            "4.5.0"
#> sys                NA     "yes"            "4.5.0"
#> systemfonts        NA     "yes"            "4.5.0"
#> terra              NA     "yes"            "4.5.0"
#> testthat           NA     "yes"            "4.5.0"
#> textshaping        NA     "yes"            "4.5.0"
#> tibble             NA     "yes"            "4.5.0"
#> tidyr              NA     "yes"            "4.5.0"
#> tidyselect         NA     "yes"            "4.5.0"
#> tidytemplate       NA     "no"             "4.5.3"
#> timeDate           NA     "no"             "4.5.0"
#> timechange         NA     "yes"            "4.5.0"
#> tinytex            NA     "no"             "4.5.0"
#> trafficlight       NA     "no"             "4.5.3"
#> tzdb               NA     "yes"            "4.5.0"
#> urca               NA     "yes"            "4.5.0"
#> urlchecker         NA     "no"             "4.5.0"
#> usethis            NA     "no"             "4.5.0"
#> utf8               NA     "yes"            "4.5.0"
#> vctrs              NA     "yes"            "4.5.0"
#> viridisLite        NA     "no"             "4.5.0"
#> vroom              NA     "yes"            "4.5.0"
#> waldo              NA     "no"             "4.5.0"
#> whisker            NA     "no"             "4.5.0"
#> whoami             NA     "no"             "4.5.0"
#> withr              NA     "no"             "4.5.0"
#> writexl            NA     "yes"            "4.5.0"
#> xfun               NA     "yes"            "4.5.0"
#> xml2               NA     "yes"            "4.5.0"
#> xmlparsedata       NA     "no"             "4.5.0"
#> xopen              NA     "no"             "4.5.0"
#> xtable             NA     "no"             "4.5.0"
#> yaml               NA     "yes"            "4.5.0"
#> ymlthis            NA     "no"             "4.5.0"
#> zip                NA     "yes"            "4.5.0"
#> zoo                NA     "yes"            "4.5.0"
#> KernSmooth         NA     "yes"            "4.5.3"
#> MASS               NA     "yes"            "4.5.3"
#> Matrix             NA     "yes"            "4.5.3"
#> base               NA     NA               "4.5.3"
#> boot               NA     "no"             "4.5.3"
#> class              NA     "yes"            "4.5.3"
#> cluster            NA     "yes"            "4.5.3"
#> codetools          NA     "no"             "4.5.3"
#> compiler           NA     NA               "4.5.3"
#> datasets           NA     NA               "4.5.3"
#> foreign            NA     "yes"            "4.5.3"
#> grDevices          NA     "yes"            "4.5.3"
#> graphics           NA     "yes"            "4.5.3"
#> grid               NA     "yes"            "4.5.3"
#> lattice            NA     "yes"            "4.5.3"
#> methods            NA     "yes"            "4.5.3"
#> mgcv               NA     "yes"            "4.5.3"
#> nlme               NA     "yes"            "4.5.3"
#> nnet               NA     "yes"            "4.5.3"
#> parallel           NA     "yes"            "4.5.3"
#> rpart              NA     "yes"            "4.5.3"
#> spatial            NA     "yes"            "4.5.3"
#> splines            NA     "yes"            "4.5.3"
#> stats              NA     "yes"            "4.5.3"
#> stats4             NA     NA               "4.5.3"
#> survival           NA     "yes"            "4.5.3"
#> tcltk              NA     "yes"            "4.5.3"
#> tools              NA     "yes"            "4.5.3"
#> utils              NA     "yes"            "4.5.3"
#> 
```
