#' piamPackages
#'
#' Fetches the names of packages available on https://pik-piam.r-universe.dev/ui#builds
#'
#' @return A character vector of names of packages available on https://pik-piam.r-universe.dev/ui#builds
#'
#' @export
piamPackages <- function() {
  packagesUrl <- "https://pik-piam.r-universe.dev/src/contrib/PACKAGES"
  fallback <- c("biospheremetrics", "blackmagicc", "boundaries", "brick", "citation",
                "edgeTransport", "edgeTrpLib", "GDPuc", "gdx", "gdx2",
                "gdxrrw", "ggtrace", "gms", "goxygen", "HARr", "iamc", "limes",
                "lpjclass", "lpjmlkit", "lpjmlstats", "lucode2", "luplot", "luscale",
                "lusweave", "m4fsdp", "madrat", "magclass", "magpie4", "MagpieNCGains",
                "magpiesets", "mip", "modelstats", "mrcommons", "mrdieter", "mrdrivers",
                "mredgebuildings", "mrfable", "mrfactors", "mrfaocore", "mrfeed",
                "mrfish", "mrindustry", "mrland", "mrlandcore", "mrmagpie", "mrorganic",
                "mrremind", "mrsoil", "mrtransport", "mrtutorial", "mrvalidation",
                "mrvalidnitrogen", "mrwaste", "mrwater", "mrwaterplots", "mstools",
                "piamenv", "piamInterfaces", "piamModelTests", "piamPlotComparison",
                "piamutils", "piamValidation", "piktests", "quitte", "r2databus",
                "regressionworlddata", "remind2", "remulator", "reportbrick",
                "reporttransport", "rmndt", "rOpenscmRunner", "shinyresults",
                "SPARQL", "trafficlight")
  return(tryCatch({
    sort(sub("^Package: ", "", grep("^Package: ", readLines(packagesUrl), value = TRUE)))
  }, warning = function(w) fallback, error = function(e) fallback))
}
