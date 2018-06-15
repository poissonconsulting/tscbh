.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Most of the data is BC Hydro property. \nDistribution is strictly prohibited.\nFor more information type: cat(ts_disclaimer())")
  packageStartupMessage("Setting option(tsdbr.file = 'tscbh.sqlite')")
  
  options(tsdbr.file = "tscbh.sqlite")
  invisible()
}
