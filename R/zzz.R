.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Most of the data is BC Hydro property. \nDistribution is strictly prohibited."
  )
  invisible()
}
