#' Add Station
#'
#' @inheritParams tsdbr::ts_add_stations
#' @export
#' @return The imported station data.
#' @export
ts_add_stations <- function(stations, file = getOption("tsdbr.file", "tscbh.sqlite")) {
  check_data(stations, values = list(Period = c("year", "month", "day", "hour")))

  tsdbr::ts_add_stations(stations, file = file)
}
