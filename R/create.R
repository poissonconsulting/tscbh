#' Create Time-Series Database
#'
#' Creates an empty SQLite database to store time series data.
#' The UTC offset for Alaska is -8.
#'
#' @param file A string of the name of the database file.
#' @param utc_offset A integer of the utc offset which must lie between -12 and 14.
#' @export
ts_create_db <- function (file = getOption("tsdbr.file", "ts.db"), 
                          utc_offset = 0L) {
  
  tsdbr::ts_create_db(file = file, utc_offset = utc_offset,
                      periods = c("day", "hour"))
}
