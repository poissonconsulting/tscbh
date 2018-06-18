#' Create Time-Series Database
#'
#' @param file A string of the name of the database file.
#' @return A connection to the database.
#' @export
ts_create_db <- function (file) {
  conn <- tsdbr::ts_create_db(file = file,
                      periods = c("day", "hour"),
                      utc_offset = -8L)
  conn
}
