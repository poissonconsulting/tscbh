#' Create Time-Series Database
#'
#' @param file A string of the name of the database file.
#' @return A connection to the database.
#' @export
ts_create_db <- function(file) {
  conn <- tsdbr::ts_create_db(
    file = file,
    periods = c("day", "hour"),
    utc_offset = -8L
  )

  DBI::dbExecute(conn, "CREATE TABLE Triad (
    Triad INTEGER PRIMARY KEY,
    Child  TEXT NOT NULL UNIQUE,
    Parent1 TEXT NOT NULL UNIQUE,
    Parent2 TEXT NOT NULL UNIQUE,
    CHECK (
      Child != Parent1 AND
      Child != Parent2 AND
      Parent1 != Parent2
    ),
    FOREIGN KEY (Child) REFERENCES Station (Station) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (Parent1) REFERENCES Station (Station) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (Parent2) REFERENCES Station (Station) ON UPDATE CASCADE ON DELETE CASCADE
    );")
  conn
}
