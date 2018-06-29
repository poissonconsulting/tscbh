format_triad <- function(triad) {
  check_vector(triad, "", length = 3, unique = TRUE)
  paste(triad[1], "=", triad[2], "+", triad[3])
}

add <- function(data, table, conn) {
  DBI::dbWriteTable(conn, table, data, append = TRUE)
  invisible(data)
}

is_even <- function(x) {
  x %% 2 == 0 
}

is_odd <- function(x) {
  !is_even(x)
}

in_commas <- function(x) {
  paste0("IN ('", paste0(x, collapse = "','"), "')")
}

punctuate <- function(x, qualifier = "or") {
  check_string(qualifier)
  if (is.logical(x) || is.integer(x) || is.numeric(x)) {
    x <- as.character(x)
  } else
    x <- paste0("'", as.character(x), "'")
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

plural <- function(x, n = 1L, end = "") {
  check_string(x)
  n <- check_count(n, coerce = TRUE)
  check_string(end)
  paste0(x, ifelse(n != 1L, "s", ""), end)
}

as_tibble <- function(data) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    data <- tibble::as_tibble(data)
  }
  data
}
