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