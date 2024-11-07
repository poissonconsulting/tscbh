#' Read BC Hydro Data File
#'
#' Reads in an Excel spreadsheet of BC Hydro data as provided by Power Records.
#' The station cannot be consistently determined from the file so must be
#' manually added.
#'
#' @param file A string specifying the path to the file.
#' @export
ts_read_bch <- function(file = "tscbh.xls") {
  chk_string(file)

  if (!file.exists(file)) {
    stop("file '", file, "' does not exist", call. = FALSE)
  }

  meta <- readxl::read_excel(file, sheet = "FYI")
  if (!any(grepl("All flow information is in cms", meta[1]))) {
    stop("all flow information must be in cms", call. = FALSE)
  }
  if (!any(grepl("Elevations [(]m[)]", meta[1]))) {
    stop("all elevations must be in m", call. = FALSE)
  }

  data <- readxl::read_excel(file, sheet = "data", skip = 2, guess_max = 10000)
  check_data(data, values = list(Hour = c(1, 24)))
  data <- data[which(colnames(data) == "Hour"):ncol(data)]
  class <- vapply(data, function(x) class(x)[1], "")
  posix <- which(class == "POSIXct")
  if (!length(posix)) stop("data must have at least column with Date values after 'Hour'", call. = FALSE)
  if (ncol(data) == posix[length(posix)]) {
    stop("data must have at least column after the last column with Date values", call. = FALSE)
  }
  datas <- list()
  for (i in seq_along(posix)) {
    datas[[i]] <- data[c(1, posix[i]:(posix[i] + 1))]
    colnames(datas[[i]]) <- c("Hour", "DateTime", "Recorded")
  }
  data <- do.call("rbind", datas)
  rm(datas)
  data$DateTime <- dtt_set_tz(data$DateTime, "Etc/GMT+8")
  data$Hour <- as.integer(data$Hour) - 1L
  dtt_hour(data$DateTime) <- data$Hour
  data$Hour <- NULL
  data <- data[!is.na(data$DateTime), ]
  check_key(data, key = "DateTime")
  data <- data[order(data$DateTime), ]
  diff <- diff(data$DateTime)
  diff <- unique(diff)
  if (!identical(diff, 1)) {
    stop("DateTimes must be consecutive hours", call. = FALSE)
  }
  as_tibble(data)
}
