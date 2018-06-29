#' Read BC Hydro Data File
#' 
#' Reads in an Excel spreadsheet of BC Hydro data as provided by Power Records.
#' The station cannot be consistently determined from the file so must be
#' manually added.
#'
#' @param file A string specifying the path to the file.
#' @export
ts_read_bch <- function(file = "tscbh.xls") {
  check_string(file)

  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  meta <- readxl::read_excel(file, sheet = "FYI")
  if(!any(grepl("All flow information is in cms", meta[1])))
    stop("all flow information must be in cms", call. = FALSE)
  if(!any(grepl("Elevations [(]m[)]", meta[1])))
    stop("all elevations must be in m", call. = FALSE)
  
  data <- readxl::read_excel(file, sheet = "data", skip = 2)
  data <- data[vapply(data, function(x) !all(is.na(x)), TRUE)]
  if(nrow(data) < 3L) stop("data must have at least three columns with non-missing values")
  if(colnames(data)[1] != "Hour") stop("the first column in data with non-missing values must be 'Hour'", call. = FALSE)
  check_data(data, values = list(Hour = c(1,24)))
  if(!is_odd(ncol(data))) stop("the number of columns in data after 'Hour' must be even", call. = FALSE)
  class <- vapply(data[-1], function(x) class(x)[1], "")
  class <- (is_odd(1:length(class)) & class == "POSIXct") | (!is_odd(1:length(class)) & class == "numeric")
  if(!all(class)) stop("the columns after 'Hour' must alternate between 'POSIXct' and 'numeric' types", call. = FALSE)
  datas <- list()
  for(i in 1:((ncol(data) - 1)/2)) {
    datas[[i]] <- data[c(1, i * 2, i * 2 + 1)]
    colnames(datas[[i]]) <- c("Hour", "DateTime", "Recorded")
  }
  data <- do.call("rbind", datas)
  rm(datas)
  data$DateTime <- lubridate::force_tz(data$DateTime, "Etc/GMT+8")
  data$Hour <- as.integer(data$Hour) - 1L
  lubridate::hour(data$DateTime) <- data$Hour
  data$Hour <- NULL
  data <- data[!is.na(data$DateTime),]
  check_key(data, key = "DateTime")
  data <- data[order(data$DateTime),]
  diff <- diff(data$DateTime)
  diff <- unique(diff)
  if(!identical(diff, 1))
    stop("DateTimes must be consecutive hours", call. = FALSE)
  as_tibble(data)
}
