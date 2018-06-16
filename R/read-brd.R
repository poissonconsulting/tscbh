#' Read Brilliant Dam Data File
#'
#' A utc_offset of -8 is equivalent to Pacific Standard Time.
#'
#' @param file A string specifying the path to the file.
#' @inheritParams tsdbr::ts_create_db
#' @export
ts_read_brd <- function(file = "brd.csv", utc_offset = -8L) {
  check_string(file)
  check_scalar(utc_offset, c(-12L, 14L))

  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  data <- utils::read.csv(file, stringsAsFactors = FALSE)
  
  check_data(data, values = list(
    TIME = "",
    BRD_FLOWS_AVG = 1,
    BRD_QSPILL_AVG = 1,
    BRX_FLOW_AVG = 1
  ), x_name = file)
  
  data <- data[c("TIME", "BRD_FLOWS_AVG", "BRD_QSPILL_AVG", "BRX_FLOW_AVG")]
  
  data <- tidyr::gather(data, "Station", "Observed",  
                        -tidyselect::matches("TIME"))

  data$DateTime <- lubridate::parse_date_time(
    data$TIME, c("YmdHM", "YmdHMS", "dmYHM", "dmYHMS"), 
    tz = ts_utc_offset_to_tz(utc_offset))

  data$TIME <- NULL
  
  data$Observed <- data$Observed * 0.028316847
  
  data$Status <- ordered("reasonable", c("reasonable", "questionable", "erroneous"))

  data <- data[c("Station", "DateTime", "Observed", "Status")]

  data <- data[order(data$Station, data$DateTime), ]
  rownames(data) <- NULL
  data
}
