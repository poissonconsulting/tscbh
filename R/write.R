#' Write csv
#'
#' The initial columns in the csv file are as follows
#' \describe{
#'   \item{Year}{The year of the value.}
#'   \item{Month}{The month of the value.}
#'   \item{Day}{The day of the value.}
#'   \item{Hour}{The hour of the value.}
#'   \item{Station}{The station}
#' }
#' The csv file should also include the following columns
#' \describe{
#'   \item{Recorded}{The value as provided by the station maintainer.}
#'   \item{Corrected}{The corrected value (see below).}
#'   \item{Status}{The reliability of the corrected value (see below).}
#'   \item{Parameter}{The parameter type, ie, Discharge, Elevation, Water Temperature etc.}
#'   \item{Units}{The units of the value.}
#'   \item{StationName}{The name of the station.}
#'   \item{Comments}{Comments on the value.}
#' }
#'
#' There are three possible status values
#' \describe{
#'   \item{Reasonable}{There is nothing obviously wrong with the value.}
#'   \item{Questionable}{The value may be wrong.}
#'   \item{Erroneous}{The value is definitely wrong.}
#' }
#' In general questionable values are those which are in the range of possible
#' values but are inconsistent with spatial or temporal neighbours
#' while erroneous value are those which are outside the range of possible values, ie negative discharge.
#'
#' If a value is erroneous, questionable or missing (indicated by NA)
#' then we will (time permitting) provide a corrected value based on neighbouring values.
#' Otherwise the corrected value is simply the recorded value.
#' The status column refers to the corrected value and
#' corrected values which differ from the recorded value are coded as Questionable.
#'
#' If data gaps are not important it is recommended that you only include Reasonable values.
#' Otherwise it is recommended that you only include Questionable values
#' that you consider to be reliable based on plotting and analysis.
#'
#' @param data The data frame with a minimum of columns Station and DateTime
#' @inheritParams utils::write.csv
#' @return An invisible copy of saved data.
#' @export
ts_write_csv <- function(data, file = "tsdbr.csv") {
  check_data(data,
    values = list(
      Station = "",
      DateTime = Sys.time()
    ),
    nrow = TRUE,
    key = c("Station", "DateTime")
  )
  chk_string(file)

  data$Depth <- NULL
  data$Site <- NULL

  colnames <- colnames(data)
  data$Year <- format(data$DateTime, "%Y")
  data$Month <- format(data$DateTime, "%m")
  data$Day <- format(data$DateTime, "%d")
  data$Hour <- format(data$DateTime, "%H")

  colnames <- setdiff(colnames, "DateTime")

  data <- data[c("Year", "Month", "Day", "Hour", colnames)]
  utils::write.csv(data, file = file, row.names = FALSE)
  invisible(as_tibble(data))
}
