#' Add Data
#'
#' @inheritParams tsdbr::ts_add_data
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, resolution = "abort",
                        conn = getOption("tsdbr.conn", NULL)) {
  
  tsdbr::ts_add_data(data = data,
                     aggregate = TRUE, na_rm = TRUE, resolution = resolution,
                     conn = conn)
}

#' Get Data
#'
#' @param period A string of the period to aggregate and average by.
#' The possible values are 'year', 'month', 'day', 'hour'.
#' @inheritParams tsdbr::ts_get_data
#' @return A data frame of the requested data.
#' @export
ts_get_data <- function(stations = ts_get_stations()$Station,
                        start_date = end_date - 366L, 
                        end_date = Sys.Date(),
                        period = "hour",
                        na_rm = FALSE,
                        status = "reasonable",
                        conn = getOption("tsdbr.conn", NULL)) {
  
  check_vector(period, c("year", "month", "day", "hour", "second"), length = 1)

  tsdbr::ts_get_data(stations = stations,
                     end_date = end_date,
                     start_date = start_date,
                     period = period,
                     na_rm = na_rm,
                     status = status,
                     fill = FALSE,
                     na_replace = NA,
                     conn = conn)
}
