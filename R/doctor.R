#' Doctor Database
#'
#' @inheritParams tsdbr::ts_doctor_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(check_limits = TRUE,
                         check_period = TRUE,
                         check_gaps = FALSE,
                         fix = FALSE, 
                         file = getOption("tsdbr.file", "ts.db")) {
  
  warning("need to check no minute or second periods")
  warning("need to check summations of time series")
  
  stations <- ts_get_stations(file = file)
  stations <- stations[stations$Period %in% c("minute", "second"),]
  if(nrow(stations)) {
    message("there are ", 
            length(unique(stations$Station)), " stations",
            " with periods less than an hour")
    if(fix) {
      conn <- ts_connect_db(file = file)
      
    }
  }
  
  tsdbr::ts_doctor_db(check_limits = check_limits, 
                      check_period = check_period, 
                      check_gaps = check_gaps,
                      fix = fix, 
                      file = file)
}
