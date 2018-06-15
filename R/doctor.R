#' Doctor Database
#'
#' @inheritParams tsdbr::ts_doctor_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(check_limits = TRUE,
                         check_period = TRUE,
                         check_gaps = FALSE,
                         fix = FALSE, 
                         file = getOption("tsdbr.file", "tscbh.db")) {
  
  warning("need to check no minute or second periods")
  warning("need to check summations of time series")
  
  tsdbr::ts_doctor_db(check_limits = check_limits, 
                      check_period = check_period, 
                      check_gaps = check_gaps,
                      fix = fix, 
                      file = file)
}
