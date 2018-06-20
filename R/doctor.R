doctor_triad <- function(triad, fix, conn) {
  triad <- c(triad$Child, triad$Parent1, triad$Parent2)
  
  span <- DBI::dbGetQuery(conn, paste0(
    "SELECT Station, MIN(DateTimeData) AS Start, MAX(DateTimeData) AS End
    FROM Data
    WHERE Station ", in_commas(triad),"
    GROUP BY Station"))
  
  span$Start <- as.POSIXct(span$Start, tz = "Etc/GMT+8")
  span$End <- as.POSIXct(span$End, tz = "Etc/GMT+8")
  
  span <- span[span$Station %in% triad[2:3],]
  if(!nrow(span)) {
    return(data.frame(Station = character(0),
                      inconsistent = numeric(0),
                      stringsAsFactors = FALSE))
  }
  start <- min(span$Start)
  end <- max(span$End)
  
  data <- ts_get_data(stations = triad, 
                      start_date = lubridate::date(start), 
                      end_date = lubridate::date(end) + 1,
                      period = "hour",
                      fill = TRUE,
                      status = "erroneous",
                      conn = conn)
  
  data <- data[c("Station", "DateTime", "Recorded", "Corrected", "Status")]
  
  data <- data[data$DateTime >= start & data$DateTime <= end,]
  
  data$Status <- ts_status_to_integer(data$Status)
  
  triad1 <- data[data$Station == triad[1],]
  triad2 <- data[data$Station == triad[2],]
  triad3 <- data[data$Station == triad[3],]
  rm(data)
  
  triad1 <- triad1[order(triad1$DateTime),]
  triad2 <- triad2[order(triad2$DateTime),]
  triad3 <- triad3[order(triad3$DateTime),]
  
  na0 <- !is.na(triad2$Corrected) & !is.na(triad3$Corrected)
  na2 <- is.na(triad2$Corrected) & is.na(triad3$Corrected)
  na1 <- !na0 & !na2
  
  inconsistent <- (na1 & !is.na(triad1$Corrected)) | 
    (na0 & (is.na(triad1$Corrected) | triad1$Corrected != triad2$Corrected + triad3$Corrected)) | 
    (na0 & triad1$Status < pmax(triad2$Status, triad3$Status))
  
  triad1$Corrected[na0] <- triad2$Corrected[na0] + triad3$Corrected[na0]
  triad1$Status[na0] <- pmax(triad2$Status[na0], triad3$Status[na0])
  is.na(triad1$Corrected[na1])<- TRUE
  
  triad1 <- triad1[inconsistent,]
  triad1$Status <- ts_integer_to_status(triad1$Status)
  
  if(!nrow(triad1)) {
    return(data.frame(Station = character(0),
                      inconsistent = numeric(0),
                      stringsAsFactors = FALSE))    
  } else if(fix) ts_add_data(data = triad1, resolution = "replace")
  data.frame(Station = triad[1],
             inconsistent = nrow(triad1),
             stringsAsFactors = FALSE)
}

#' Doctor Database
#'
#' @inheritParams tsdbr::ts_doctor_db
#' @param check_triads A flag indicating whether to check if triads are consistent.
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(check_limits = TRUE,
                         check_period = TRUE,
                         check_gaps = TRUE,
                         check_triads = TRUE,
                         fix = FALSE, 
                         conn = getOption("tsdbr.conn", NULL)) {
  
  flag <- tsdbr::ts_doctor_db(check_limits = check_limits, 
                              check_period = check_period,
                              check_gaps = check_gaps,
                              fix = fix,
                              conn = conn)
  
  triads <- FALSE
  if(check_triads) {
    triads <- ts_get_table("Triad", conn = conn)
    triads <- triads[order(triads$Triad),]
    
    triads <- split(triads, triads$Triad)
    triads <- lapply(triads, doctor_triad, fix = fix, conn = conn)
    triads <- do.call("rbind", triads)
    if(nrow(triads)) {
      message("the following stations ", ifelse(fix, "had", "have")," inconsistent triads:\n",
              paste0(utils::capture.output(triads), collapse = "\n"))
    }
    triads <- nrow(triads) > 0
  }
  flag && !triads
}
