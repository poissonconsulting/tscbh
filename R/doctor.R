doctor_triad <- function(triad, conn) {
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
    message("no parent data for triad ", format_triad(triad), call. = FALSE)
    return(TRUE)
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
  
  triad1 <- data[data$Station == triad[1],]
  triad2 <- data[data$Station == triad[2],]
  triad3 <- data[data$Station == triad[3],]
  rm(data)
  
  triad1 <- triad1[order(triad1$DateTime),]
  triad2 <- triad2[order(triad2$DateTime),]
  triad3 <- triad3[order(triad3$DateTime),]
  
  na2 <- is.na(triad2$Corrected) & is.na(triad3$Corrected)
  na0 <- !is.na(triad2$Corrected) & !is.na(triad3$Corrected)
  na1 <- !na0 & !na2
  
  triad1$Corrected[na0] <- triad2$Corrected[na0] + triad3$Corrected[na0]
  triad1$Status[na0] <- pmax(triad1$Status[na0], triad2$Status[na0], triad3$Status[na0])
  
  is.na(triad1$Corrected[na1])<- TRUE  
  triad1$Status[na1] <- "reasonable"
  
  ts_add_data(data = triad1, resolution = "replace")
  return(TRUE)
}

#' Doctor Database
#'
#' @inheritParams tsdbr::ts_doctor_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(conn = getOption("tsdbr.conn", NULL)) {
  
  flag <- tsdbr::ts_doctor_db(check_limits = TRUE, 
                      check_period = TRUE,
                      check_gaps = TRUE,
                      fix = TRUE,
                      conn = conn)
  
  if(!flag) {
    warning("fixes failed (triads unchecked)!")
    return(FALSE)
  }
  
  triads <- ts_get_table("Triad", conn = conn)
  triads <- triads[order(triads$Triad),]
  
  triads <- split(triads, triads$Triad)
  
  triads <- vapply(triads, doctor_triad, TRUE, conn = conn)
  all(triads)
}
