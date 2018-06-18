doctor_triad <- function(triad, span, fix, conn) {
  stations <- ts_get_stations(conn = conn)
  stations <- stations[stations$Station %in% triad,]
  if(nrow(stations) != 3L) {
    warning("missing triad: ", punctuate(triad, "and"))
    return(FALSE)
  }
  stopifnot(all(stations$Period == "hour"))
  span <- span[span$Station %in% triad[2:3],]
  start <- min(span$Start)
  end <- max(span$End)
  
  data <- ts_get_data(stations = triad, 
                      start_date = lubridate::date(start), 
                      end_date = lubridate::date(end) + 1,
                      period = "second",
                      status = "erroneous",
                      conn = conn)
  
  data <- data[c("Station", "DateTime", "Recorded", "Corrected", "Status")]
  
  data <- data[data$DateTime >= start & data$DateTime <= end,]
  
  print(head(data))
  
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

  triad1$Corrected[na2] <- triad2$Corrected[na2] + triad3$Corrected[na2]
  is.na(triad1$Corrected[na1]) <- TRUE  
  triad1$Status <- pmax(triad1$Status, triad2$Status, triad3$Status)
  triad1$Status[is.na(triad1$Corrected)] <- "reasonable"
  
  ts_add_data(data = triad1, resolution = "replace")
  return(TRUE)
}

#' Doctor Database
#'
#' @inheritParams tsdbr::ts_doctor_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(conn = getOption("tsdbr.conn", NULL)) {
  
  flag <- TRUE
  # tsdbr::ts_doctor_db(check_limits = TRUE, 
  #                     check_period = TRUE, 
  #                     check_gaps = TRUE,
  #                     fix = TRUE, 
  #                     conn = conn)
  
  if(!flag) {
    warning("did not check triads as previous checks failed!")
    return(FALSE)
  }
  
  span <- ts_get_table("DataSpan", conn = conn)
  span$Start <- as.POSIXct(span$Start, tz = "Etc/GMT+8")
  span$End <- as.POSIXct(span$End, tz = "Etc/GMT+8")
  
  triads <- list()
  triads <- c(list(c("DDM_SPOG", "DDM_SPOG1", "DDM_SPOG2")))
  # triads <- c(list(c("DDM_LLOG", "DDM_LLOG1", "DDM_LLOG2")))
  # triads <- c(list(c("DDM", "DDM_LLOG", "DDM_SPOG")))
  # triads <- c(list(c("REV", "REVTB", "REVS")))
  # triads <- c(list(c("MCA", "MCATB", "MCAS")))
  # triads <- c(list(c("HLK_ALH", "HLK", "ALK")))
  # triads <- c(list(c("BRD", "BRDTB", "BRDS")))
  # triads <- c(list(c("BRD_BRX", "BRD", "BRX")))
  
  triads <- vapply(triads, doctor_triad, TRUE, span = span, fix = fix, conn = conn)
  all(triads)
}
