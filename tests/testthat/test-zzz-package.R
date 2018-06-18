context("package")

test_that("package", {
  file <- ":memory:"
  file <- "tscbh.sqlite"
  if(file.exists(file)) unlink(file)
  conn <- ts_create_db(file = file)
  teardown(ts_disconnect_db(conn))
  options(tsdbr.conn = conn)
  
  dir <- system.file("exdata", package = "tscbh", mustWork = TRUE)
  
  expect_is(ts_add_parameter("discharge", "cumecs"), "data.frame")
  expect_is(ts_add_site("Duncan Dam"), "data.frame")
  
  stations <- data.frame(
    Station = c("DDM", "DDM_SPOG", "DDM_LLOG", "DDM_SPOG1", 
                "DDM_SPOG2", "DDM_LLOG1", "DDM_LLOG2"),
    Parameter = "discharge",
    Site = "Duncan Dam",
    Period = "hour",
    StationID = c("Duncan_Releases", "Duncan_SPOG_Releases",
                  "Duncan_LLOG_Releases", "Duncan_SPOG1_Releases",
                  "Duncan_SPOG2_Releases", "Duncan_LLOG1_Releases",
                  "Duncan_LLOG2_Releases"),
    stringsAsFactors = FALSE)
  
  expect_is(ts_add_stations(stations), "data.frame")
  
  stations <- data.frame(
    Station = c("REV", "REVTB", "REVS"),
    Parameter = "discharge",
    Site = "Revelstoke Dam",
    Period = "hour",
    stringsAsFactors = FALSE)
  
  expect_is(ts_add_site("Revelstoke Dam"), "data.frame")
  expect_is(ts_add_stations(stations), "data.frame")
  
  ts_add_triad("DDM_SPOG", "DDM_SPOG1", "DDM_SPOG2")
  ts_add_triad("DDM_LLOG", "DDM_LLOG1", "DDM_LLOG2")
  ts_add_triad("DDM", "DDM_LLOG", "DDM_SPOG")
  ts_add_triad("REV", "REVTB", "REVS")
  expect_error(ts_add_triad("MCA", "MCATB", "MCAS"))
  # ts_add_triad("HLK_ALH", "HLK", "ALK")
  # ts_add_triad("BRD", "BRDTB", "BRDS")
  # ts_add_triad("BRD_BRX", "BRD", "BRX")
  
  zrxp <- ts_read_zrxp(file = file.path(dir, "data.zrxp"))
  zrxp <- zrxp[lubridate::day(zrxp$DateTime) == 31,]
  zrxp <- ts_translate_stations(zrxp)
  zrxp <- zrxp[!is.na(zrxp$Station),]
  zrxp$Recorded[lubridate::hour(zrxp$DateTime) == 1] <- NA
  zrxp$Recorded[zrxp$Station == "DDM_LLOG1" & lubridate::hour(zrxp$DateTime) == 2] <- NA
  zrxp$Recorded[zrxp$Station == "DDM" & lubridate::hour(zrxp$DateTime) == 3] <- NA
  zrxp$Status[zrxp$Station == "DDM_LLOG1" & lubridate::hour(zrxp$DateTime) == 4] <- "questionable"
  zrxp$Status[zrxp$Station == "DDM_LLOG2" & lubridate::hour(zrxp$DateTime) == 5] <- "erroneous"
  zrxp$Recorded[zrxp$Station == "DDM" & lubridate::hour(zrxp$DateTime) == 6] <- 
    zrxp$Recorded[zrxp$Station == "DDM" & lubridate::hour(zrxp$DateTime) == 6] + 1
  
  expect_is(ts_add_data(zrxp), "data.frame")
  expect_error(ts_add_data(zrxp))
  data <- ts_get_data(start_date = as.Date("2015-03-31"), end_date = as.Date("2015-04-01"))
  expect_message(ts_doctor_db(), "no parent data for triad REV = REVTB [+] REVS")
  data <- ts_get_data(start_date = as.Date("2015-03-31"), end_date = as.Date("2015-04-01"),
                      status = "erroneous")
  
  ddm <- data[data$Station == "DDM" & lubridate::hour(data$DateTime) %in% c(1,3,6),]
  expect_identical(ddm$Corrected, c(NA, 111.596, 111.451))
  expect_identical(ddm$Recorded, c(NA, NA, 112.451))
  expect_identical(ddm$Status, ordered(rep("reasonable", 3), c("reasonable", "questionable", "erroneous")))
  llog <- data[data$Station == "DDM_LLOG" & lubridate::hour(data$DateTime) %in% c(1,2,4,5),]
  expect_identical(llog$Corrected, c(NA, NA, 111.542, 111.505))
  expect_identical(llog$Recorded, rep(NA_real_, 4))
  expect_identical(llog$Status, ordered(c("reasonable", "reasonable", "questionable", "erroneous"), c("reasonable", "questionable", "erroneous")))
})
