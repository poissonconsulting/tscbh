context("package")

test_that("package", {
  file <- ":memory:"
  file <- "tscbh.sqlite"
  if(file.exists(file)) unlink(file)
  conn <- ts_create_db(file = file)
  teardown(ts_disconnect_db(conn))
  options(tsdbr.conn = conn)

  dir <- system.file("exdata", package = "tscbh", mustWork = TRUE)
  zrxp <- ts_read_zrxp(file = file.path(dir, "data.zrxp"))
  
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
  
  zrxp <- ts_translate_stations(zrxp)
  zrxp <- zrxp[!is.na(zrxp$Station),]
  expect_is(ts_add_data(zrxp), "data.frame")
#  expect_false(ts_doctor_db())
})
  