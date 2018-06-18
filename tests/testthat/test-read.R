context("read")

test_that("read_zrxp", {
  dir <- system.file("exdata", package = "tscbh", mustWork = TRUE)
  
  zrxp <- ts_read_zrxp(file = file.path(dir, "data.zrxp"))
  zrxp2 <- ts_read_zrxp(file = file.path(dir, "data2.zrxp"))
  zrxp3 <- ts_read_zrxp(file = file.path(dir, "data3.zrxp"))
  zrxp <- rbind(zrxp, zrxp2, zrxp3)
  
  expect_identical(nrow(zrxp), 46249L)
  expect_identical(colnames(zrxp), c("Station", "DateTime", "Recorded", "Status"))
  expect_identical(zrxp[1,], data.frame(
    Station = "ALH_TurbineFlow", 
    DateTime = as.POSIXct("2015-03-31 00:00:00", tz = "Etc/GMT+8"),
    Recorded = 0,
    Status = ordered("reasonable", c("reasonable", "questionable", "erroneous")),
    stringsAsFactors = FALSE))
  
})
  
test_that("read_brd", {
  dir <- system.file("exdata", package = "tscbh", mustWork = TRUE)
  
  brd <- ts_read_brd(file = file.path(dir, "brd.csv"))
  brd2 <- ts_read_brd(file = file.path(dir, "brd2.csv"))
  brd <- rbind(brd, brd2)
  
  expect_identical(nrow(brd), 13101L)
  expect_identical(colnames(brd), c("Station", "DateTime", "Recorded", "Status"))
  brd$Recorded <- signif(brd$Recorded, 7)

  expect_identical(brd[1,], data.frame(
    Station = "BRD_FLOWS_AVG", 
    DateTime = as.POSIXct("2015-01-01 00:00:00", tz = "Etc/GMT+8"),
    Recorded = 140.2369,
    Status = ordered("reasonable", c("reasonable", "questionable", "erroneous")),
    stringsAsFactors = FALSE))
})
