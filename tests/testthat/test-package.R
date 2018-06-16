context("package")

test_that("package", {
  
  dir <- system.file("exdata", package = "tscbh", mustWork = TRUE)
  
  zrxp <- ts_read_zrxp(file = file.path(dir, "data.zrxp"))
  zrxp2 <- ts_read_zrxp(file = file.path(dir, "data2.zrxp"))
  zrxp3 <- ts_read_zrxp(file = file.path(dir, "data3.zrxp"))
  
  zrxp <- rbind(zrxp, zrxp2, zrxp3)
  expect_identical(nrow(zrxp), 46249L)
  expect_identical(colnames(zrxp), c("Station", "DateTime", "Observed", "Status"))
})
  