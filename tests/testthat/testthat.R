library(testthat)
library(winfapReader)


test_that("read_pot works", {
  site_id <- 12345
  potOut <- read_pot(site_id,loc_WinFapFiles = "../..")
  expect_is(potOut$WaterYearInfo,"data.frame") ## basic check
  expect_is(potOut$tablePOT,"data.frame") ## basic check
  expect_gt(nrow(potOut$tablePOT),198) ## basic check
  expect_is(potOut$tablePOT$Date,"Date") ## in case things get messy with lubridate
  expect_is(potOut$tablePOT$Flow,"numeric") ## in case things get messy with the reading of things
  expect_gt(potOut$WaterYearInfo[potOut$WaterYearInfo$WaterYear == 2000,"potPercComplete"], 13)
  expect_lt(potOut$WaterYearInfo[potOut$WaterYearInfo$WaterYear == 2000,"potPercComplete"], 14)
  expect_lt(potOut$WaterYearInfo[potOut$WaterYearInfo$WaterYear == 2008,"potPercComplete"], 1)
})


test_that("read_pot works - getAmax", {
  site_id <- 12345
  potOut <- read_pot(site_id,loc_WinFapFiles = "../..", getAmax = TRUE)
  expect_true(potOut$WaterYearInfo$amaxRejected[potOut$WaterYearInfo$WaterYear == 1974])
  expect_gt(ncol(potOut$tablePOT),3) ## basic check
})


test_that("read_amax works", {
  site_id <- 12345
  amaxTable <- read_amax(site_id,loc_WinFapFiles = "../..")
  expect_is(amaxTable,"data.frame") ## basic check
  expect_is(amaxTable$Date,"Date") ## in case things get messy with lubridate
  expect_true(amaxTable$Rejected[amaxTable$WaterYear == 1974]) ## check rejections are read in correcty
  expect_equal(sum(!amaxTable$Rejected), 57)
})

