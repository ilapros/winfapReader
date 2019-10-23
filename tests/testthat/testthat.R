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

cdtrue <-
  data.frame(Station = "12345",
          River = "Name",
          Location = "The Location",
          Nominal_Easting = 2800,
          Nominal_Northing = 2800,
          Easting = 280000,
          Northing = 280000,
          Ceasting = 123456,
          Cnorthing = 123456,
          Grid = "GB",
          DTM_Area = 100,
          ALTBAR = 500,
          ASPBAR =100,
          ASPVAR =0.5,
          BFIHOST =0.4,
          DPLBAR =  3,
          DPSBAR = 192.4,
          FARL =1.000,
          FPEXT =0.0164,
          LDP =  5.89,
          PROPWET =0.66,
          RMED1H = 13.2,
          RMED1D = 75.3,
          RMED2D =101.4,
          SAAR =2457,
          SAAR4170 =2392,
          SPRHOST =48.38,
          URBCONC1990 =-9999.000,
          URBEXT1990 =0.0000,
          URBLOC1990 =-9999.000,
          URBCONC2000 =-9999.000,
          URBEXT2000 =0.0000,
          URBLOC2000 =-9999.000,
          suitQMED = "NO",
          suitPool = "NO")

test_that("read_cd3 works", {
  site_id <- 12345
  cd3Out <- read_cd3(site_id,loc_WinFapFiles = "../..")
  expect_is(cd3Out,"data.frame") ## basic check
  ### can have troubles with factorsv
  testthat::expect_type(cd3Out$Location,"character")
  testthat::expect_type(cd3Out$River,"character")
  expect_identical(cd3Out, cdtrue) ## is everything read in correctly?
})

