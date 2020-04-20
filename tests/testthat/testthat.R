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
  expect_error(read_pot(c(12),loc_WinFapFiles = "../.."))
})


test_that("read_pot works with multiple stations", {
  site_id <- c(12345,54321)
  potOut <- read_pot(site_id,loc_WinFapFiles = "../..")
  expect_is(potOut,"list") ## basic check
  expect_identical(names(potOut), c("12345","54321")) ## naming is correct?
  expect_warning(read_pot(c(12,12345),loc_WinFapFiles = "../.."))
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
  expect_error(read_amax(c(12),loc_WinFapFiles = "../.."))
})

test_that("read_amax works with multiple stations", {
  site_id <- c(12345,54321)
  amaxTable <- read_amax(site_id,loc_WinFapFiles = "../..")
  expect_is(amaxTable,"list") ## basic check
  expect_identical(names(amaxTable), c("12345","54321")) ## naming is correct?
  expect_warning(read_amax(c(12,12345),loc_WinFapFiles = "../.."))
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

test_that("read_cd3 works for multiple stations", {
  site_id <- c(12345,54321)
  cd3Out <- read_cd3(site_id,loc_WinFapFiles = "../..")
  expect_is(cd3Out,"list") ## basic check
  expect_identical(names(cd3Out), c("12345","54321")) ## naming is correct?
})

test_that("get_amax works", {
  a40003 <- get_amax(40003) # the Medway at Teston / East Farleigh
  multipleStations <- get_amax(c(40003, 42003))
  expect_is(a40003,"data.frame") ## basic check
  expect_identical(as.character(sapply(a40003, class)), c("numeric","numeric","Date","numeric","numeric","logical"))
  ## multiple stations
  expect_equal(length(multipleStations), 2)
  expect_identical(names(multipleStations), c("40003", "42003"))
  ### if the stations doesn't exist don't retrieve it
  expect_is(get_amax(c(72,72014)),"list")
  expect_length(get_amax(c(72,72014)),1)
})

test_that("get_pot works", {
  p40003 <- get_pot(40003) # the Medway at Teston / East Farleigh
  expect_is(p40003,"list") ## basic check
  expect_equal(length(p40003), 3)
  expect_identical(names(p40003),
                   c("tablePOT","WaterYearInfo","dateRange"))
  expect_equal(nrow(p40003$tablePOT[p40003$tablePOT$WaterYear == 1971,]) , 0)
  multipleStations <- get_pot(c(40003, 42003))
  ## multiple stations
  expect_equal(length(multipleStations), 2)
  expect_identical(names(multipleStations), c("40003", "42003"))
  ### if the stations doesn't exist don't retrieve it
  expect_is(get_pot(c(72,72014)),"list")
  expect_length(get_pot(c(72,72014)),1)
})


test_that("get_cd works", {
  cd40003 <- get_cd(40003) # the Medway at Teston / East Farleigh
  expect_is(cd40003,"data.frame") ## basic check
  expect_equal(nrow(cd40003), 1)
  expect_equal(cd40003$id, 40003)
  multipleStations <- get_cd(c(40003, 42003))
  ## multiple stations
  expect_equal(length(multipleStations), 2)
  expect_identical(names(multipleStations), c("40003", "42003"))
  ### if the stations doesn't exist don't retrieve it
  expect_is(get_cd(c(72,72014)),"list")
  expect_length(get_cd(c(72,72014)),1)
})


