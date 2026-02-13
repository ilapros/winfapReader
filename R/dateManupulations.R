
#' Derive water year value for a date
#'
#' @param date the (vector of) dates for which the water year will be calculated
#' @param start_month the month in which the water year starts, default is October
#' @param start_hour the time of the day in which the water year starts, default is 9
#' @param tz time zone for the record, defaults to "UTC"
#' @return The water year value
#' @export
#' @examples
#' \dontrun{
#' water_year(c(as.POSIXct("1958-10-01 09:00:00", tz = "UTC"),
#'              as.POSIXct("2021-10-01 12:00:00", tz = "UTC"),
#'              as.POSIXct("1918-10-01 08:55:00", tz = "UTC")))
#'
#' water_year(c(as.Date("2021-01-01"),as.Date("2021-11-01"),
#' as.Date("2019-02-01"),as.Date("1965-09-30")))
#' }
#' @importFrom utils read.csv
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate leap_year
#' @importFrom lubridate dmy
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate date
#' @importFrom lubridate int_overlaps
#' @importFrom lubridate interval
#' @importFrom lubridate as_datetime
#' @importFrom lubridate hours
#' @importFrom lubridate make_datetime
water_year <- function(date, start_month = 10, start_hour = 9, tz = "UTC") {
  # Normalize input to POSIXct
  if (inherits(date, "Date")) {
    x_dt <- lubridate::as_datetime(date, tz = tz)+ lubridate::hours(start_hour)
  } else if (inherits(date, "POSIXt")) {
    x_dt <- date
  } else {
    stop("Input must be Date or POSIXt.")
  }
  # Calendar year of the given date/time
  yr <- lubridate::year(x_dt)
  # Construct the water-year start datetime
  wy_start <- lubridate::make_datetime(
    year  = yr,
    month = start_month,
    day   = 1,
    hour  = start_hour,
    tz    = tz
  )
  # Determine water year
  yr[(x_dt < wy_start)] <- yr[(x_dt < wy_start)]-1
  yr
}


gap_percent <- function(beg, end){
  #Given 2 dates in ymd lubridate form IN THE SAME WATER YEAR, returns
  #the percentage of that WY that they span
  #if any of the two dates pairs is in different water years the function will stop and throw an error
  bwy <- water_year(beg)
  if(any(bwy != water_year(end))){
    stop('gap_percent error - dates not in same WY')
  } else if(any((end - beg) < 0)){
    stop('gap_percent error - some end dates of gaps are earlier than the begin dates')
  } else {
    gap_days <- as.numeric(end - beg)
    leap <- leap_year(bwy+1) #check for leap year
    gap_percent <- 100 * gap_days / (365+leap)
  }
  return(gap_percent)
}

int_extend <- function(int1,int2){
  ## used when two intervals of missing data are overlapping
  ## derives the larger extent of the combined interval
  c(min(int1[,1],int2[,1]),max(int1[,2],int2[,2]))
}


unify_gaps_rej <- function(datesDF){
  ## this function is used to make a unified set of invalid dates from the .PT files
  ## these could be due to missing data or rejected data - sometimes these periods overlap
  ### all the list/matrix things are needed in case unequal numbers of comparisons are made for each interval
  ### also gets useful when only two gaps are present
  ## first identify whether any overlapping intervals exist
  checkOvers <- apply(datesDF,1, function(x) int_overlaps(interval(x[1],x[2]),interval(datesDF$beg[!(datesDF$beg == x[1] & datesDF$end == x[2])],datesDF$end[!(datesDF$beg == x[1] & datesDF$end == x[2])])))
  while(ifelse(is.matrix(checkOvers),any(checkOvers),any(unlist(lapply(checkOvers,function(x) any(x)))))){
    whereOverFound <- apply(datesDF,1,
                            function(x) int_overlaps(interval(x[1],x[2]),
                                                     interval(datesDF$beg[!(datesDF$beg == x[1] & datesDF$end == x[2])],datesDF$end[!(datesDF$beg == x[1] & datesDF$end == x[2])])))
    whereOverFound <- ifelse(is.matrix(whereOverFound),
                             as.numeric(which(whereOverFound,arr.ind = TRUE)[1,2]),
                             which(unlist(lapply(whereOverFound,function(x) any(x))))[1])

    whereOther <- which(int_overlaps(interval(datesDF[whereOverFound,1],datesDF[whereOverFound,2]),interval(datesDF[,1],datesDF[,2])))
    whereOther <- whereOther[whereOther != whereOverFound]
    for(j in whereOther) {zz <- int_extend(datesDF[whereOverFound,],datesDF[j,]); datesDF[nrow(datesDF)+1,1] <- zz[1]; datesDF[nrow(datesDF),2] <- zz[2];rm(zz)}
    rownames(datesDF) <- NULL
    datesDF <- datesDF[-c(whereOverFound,whereOther),]
    ### if rejection or gaps overlaps many other intervals we'll end up with duplicates - remove those!
    datesDF <- datesDF[!duplicated(datesDF),]
    checkOvers <- apply(datesDF,1, function(x) int_overlaps(interval(x[1],x[2]),interval(datesDF$beg[!(datesDF$beg == x[1] & datesDF$end == x[2])],datesDF$end[!(datesDF$beg == x[1] & datesDF$end == x[2])])))
    rownames(datesDF) <- NULL
  }
  datesDF <- datesDF[order(datesDF$beg),]
  rownames(datesDF) <- NULL
  datesDF
}


compile_valid_data <- function(datesDF,WY_table){
  for (i in 1 : nrow(datesDF)){ ##
    beg <- datesDF$beg[i]
    end <- datesDF$end[i]
    # Use water_year function
    # difference in year between beginning and end of mssing records
    diff <- water_year(end) - water_year(beg)

    #3 cases here - when the gap  spans 0, 1, and >1 WY
    #CASE1
    if (diff == 0){
      gap_percent <- gap_percent(beg,end)
      #update WY_table
      temp <- WY_table$percent_complete[WY_table$WY ==
                                         water_year(beg)]
      temp <- temp - gap_percent
      WY_table$percent_complete[WY_table$WY == water_year(beg)] <-
        temp
    }
    #CASE2
    if (diff == 1){
      WY_split <- ymd(paste(water_year(end),"Oct",01))
      #update First WY
      gap_percent <- gap_percent(beg,WY_split-1)
      #the "-1" above is because 1st Oct is in next WY
      temp <- WY_table$percent_complete[WY_table$WY ==
                                         water_year(beg)]
      temp <- temp - gap_percent
      WY_table$percent_complete[WY_table$WY == water_year(beg)] <-
        temp
      #update Second WY
      gap_percent <- gap_percent(WY_split, end)
      temp <- WY_table$percent_complete[WY_table$WY ==
                                         water_year(end)]
      temp <- temp - gap_percent
      WY_table$percent_complete[WY_table$WY == water_year(end)] <-
        temp
    }
    #CASE3
    if (diff > 1){
      #Update First WY
      WY_1 <- ymd(paste(water_year(beg)+1,"Oct",01))
      gap_percent <- gap_percent(beg,WY_1-1)
      #the "-1" above is because 1st Oct is in next WY
      temp <- WY_table$percent_complete[WY_table$WY ==
                                         water_year(beg)]
      temp <- temp - gap_percent
      WY_table$percent_complete[WY_table$WY == water_year(beg)] <-
        temp
      #Update Last WY
      WY_2 <- ymd(paste(water_year(end),"Oct",01))
      gap_percent <- gap_percent(WY_2, end)
      temp <- WY_table$percent_complete[WY_table$WY ==
                                         water_year(end)]
      temp <- temp - gap_percent
      WY_table$percent_complete[WY_table$WY == water_year(end)] <-
        temp
      #Update all middle WYs (that have no data!!)
      WY_no_data <- c( (water_year(beg)+1) : (water_year(end)-1) )
      WY_table$percent_complete[WY_table$WY %in% WY_no_data] <- 0
    }
  }
  WY_table
}


#
#
# gap_percent(c(date("2010-04-03"),date("2011-01-03")),c(date("2010-06-03"),date("2011-02-03")))
#
# testthat::test_that("gap_percent on incorrect date pairs", {
#   testthat::expect_equal(gap_percent(c(date("2010-04-03"),
#                                        date("2011-01-03")),c(date("2010-06-03"), date("2013-02-03"))))
#                          })

