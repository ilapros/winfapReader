options(stringsAsFactors = FALSE)

#' Known events which happened on October 1st before 9am
#'
#' The Water Year in the UK runs from 9am of the 1st October of a given year
#' to 8:59am of the 1st October of the next year. Since the WINFAP files contain
#' information only on the date of the annual maximum (and not time) it is possible that an event is
#' mis-classified when using the \code{water_year} function. This dataset lists the events which are
#' known to have happened to October 1st before 9am. This is used to correct the \code{WaterYear}
#' information in these known cases in the \code{read_amax} and \code{get_amax} functions. For some stations
#' events on October 1st have been deemed as annual maxima only in some winfap releases. They are
#' maintained in the dataset in the event that somebody read old winfap files.
#'
#'
#' @format A data frame with 36 rows and 3 variables:
#' \describe{
#'   \item{Station}{NRFA station number}
#'   \item{Date}{date of maximum flow (always the 1st October)}
#'   \item{WaterYear}{the correct water year for the peak flow}
#' }
#' @source Derived manually by identifying events which happened on Oct. 1st and comparing it with information on \url{https://nrfa.ceh.ac.uk}
"known_Oct1"
############# events which happened before 9am on 1st October - and therefore of the previous Water Year
known_Oct1 <- data.frame(Station = 6008, Date = as.Date("1985-10-01"), WaterYear = 1984)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(38001,as.Date("1917-10-01"),1916)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(38001, as.Date("1965-10-01"), 1964)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(32029, as.Date("1964-10-01"), 1963)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(39018, as.Date("1962-10-01"), 1961)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(69030, as.Date("1968-10-01"), 1967)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(27035, as.Date("1967-10-01"), 1966)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(42009, as.Date("1971-10-01"), 1970)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(45005, as.Date("1962-10-01"), 1961)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(67008, as.Date("1965-10-01"), 1964)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(84011, as.Date("1985-10-01"), 1984)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(12007, as.Date("2001-10-01"), 2000)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(15013, as.Date("2001-10-01"), 2000)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(46006, as.Date("2010-10-01"), 2009)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(39036, as.Date("2006-10-01"), 2005)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(47011, as.Date("2010-10-01"), 2009)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(69024, as.Date("1967-10-01"), 1966)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(205008, as.Date("1975-10-01"), 1974)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(72014, as.Date("1967-10-01"), 1966)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(84014, as.Date("1970-10-01"), 1969)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28052, as.Date("2019-10-01"), 2018) ## appeared in v9
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(30015, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28009, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28022, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28024, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28056, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(31005, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(54114, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(30017, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(31010, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(31025, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(32003, as.Date("2019-10-01"), 2018)
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(28015, as.Date("2019-10-01"), 2018) ## no longer on Oct1st in v10, leave in case somebody uses this to read files from v9.0
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(54102, as.Date("2019-10-01"), 2018) ## appeared in v10; in v11 the max for the year is no longer on Oct 1st; leave for those reading using old WinfapFiles
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(11002, as.Date("1976-10-01"), 1975) ## changed in v10
known_Oct1[nrow(known_Oct1)+1,] <- data.frame(32004, as.Date("2019-10-01"), 2018) ## changed in v11

# known_Oct1[nrow(known_Oct1)+1,] <- data.frame(84011, as.Date("1985-10-01"), 1984)
# usethis::use_data(known_Oct1, overwrite = TRUE)

#
########
########
# The code used to identify the stations for which issues on the 1st of October happen is given below
# library(lubridate)
# water_year <- function(date, start_month = 10){
#   # Given a date in ymd lubridate form, returns the WY that date is in.
#   # The XXXX Water year starts 01st start_month XXXX and ends on the last day of the month before the start_month (XXXX+1)
#   year(date) + ifelse(month(date) < start_month,-1,0)
# }
#
# read_amax <- function(station, loc_WinFapFiles = getwd()){
#   whereAM <- list.files(loc_WinFapFiles,recursive=TRUE,pattern = paste0("^",station,".am|.AM"),full.names=TRUE)
#   rr <- readLines(whereAM)
#   aa <- rr[(which(rr == "[AM Values]")+1):(length(rr)-1)]
#   out <- cbind(station,read.csv(textConnection(aa),header=FALSE))
#   names(out) <- c("Station","Date","Flow","Stage")
#   out$Date <- as.Date(out$Date,format = "%d %B %Y")
#   out$WaterYear <- water_year(out$Date)
#   #### Check if any amax should be rejected
#   rejStart <- grep("[AM Rejected]",rr,fixed = TRUE)
#   rejEnd <- grep("[END]",rr,fixed = TRUE)
#   ## there are many [END] in the file - identify the one which closes the [AM Rejected] section
#   rejEnd <- rejEnd[rejEnd > rejStart][1]
#   rejWY <- NULL
#   if(length(rejStart)>0){
#     rejWY <- rr[(rejStart+1):(rejEnd-1)]
#     ## check that missing amax are in the format you expect with WY repeated in each line
#     if(length(unique(unlist(strsplit(rejWY,split = ",")))) != length(rejWY)) print(paste(i,"check lengths of rejected amax"))
#     rejWY <- as.numeric(unique(unlist(strsplit(rejWY,split = ","))))
#     zz <- data.frame(Station = rep(station,length(rejWY)), WaterYear = rejWY)
#     zz$Rejected <- TRUE
#     out <- merge(out,zz,all.x = TRUE)
#     out[is.na(out$Rejected),"Rejected"] <- FALSE
#   }
#   if(length(rejStart) == 0) out$Rejected <- FALSE
#   out <- out[order(out$Date),]
#   out[,c("Station","WaterYear","Date","Flow","Stage","Rejected")]
# }
#
#
#
# # initialise empty data.frames
#
# allAmax <- data.frame(Station = -999, WaterYear = 3000, Date = as.Date("3000-01-01"),Flow = -999, Stage = -999, Rejected = FALSE)[-1,]
#
# ### v90 changed the names of folders
# suitPool <- "../v90/suitable-for-pooling/"
# suitQMED <- "../v90/suitable-for-qmed/"
# suitNO   <- "../v90/suitable-for-neither/"
#
# for(whichFold in c(suitPool,suitQMED,suitNO)){
#   print(whichFold)
#   for(i in unlist(strsplit(list.files(path = whichFold, pattern = ".AM|.am"),split = ".AM|.am"))){
#     allAmax <- rbind(allAmax, read_amax(i,whichFold))
#   }
# }
# allAmax <- allAmax[order(allAmax$Station,allAmax$Date),]
#
# ##### check if any station has two events per water years - probably due to 1-Oct issues
# tt <- tapply(allAmax$WaterYear, factor(allAmax$Station), function(x) any(table(x) != 1))
# tt[tt]
#
# ## 8 events on Oct 1 after 9:00 so they get assigned the right WY
# dim(known_Oct1[order(known_Oct1$Station, known_Oct1$Date),]) ## 20
# dim(allAmax[allAmax$Station %in% names(tt[tt]) &
#               month(allAmax$Date) == 10 &
#               day(allAmax$Date) == 1,]) ## 28
# ## any recent event on Oct 1st
# sort(allAmax[allAmax$Station %in% names(tt[tt]) &
#                month(allAmax$Date) == 10 &
#                day(allAmax$Date) == 1,"WaterYear"])
# ### after this manual check on the NRFA websites were carried out
# sort(as.numeric(names(tt[tt])))
# sort(unique(known_Oct1$Station))
# all.equal(sort(as.numeric(names(tt[tt]))), sort(unique(known_Oct1$Station)))
## all in there
