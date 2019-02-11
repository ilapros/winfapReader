options(stringsAsFactors = FALSE)

#' Known events which happened on October 1st before 9am
#'
#' The Water Year in the UK runs from 9am of the 1st October of a given year
#' to 8:59am of the 1st October of the next year. Since the WinFap files contain
#' information only on the date of the annual maximum (and not time) it is possible that an event is
#' mis-classified when using the \code{water_year} function. This dataset lists the events which are
#' known to have happened to October 1st before 9am. This is used to correct the \code{WaterYear}
#' information in these known cases in the \code{read_amax} function
#'
#'
#' @format A data frame with 17 rows and 3 variables:
#' \describe{
#'   \item{Station}{NRFA station number}
#'   \item{Date}{date of maximum flow (always the 1st October)}
#'   \item{WaterYear}{the correct water year for the peak flow}
#' }
#' @source Derived manually by identifying events which happened on Oct. 1st and comparing it with information on \url{nrfa.ceh.ac.uk}
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

########
# The code used to identify the stations for which issues on the 1st of October happen is given below
# read_amax <- function(station, loc_WinFapFiles = getwd()){
#   whereAM <- list.files(loc_WinFapFiles,recursive=TRUE,pattern = paste0("^",station,".AM"),full.names=TRUE)
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
# suitPool <- unlist(strsplit(list.files(path = "../v6/Suitable for Pooling/", pattern = ".AM"),split = ".AM"))
# suitQMED <- unlist(strsplit(list.files(path = "../v6/Suitable for QMED//", pattern = ".AM"),split = ".AM"))
# suitNO   <- unlist(strsplit(list.files(path = "../v6/Not suitable for QMED or Pooling/", pattern = ".AM"),split = ".AM"))
#
# for(whichFold in c("../v6/Not suitable for QMED or Pooling/","../v6/Suitable for QMED/","../v6/Suitable for Pooling/")){
#   print(whichFold)
#   for(i in unlist(strsplit(list.files(path = whichFold, pattern = ".AM"),split = ".AM"))){
#     allAmax <- rbind(allAmax, read_amax(i,whichFold))
#   }
# }


##### check if any station has two events per water years - probably due to 1-Oct issues
# tt <- tapply(allAmax$WaterYear, factor(allAmax$Station), function(x) any(table(x) != 1))
# tt[tt]
#### after this manual check on the NRFA websites were carried out
