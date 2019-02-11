
### PT ----

#' The main function to read Peaks-Over-Threshold (POT) data contained in .PT files
#'
#' The function reads .PT files and checks for the presence of any [POT GAPS] and [POT REJECTED] periods.
#'
#' @param station the NRFA station number for which the .PT file (names \code{station.PT}) should be read
#' @param loc_WinFapFiles the file.path of the WinFap files, i.e. the location in which the
#' \code{station.PT} file can be found. Default is the working directory
#' @param getAmax logical. If \code{TRUE} the annual maxima values (extracted from a \code{station.AM}
#' file) will be attached to the \code{WaterYearInfo} table
#'
#'
#' @return a list of three objects \code{tablePOT}, \code{WaterYearInfo} and \code{dateRange}.
#' @return \code{tablePOT} contains a table with all the peaks above the threshold present in the .PT file
#' @return \code{WaterYearInfo} a table containing the information on the percentage of missing values
#' in any water year for which some data is available in the POT record (based on the dates indicated in [POT REJECTED] or [POT GAPS]). This is useful to assess
#' whether the lack of exceedances is genuine or the result of missing data and to assess whether the threshold
#' exceedances present in \code{tablePOT} can be deemed to be representative of the whole year
#' @return \code{dateRange} a vector with the first and last date of recording for the POT record as provided in the [POT Details] field.
#' Note that this period might be different than the period for which annual maxima records are available
#' @seealso Information on the .PT files and river flow gauging in the UK can be found at the National River Flow Archive website \url{nrfa.ceh.ac.uk}
#' @export
read_pot <- function(station, loc_WinFapFiles = getwd(), getAmax = FALSE){
  # , rangeOut = FALSE # @param rangeOut logical, if TRUE and additional element is provided in the output containing a range of Water Year for the Record Period provied in the [POT Details] field
  ### read the station.PT file wherever it is in the loc_WinFapFiles folder
  wherePOT <- list.files(loc_WinFapFiles,recursive=TRUE,pattern = paste0("^",station,".PT"),full.names=TRUE)
  POTtable <- read.csv(file = as.character(wherePOT[1]),header=FALSE,stringsAsFactors = FALSE)
  statno <- as.character(POTtable[2,1])
  ########
  ##Create table shell using beginning and ending record periods
  beg <- POTtable$V2[POTtable$V1 == "Record Period"]
  end <- POTtable$V3[POTtable$V1 == "Record Period"]
  # find beginning and eng year of record
  beg <- dmy(beg); end <- dmy(end)
  dateRange <- c(beg,end)
  WYvec <- seq(from = ifelse(month(beg) < 10, year(beg) -1,year(beg)),
               ### need to correct for begginign and end in different water years
               to = ifelse(month(end) < 10, year(end) -1, year(end)))
  WYvec <- unique(WYvec)
  # if (month(beg) < 10){ #starts in previous WY
  #   WYvec <- append(year(beg) - 1 , WYvec)
  # }
  # if (month(end) < 10){ #ends in previous WY
  #   WYvec <- head(WYvec, -1)
  # }
  #Create WY table shell
  WY_table <- data.frame(WY = WYvec, percent_complete = 100,
                         threshold = as.numeric(as.character(POTtable$V2[POTtable$V1 == "Threshold"])))

  #First and Last percent_complete's aren't 100% as recording is only
  #for part of WY
  WY_1 <- ymd(paste(water_year(beg)+1,"Sep",30))
  WY_table$percent_complete[1] <- gap_percent(beg, WY_1)
  WY_2 <- ymd(paste(water_year(end), "Oct", 01))
  WY_table$percent_complete[length(WY_table$WY)] <- gap_percent(WY_2, end)

  ## where are the lines indicating gaps and rejected records?
  begGap <- grep('[POT GAPS]',toupper(POTtable[,1]),fixed = TRUE)
  endGap <- grep('[END]',toupper(POTtable[,1]),fixed = TRUE)[grep('[END]',toupper(POTtable[,1]),fixed = TRUE) > begGap][1]
  begRej <- grep('[POT REJECTED]',toupper(POTtable[,1]),fixed = TRUE)
  endRej <- grep('[END]',toupper(POTtable[,1]),fixed = TRUE)[grep('[END]',toupper(POTtable[,1]),fixed = TRUE) > begRej][1]

  ### initialise data frame of rejected/gaps dates
  combDates <- data.frame(beg = dmy("01-01-2055"), end = dmy("01-12-2055"))
  combDates <- combDates[-1,]
  if((length(begRej) > 0)) combDates <- rbind(combDates,data.frame(beg = dmy(POTtable[(begRej+1):(endRej-1),1]), end = dmy(POTtable[(begRej+1):(endRej-1),2])))
  if((length(begGap) > 0)) combDates <- rbind(combDates,data.frame(beg = dmy(POTtable[(begGap+1):(endGap-1),1]), end = dmy(POTtable[(begGap+1):(endGap-1),2])))
  if(nrow(combDates)>0){
    combDates <- unify_gaps_rej(combDates)
    WY_table <- compile_valid_data(datesDF = combDates, WY_table = WY_table)
  }
  ### more consistent naming using CamelCase
  names(WY_table) <- c("WaterYear","potPercComplete","potThreshold")
  tablePOT <- POTtable[seq(match('[POT Values]',POTtable[,1])+1,nrow(POTtable)-1),]
  names(tablePOT) <- c("Date","Flow","Stage")
  tablePOT$Date <- dmy(tablePOT$Date)
  tablePOT$Flow <- as.numeric(tablePOT$Flow)
  tablePOT$Stage <- as.numeric(tablePOT$Stage)
  tablePOT$Station <- as.numeric(statno)
  tablePOT$WaterYear <- water_year(tablePOT$Date)
  if(getAmax){
    amax <- read_amax(station = statno, loc_WinFapFiles = loc_WinFapFiles)
    names(amax) <- c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected")
    WY_table <- merge(amax,WY_table,all=TRUE)
    WY_table <- WY_table[,c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected","potPercComplete","potThreshold")]
    WY_table$Station <- statno
  }
  out <- list(tablePOT = tablePOT[,c("Station","Date","WaterYear","Flow","Stage")],
              WaterYearInfo = WY_table,
              dateRange = dateRange)
  out
}

### AM ----

#' The main function to read annual maxima (AMAX) data contained in .AM files
#'
#' The funtion reads .AM files while checking for the presence of any [AM Rejected] information
#'
#' @param station the NRFA station number for which the .AM file (names \code{station.AM}) should be read
#' @param loc_WinFapFiles the file.path of the WinFap files, i.e. the location in which the
#' \code{station.AM} file can be found. Default is the working directory
#'
#' @return a data.frame with information on the annual maxima for the station and the following columns
#' \describe{
#'  \item{Station}{NRFA station number}
#'  \item{WaterYear}{the correct water year for the peak flow}
#'  \item{Date}{date of maximum flow}
#'  \item{Flow}{the maximum flow in m3/s}
#'  \item{Stage}{the stage (height) reached by the river - this information is used to derive the flow via a rating curve}
#'  \item{Rejected}{logical, if TRUE the water year has been flagged as rejected by the NRFA}
#' }
#' @seealso Information on the .AM files and river flow gauging in the UK can be found at the National River Flow Archive website \url{nrfa.ceh.ac.uk}
#' @export
read_amax <- function(station, loc_WinFapFiles = getwd()){
  whereAM <- list.files(loc_WinFapFiles,recursive=TRUE,pattern = paste0("^",station,".AM"),full.names=TRUE)
  rr <- readLines(whereAM)
  aa <- rr[(which(rr == "[AM Values]")+1):(length(rr)-1)]
  out <- cbind(station,read.csv(textConnection(aa),header=FALSE))
  names(out) <- c("Station","Date","Flow","Stage")
  out$Date <- as.Date(out$Date,format = "%d %B %Y")
  out$WaterYear <- water_year(out$Date)
  #### Check if any amax should be rejected
  rejStart <- grep("[AM Rejected]",rr,fixed = TRUE)
  rejEnd <- grep("[END]",rr,fixed = TRUE)
  ## there are many [END] in the file - identify the one which closes the [AM Rejected] section
  rejEnd <- rejEnd[rejEnd > rejStart][1]
  rejWY <- NULL
  if(length(rejStart)>0){
    rejWY <- rr[(rejStart+1):(rejEnd-1)]
    ## check that missing amax are in the format you expect with WY repeated in each line
    if(length(unique(unlist(strsplit(rejWY,split = ",")))) != length(rejWY)) print(paste(station,"check lengths of rejected amax"))
    rejWY <- as.numeric(unique(unlist(strsplit(rejWY,split = ","))))
    zz <- data.frame(Station = rep(station,length(rejWY)), WaterYear = rejWY)
    zz$Rejected <- TRUE
    out <- merge(out,zz,all.x = TRUE)
    out[is.na(out$Rejected),"Rejected"] <- FALSE
  }
  if(length(rejStart) == 0) out$Rejected <- FALSE
  out <- out[order(out$Date),]
  ### known exceptions for events which happenend on 1st october before 9:00 AM in known_Oct1
  # if that happens substitute the right year
  if(station %in% known_Oct1$Station)
    out[out$Date %in% (known_Oct1[known_Oct1$Station == station,"Date"]), "WaterYear"] <- known_Oct1[known_Oct1$Station == station,"WaterYear"]
  out[,c("Station","WaterYear","Date","Flow","Stage","Rejected")]
}



### CD3 ----

split_or_NA <- function(x,ind=2) {
  ## needed because some catchment descriptors are missing in the cd3 files
  if(length(x) == 0) return(rep(NA,length(ind)))
  if(length(x) > 0) strsplit(x,",")[[1]][ind]
}

#' The main function to read the FEH catchment descriptors contained in .CD3 files
#'
#' The funtion reads .CD3 files and makes a data frame with the main catchment descriptors and information
#'
#' @param station the NRFA station number for which the .CD3 file (names \code{station.CD3}) should be read
#' @param loc_WinFapFiles the file.path of the WinFap files, i.e. the location in which the
#' \code{station.CD3} file can be found. Default is the working directory
#'
#' @return a data.frame with information on the catchment descriptors for the station
#' @seealso Information on the .CD3 files and river flow gauging in the UK can be found at the National River Flow Archive website \url{nrfa.ceh.ac.uk}.
#' Specific information on the catchment descriptors can be found at \url{https://nrfa.ceh.ac.uk/feh-catchment-descriptors}
#' @export
read_cd3 <- function(station, loc_WinFapFiles = getwd()){
  whereCD <- list.files(loc_WinFapFiles,recursive=TRUE,pattern = paste0("^",station,".CD3"),full.names=TRUE)
  rr <- readLines(whereCD )
  oo <- rr[(which(rr == "[STATION NUMBER]")+1)]
  zz <- rr[(which(rr == "[CDS DETAILS]")+1):(which(rr == "[CDS DETAILS]")+2)]
  oo <- c(oo,unlist(lapply(strsplit(zz,","), function(x) x[[2]])))
  oo <- c(oo,split_or_NA(grep("NOMINAL NGR",rr,value = TRUE),ind = c(2,3)))
  oo <- c(oo,split_or_NA(grep("IHDTM NGR",rr,value = TRUE),ind = c(3,4)))
  oo <- c(oo,split_or_NA(grep("CENTROID NGR",rr,value = TRUE),ind = c(3,4,2)))
  oo <- c(oo,split_or_NA(grep("DTM AREA",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("ALTBAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("ASPBAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("ASPVAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("BFIHOST",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("DPLBAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("DPSBAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("FARL",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("FPEXT",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("LDP",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("PROPWET",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("RMED-1H",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("RMED-1D",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("RMED-2D",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("SAAR",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("SAAR4170",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("SPRHOST",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBCONC1990",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBEXT1990",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBLOC1990",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBCONC2000",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBEXT2000",rr,value = TRUE),ind = 2))
  oo <- c(oo,split_or_NA(grep("URBLOC2000",rr,value = TRUE),ind = 2))
  catchDesc <- data.frame(data.frame(matrix(rep("Nothing",3),ncol = 3)),data.frame(matrix(rep(-3,30),ncol = 30)))
  names(catchDesc) <- c("Station","River","Location","Nominal_Easting", "Nominal_Northing",
                        "Easting","Northing", "Ceasting", "Cnorthing", "Grid", "DTM_Area", "ALTBAR",
                        "ASPBAR", "ASPVAR", "BFIHOST","DPLBAR", "DPSBAR", "FARL",
                        "FPEXT", "LDP", "PROPWET", "RMED1H", "RMED1D", "RMED2D",
                        "SAAR", "SAAR4170", "SPRHOST","URBCONC1990", "URBEXT1990",
                        "URBLOC1990", "URBCONC2000", "URBEXT2000","URBLOC2000")
  catchDesc[1,c(1:3,10)] <- oo[c(1:3,10)]
  catchDesc[1,c(4:9,11:33)] <- as.numeric(oo[c(4:9,11:33)])
  zz <- rr[(which(rr == "[SUITABILITY]")+1):(which(rr == "[SUITABILITY]")+2)]
  oo <- unlist(lapply(strsplit(zz,","), function(x) x[[2]]))
  catchDesc$suitQMED <- oo[1]
  catchDesc$suitPool <- oo[2]
  catchDesc
}


