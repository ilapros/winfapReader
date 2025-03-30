# utils::globalVariables("read_amax")

### PT ----

#' A function to read .PT files
#'
#' The function reads .PT files once these are in a local folder: these files contain information on Peaks-Over-Threshold (POT) records from the instantaneous river flow measurements.
#' The function checks for the presence of any [POT GAPS] and [POT REJECTED] periods.
#' If these are present, they are merged and information on the proportion of days with missing records in each water year is provided.
#' @param station NRFA station number(s) for which the .PT file (names \code{station.PT}) should be read.
#' @param loc_WinFapFiles the file.path of the WINFAP files, i.e. the location in which the
#' \code{station.PT} file can be found. Default is the working directory
#' @param getAmax logical. If \code{TRUE} the annual maxima values (extracted from a \code{station.AM}
#' file) will be attached to the \code{WaterYearInfo} table
#'
#'
#' @return a list of three objects \code{tablePOT}, \code{WaterYearInfo} and \code{dateRange}.
#' @return \code{tablePOT} contains a table with all the peaks above the threshold present in the .PT file
#' @return \code{WaterYearInfo} a table containing the information on the percentage of missing values
#' in any water year for which some data is available in the POT record. This is useful to assess
#' whether the lack of exceedances is genuine or the result of missing data and to assess whether the threshold
#' exceedances present in \code{tablePOT} can be deemed to be representative of the whole year
#' @return \code{dateRange} a vector with the first and last date of recording for the POT record as provided in the [POT Details] field.
#' Note that this period might be different than the period for which annual maxima records are available
#' @seealso Information on the .PT files and river flow gauging in the UK can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}
#' @export
read_pot <- function(station, loc_WinFapFiles = getwd(), getAmax = FALSE){
  ## need to have a way to specify if the amax should be found locally or obtained from API
  ## this overloads the getAmax which if a path becomes the path where files are
  typeget <- ifelse(getAmax,loc_WinFapFiles,"none")

  if(length(station) == 1){
    ### read the station.PT file wherever it is in the loc_WinFapFiles folder
    wherePOT <- findfile(loc_WinFapFiles = loc_WinFapFiles,station = station,whichFile = ".pt")
    if(length(wherePOT) < 1) stop("Station does not have PT files in the loc_WinFapFiles folder")
    out <- read_pot_int(wherePOT, getAmax = typeget)
  }
  if(length(station) > 1) {
    wherePOT <- lapply(X = as.list(station), findfile,loc_WinFapFiles=loc_WinFapFiles,whichFile = ".pt")
    lwhere <- sapply(wherePOT, length)
    if(all(lwhere < 1)){
      stop(paste("Stations do not have POT files in the loc_WinFapFiles folder \n"))
    }
    if(any(lwhere < 1)){
      warning(paste("Some stations do not have POT files in the loc_WinFapFiles folder:",station[lwhere < 1]," \n"))
      wherePOT <- wherePOT[lwhere > 0]
      station <- station[lwhere > 0]
    }
    out <- lapply(wherePOT, read_pot_int, getAmax = typeget)
    names(out) <- station
  }
  out
}


read_pot_int <- function(filetext, getAmax){
  POTtable <- utils::read.csv(file = filetext,header=FALSE,stringsAsFactors = FALSE)
  statno <- as.character(POTtable[2,1])

  ##Create table shell using beginning and ending record periods
  beg <- POTtable$V2[POTtable$V1 == "Record Period"]
  end <- POTtable$V3[POTtable$V1 == "Record Period"]
  #error if table is empty
  if(is.null(beg)) stop(sprintf("no pot recorded at station %s", statno), call. = FALSE)
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
  if(grepl("-", tablePOT$Date[1])) tablePOT$Date <- ymd_hms(tablePOT$Date)
    else tablePOT$Date <- dmy(tablePOT$Date)
  tablePOT$Flow <- as.numeric(tablePOT$Flow)
  tablePOT$Stage <- as.numeric(tablePOT$Stage)
  tablePOT$Station <- as.numeric(statno)
  tablePOT$WaterYear <- water_year(tablePOT$Date)
  if(getAmax != "none"){
    if(getAmax != "get"){
      amax <- read_amax(station = statno, loc_WinFapFiles = getAmax)
      names(amax) <- c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected")
      WY_table <- merge(amax,WY_table,all=TRUE)
      WY_table <- WY_table[,c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected","potPercComplete","potThreshold")]
      WY_table$Station <- statno
    }
    if(getAmax == "get"){
      amax <- get_amax(station = statno)
      names(amax) <- c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected")
      WY_table <- merge(amax,WY_table,all=TRUE)
      WY_table <- WY_table[,c("Station","WaterYear","amaxDate","amaxFlow","amaxStage","amaxRejected","potPercComplete","potThreshold")]
      WY_table$Station <- statno
    }
  }
  rownames(tablePOT) <- NULL
  out <- list(tablePOT = tablePOT[,c("Station","Date","WaterYear","Flow","Stage")],
              WaterYearInfo = WY_table,
              dateRange = dateRange)
  out
}


### AM ----

#' A function to read .AM files
#'
#' The function reads .AM files once these are in a local folder: these files contain information on annual maxima (AMAX) records extracted from the instantaneous river flow measurements.
#' The function checks for the presence of any [AM Rejected] information and includes it in the output.
#'
#' @param station NRFA station number(s) for which the .AM file (named \code{station.AM}) should be read.
#' @param loc_WinFapFiles the file.path of the WINFAP files, i.e. the location in which the
#' \code{station.AM} file can be found. Default is the working directory
#'
#' @return a data.frame with information on the annual maxima for the station with the following columns
#' \describe{
#'  \item{Station}{NRFA station number (can be a vector of station numbers)}
#'  \item{WaterYear}{the correct water year for the peak flow}
#'  \item{Date}{date of maximum flow}
#'  \item{Flow}{the maximum flow in m3/s}
#'  \item{Stage}{the stage (height) reached by the river - this information is used to derive the flow via a rating curve}
#'  \item{Rejected}{logical, if TRUE the water year has been flagged as rejected by the NRFA}
#' }
#' @seealso Information on the .AM files and river flow gauging in the UK can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}
#' @export
read_amax <- function(station, loc_WinFapFiles = getwd()){
  if(length(station) == 1){
    whereAM <- findfile(loc_WinFapFiles = loc_WinFapFiles,station = station,whichFile = ".am")
    if(length(whereAM) < 1) stop("Station does not have AMAX files in the loc_WinFapFiles folder")
    out <- read_amax_int(whereAM)
  }
  if(length(station) > 1) {
    whereAM <- lapply(X = as.list(station), findfile,loc_WinFapFiles=loc_WinFapFiles,whichFile = ".am")
    lwhere <- sapply(whereAM, length)
    if(all(lwhere < 1)){
      stop(paste("Stations do not have AMAX files in the loc_WinFapFiles folder"))
    }
    if(any(lwhere < 1)){
      warning(paste("Some stations do not have AMAX files in the loc_WinFapFiles folder:",station[lwhere < 1]," \n"))
      whereAM <- whereAM[lwhere > 0]
      station <- station[lwhere > 0]
    }
    out <- lapply(whereAM, read_amax_int)
    names(out) <- station
  }
  out
}

findfile <- function(station, loc_WinFapFiles, whichFile = ".am"){
  zerost <- station
  while(nchar(zerost) < 6) zerost <- paste0("0",zerost)
  list.files(loc_WinFapFiles,recursive=TRUE,
             pattern =paste0("^",station,toupper(whichFile),"|",
                             "^",station,tolower(whichFile),"|",
                             "^",zerost,toupper(whichFile),"|",
                             "^",zerost,tolower(whichFile)),full.names=TRUE)
}

read_amax_int <- function(filetext){
  rr <- readLines(filetext)
  rr <- rr[nchar(rr) > 0]
  station <- as.numeric(as.character(rr[(which(rr == "[STATION NUMBER]")+1)]))
  if((which(rr == "[AM Values]")+1) == length(rr)) stop(sprintf("no amax recorded at station %s", station), call. = FALSE)
  aa <- rr[(which(rr == "[AM Values]")+1):(length(rr)-1)]
  out <- cbind(station, utils::read.csv(textConnection(aa),header=FALSE, stringsAsFactors = FALSE))
  names(out) <- c("Station","Date","Flow","Stage")
  if(grepl("-", out$Date[1])) out$Date <- ymd_hms(out$Date)
    else out$Date <- dmy(out$Date)
  # out$Date <- lubridate::dmy(out$Date)
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


#' A function to read .CD3 files
#'
#' The function reads .CD3 files once these are in a local folder: these files contain information on the gauging station and on the catchment upstream the station.
#'
#' @param station the NRFA station number(s) for which the .CD3 file (names \code{station.CD3}) should be read
#' @param loc_WinFapFiles the file.path of the WINFAP files, i.e. the location in which the
#' \code{station.CD3} file can be found. Default is the working directory
#'
#' @return a data.frame with information on the catchment descriptors for the station
#' @seealso Information on the .CD3 files and river flow gauging in the UK can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}.
#' Specific information on the catchment descriptors can be found at \url{https://nrfa.ceh.ac.uk/feh-catchment-descriptors}
#' @export
#' @aliases read_cd3
read_cd3 <- function(station, loc_WinFapFiles = getwd()){
  if(length(station) == 1){
    whereCD <- findfile(loc_WinFapFiles = loc_WinFapFiles,station = station,whichFile = ".cd3")
    if(length(whereCD) < 1) stop("Station does not have CD3 files in the loc_WinFapFiles folder")
    out <- read_cd3_int(whereCD)
  }
  if(length(station) > 1) {
    whereCD <- lapply(X = as.list(station), findfile,loc_WinFapFiles=loc_WinFapFiles,whichFile = ".cd3")
    lwhere <- sapply(whereCD, length)
    if(all(lwhere < 1)){
      stop(paste("Stations do not have CD3 files in the loc_WinFapFiles folder"))
    }
    if(any(lwhere < 1)){
      warning(paste("Some stations do not have AMAX files in the loc_WinFapFiles folder:",station[lwhere < 1]," \n"))
      whereCD <- whereCD[lwhere > 0]
      station <- station[lwhere > 0]
    }
    out <- lapply(whereCD, read_cd3_int)
    names(out) <- station
  }
  out
}

read_cd3_int <- function(filetext){
  rr <- readLines(filetext)
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
  catchDesc <- data.frame(data.frame(matrix(rep("Nothing",3),ncol = 3),stringsAsFactors = FALSE),
                          data.frame(matrix(rep(-3,30),ncol = 30)),
                          stringsAsFactors = FALSE)
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


