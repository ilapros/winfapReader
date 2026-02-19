##### all these function are heavily inspired by the rnrfa::nrfa_api function
##### thanks to Claudia Vitolo to make those accessible



#' A function to obtain information on the station and on the catchment upstream of the station using the NRFA API
#'
#' The function queries the NRFA API for for information of a given station. Unlike \code{\link{get_amax}} and \code{\link{get_pot}}, the output of this function is not exactly the same from the output of the \code{\link{read_cd3}} function due to differences in the information made available by the NRFA API
#'
#' @param station the NRFA station(s) number for which the the information is required
#' @param fields the type of information which is required. Can be "feh" (default), which outputs a subset of information typically used when applying the flood estimation handbook methods, or "all", which output all information made available in the NRFA API.
#' @return a data.frame of one row with different columns depending on whether fields = "all" or fields = "feh" was selected.
#' @seealso \code{\link{read_cd3}}. Information on catchment descriptors river flow gauging in the UK can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}
#'
#' @examples
#'  cdMult <- get_cd(c(40003,42003), fields = "all")
#'  ### lots of information on the catchment/station
#'  ### including information on rejected annual maxima
#'  cdMult$`40003`$`peak-flow-rejected-amax-years` ## no rejections
#'  cdMult$`42003`$`peak-flow-rejected-amax-years` ## several rejections
#'  cd40003 <- get_cd(40003, fields = "feh")
#'  # less information, mostly the FEH descriptors
#'  dim(cd40003)
#'  sapply(cdMult, ncol)
#'
#' @export
get_cd <- function(station,fields = "feh"){
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("Package \"httr\" is needed for this function to work. Please install it or use the read_cd3 function after you have downloaded the winfap files at https://nrfa.ceh.ac.uk/peak-flow-dataset.")
    return(NULL)
  }
  if (!curl::has_internet()){message("There appears to be no internet connection"); return(NULL)}

  id <- station
  if(length(id) == 1) out <- get_cd_int(stid=id, fields = fields)
  if(length(id) > 1) {
    out <- lapply(X = as.list(id), FUN = get_cd_int, fields = fields)
    names(out) <- station
    out <- out[!sapply(out,is.null)]
  }
  return(out)
}


######### getting amax from api -----

#' A function to obtain annual maxima (AMAX) data using the NRFA API
#'
#' The function queries the NRFA API for the .AM file similar to the WINFAP file for a given stations. It then processes the file in a fashion similar to \code{read_amax}.
#'
#' @param station the NRFA station number for which the annual maxima records should be obtained. Can also be a vector of station numbers.
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
#' @seealso \code{\link{read_amax}}. Information on river flow gauging in the UK and the annual maxima can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}
#' @examples
#'   a40003 <- get_amax(40003) # the Medway at Teston / East Farleigh
#'   multipleStations <- get_amax(c(40003, 42003))
#'   names(multipleStations)
#'   summary(multipleStations$`42003`)
#' @export
get_amax <- function(station){
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("Package \"httr\" is needed for this function to work. Please install it or use the read_amax function after you have downloaded the winfap files at https://nrfa.ceh.ac.uk/peak-flow-dataset.",
         call. = FALSE)
    return(NULL)
  }
  if (!curl::has_internet()){message("There appears to be no internet connection"); return(NULL)}
  id <- station
  if(length(id) == 1) out <- get_amax_int(stid=id)
  if(length(id) > 1) {
    out <- lapply(X = as.list(id), FUN = get_amax_int)
    names(out) <- station
    out <- out[!sapply(out,is.null)]
  }
  return(out)
}


######  getting POTs from API -------

#' A function to obtain Peaks-Over-Threshold (POT) data using the NRFA API
#'
#' The function queries the NRFA API for the .PT file similar to the WINFAP file for a given stations. It then processes the file in a fashion similar to \code{\link{read_pot}}.
#'
#' @param station the NRFA station number for which peaks over threshold information should be obtained. It can also be a vector of station numbers
#' @param getAmax logical. If \code{TRUE} information on the annual maxima values will be retrieved and attached to the \code{WaterYearInfo} table
#'
#' @return Like \code{\link{read_pot}} a list of three objects \code{tablePOT}, \code{WaterYearInfo} and \code{dateRange}.
#' @return \code{tablePOT} contains a table with all the peaks above the threshold present in the record
#' @return \code{WaterYearInfo} a table containing the information on the percentage of missing values
#' in any water year for which some data is available in the POT record. This is useful to assess
#' whether the lack of exceedances is genuine or the result of missing data and to assess whether the threshold
#' exceedances present in \code{tablePOT} can be deemed to be representative of the whole year
#' @return \code{dateRange} a vector with the first and last date of recording for the POT record as provided in the [POT Details] field.
#' Note that this period might be different than the period for which annual maxima records are available
#' @seealso \code{\link{read_pot}}. Information on the peaks over threshold records and river flow gauging in the UK can be found at the National River Flow Archive website \url{https://nrfa.ceh.ac.uk}
#'
#' @examples
#' \dontrun{
#'   ### the example take longer than 5 seconds to run
#'   p40003 <- get_pot(40003) # the Medway at Teston / East Farleigh
#'   p40003$tablePOT[p40003$tablePOT$WaterYear > 1969 &
#'         p40003$tablePOT$WaterYear < 1977,]
#'   ### no events in 1971 nor 1975
#'   p40003$WaterYearInfo[p40003$WaterYearInfo$WaterYear > 1969 &
#'         p40003$WaterYearInfo$WaterYear < 1977,]
#'   # in 1971 all records are valid,
#'   # in 1975 no exceedances
#'   # might be due to the fact that almost no valid record are available
#'
#'   p40003 <- get_pot(40003, getAmax = TRUE)
#'   p40003$WaterYearInfo[p40003$WaterYearInfo$WaterYear > 1969 &
#'        p40003$WaterYearInfo$WaterYear < 1977,]
#'   # the annual maximum in 1971 and 1975 was below the threshold
#'   # no events exceeded the threshold
#' }
#' @export
get_pot <- function(station, getAmax = FALSE){
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("Package \"httr\" is needed for this function to work. Please install it or use the read_pot function after you have downloaded the winfap files at https://nrfa.ceh.ac.uk/peak-flow-dataset.")
    return(NULL)
  }
  if (!curl::has_internet()){message("There appears to be no internet connection"); return(NULL)}
  id <- station
  if(length(id) == 1) out <- get_pot_int(stid=id, getAmax = getAmax)
  if(length(id) > 1) {
    out <- lapply(X = as.list(id), FUN = get_pot_int, getAmax = getAmax)
    names(out) <- station
    out <- out[!sapply(out,is.null)]
  }
  return(out)
}



