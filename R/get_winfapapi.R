##### all these function are heavily inspired by the rnrfa::nrfa_api function
##### thanks to Claudia Vitolo to make those accessible

get_cd_int <- function(stid, fields){
  # Set a user agent
  ua <- httr::user_agent("https://github.com/ilapros/winfaReader")
  ### construct call
  root_entry_point <- "https://nrfaapps.ceh.ac.uk/nrfa/ws/"
  url <- paste0(root_entry_point, "station-info")
  if(!fields %in% c("feh","all")) {
    warning("fields must be either \"feh\" or \"all\" - defaulting to feh")
    fields <- "feh"
  }
  params <- list(station=stid,
                 format="json-object",
                 fields=switch(fields,
                               "feh" =
                               "id,river,location,spatial-location,feh-pooling,feh-qmed,feh-neither,benchmark,feh-descriptors,hydrometric-area,qmed",
                               "all" = "all"))
  resp <- httr::GET(url = url, query = params, ua)
  # Check response
  if (httr::http_error(resp)) {
    if(resp$status_code == 400) message(sprintf("maybe station %s does not exist", stid))
    # stop(sprintf("NRFA API request failed [%s]", httr::status_code(resp)),
    #              call. = FALSE)
    return(NULL)
  }

  # Check output format
  if (httr::http_type(resp) != "application/json") {
    message("API did not return json")
    return(NULL)
  }

  # Parse content
  page_content <- try(httr::content(resp, "text", encoding = "UTF-8"), silent=TRUE)
  if(class(page_content) == "try-error") {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  parsed <- jsonlite::fromJSON(page_content, simplifyDataFrame = TRUE, flatten = TRUE)$data
  #
  if(fields == "all") {
    ## the information for rejected periods is not reliable for POT and redundant for AMAX
    ## best to drop it
    parsed <- parsed[,grep(pattern = "peak-flow-rejected-periods",
                           names(parsed),invert = TRUE)]
    ## rejected years as a unique character object to make stations easier to stack
    miss <- paste(parsed[,grep(pattern = "peak-flow-rejected-amax-years",names(parsed))][[1]], collapse = ", ")
    parsed[,grep(pattern = "peak-flow-rejected-amax-years",names(parsed))] <- miss
    names(parsed)[grep(pattern = "peak-flow-rejected-amax-years",names(parsed))] <- "peak-flow-rejected-amax-years"
    rm(miss)
    ### depening on whether amax are missing or not data columns are read in in a different order, so ensure all outputs have the same columns
    parsed <- parsed[,c("id", "name", "catchment-area", "river", "location", "station-level",
                        "measuring-authority-id", "measuring-authority-station-id", "hydrometric-area",
                        "opened", "closed", "station-type", "bankfull-flow", "structurefull-flow",
                        "sensitivity", "nrfa-mean-flow", "nrfa-peak-flow", "feh-pooling",
                        "feh-qmed", "feh-neither", "nhmp", "benchmark", "live-data",
                        "factors-affecting-runoff", "gdf-start-date", "gdf-end-date",
                        "gdf-mean-flow", "gdf-min-flow", "gdf-first-date-of-min", "gdf-last-date-of-min",
                        "gdf-max-flow", "gdf-first-date-of-max", "gdf-last-date-of-max",
                        "gdf-q95-flow", "gdf-q70-flow", "gdf-q50-flow", "gdf-q10-flow",
                        "gdf-q05-flow", "gdf-base-flow-index", "gdf-day-count", "gdf-flow-count",
                        "gdf-percent-complete", "peak-flow-start-date", "peak-flow-end-date",
                        "qmed", "minimum-altitude", "10-percentile-altitude", "50-percentile-altitude",
                        "90-percentile-altitude", "maximum-altitude", "saar-1941-1970",
                        "saar-1961-1990", "lcm2000-woodland", "lcm2000-arable-horticultural",
                        "lcm2000-grassland", "lcm2000-mountain-heath-bog", "lcm2000-urban",
                        "lcm2007-woodland", "lcm2007-arable-horticultural", "lcm2007-grassland",
                        "lcm2007-mountain-heath-bog", "lcm2007-urban", "high-perm-bedrock",
                        "moderate-perm-bedrock", "low-perm-bedrock", "mixed-perm-bedrock",
                        "high-perm-superficial", "low-perm-superficial", "mixed-perm-superficial",
                        "propwet", "bfihost", "farl", "dpsbar", "sprhost", "rmed-1d",
                        "rmed-2d", "rmed-1h", "ldp", "dplbar", "altbar", "aspbar", "aspvar",
                        "ihdtm-height", "ihdtm-catchment-area", "mean-flood-plain-depth",
                        "mean-flood-plain-location", "mean-flood-plain-extent", "urbext-1990",
                        "urbconc-1990", "urbloc-1990", "urbext-2000", "urbconc-2000",
                        "urbloc-2000", "easting", "northing", "latitude", "longitude",
                        "grid-reference.ngr", "grid-reference.easting", "grid-reference.northing",
                        "lat-long.string", "lat-long.latitude", "lat-long.longitude",
                        "peak-flow-rejected-amax-years")]
  }
  parsed
}




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

### zz <- httr::GET("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series/39001.am?format=feh-data&data-type=amax-stage", httr::write_disk(file.path("wffls","thef4.txt"), overwrite = TRUE))
### the code above downloads the files into the specified path
## could be an additional functionality - not for the moment
get_amax_int <-function(stid){
  # Set a user agent
  ua <- httr::user_agent("https://github.com/ilapros/winfaReader")
  ### construct call
  root_entry_point <- "https://nrfaapps.ceh.ac.uk/nrfa/ws/"
  url <- paste0(root_entry_point, "time-series")

  params <- list(station=stid,
                 format="feh-data",
                 `data-type`="amax-flow")
  resp <- httr::GET(url = url, query = params, ua)
  # Check response
  if (httr::http_error(resp)) {
    if(resp$status_code == 400) message(sprintf("maybe station %s does not exist", stid))
    # stop(sprintf("NRFA API request failed [%s]", httr::status_code(resp)),
    #      call. = FALSE)
    return(NULL)
  }
  # Check output format
  if (httr::http_type(resp) != "text/csv") {
    message("API did not return text", call. = FALSE)
    return(NULL)
  }
  page_content <- try(httr::content(resp, "text", encoding = "UTF-8"))
  if(class(page_content) == "try-error") {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  read_amax_int(textConnection(page_content))
}


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
get_pot_int <-function(stid, getAmax){
  typeget <- ifelse(getAmax,"get","none")
  # Set a user agent
  ua <- httr::user_agent("https://github.com/ilapros/winfaReader")
  ### construct call
  root_entry_point <- "https://nrfaapps.ceh.ac.uk/nrfa/ws/"
  url <- paste0(root_entry_point, "time-series")

  params <- list(station=stid,
                 format="feh-data",
                 `data-type`="pot-flow")
  resp <- httr::GET(url = url, query = params, ua)
  # Check response
  if (httr::http_error(resp)) {
    if(resp$status_code == 400) message(sprintf("maybe station %s does not exist", stid))
    # stop(sprintf("NRFA API request failed [%s]", httr::status_code(resp)),
    #      call. = FALSE)
    return(NULL)
  }
  # Check output format
  if (httr::http_type(resp) != "text/csv") {
    message("API did not return text", call. = FALSE)
    return(NULL)
  }
  page_content <- try(httr::content(resp, "text", encoding = "UTF-8"))
  if(class(page_content) == "try-error") {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  read_pot_int(textConnection(page_content), getAmax = typeget)
}



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
#' \donttest{
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



