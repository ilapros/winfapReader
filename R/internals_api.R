
get_cd_int <- function(stid, fields){
  # Set a user agent
  ua <- "https://github.com/ilapros/winfaReader"
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
  resp <- httr2::request(root_entry_point) |>
    httr2::req_url_path_append("/station-info") |>
      httr2::req_user_agent(ua) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_error(is_error = function(resp) FALSE)|>
      httr2::req_perform()

  # Check response
  if(httr2::resp_is_error(resp)) {
    message(sprintf("maybe station %s does not exist", stid))
    return(NULL)
  }
  # Check output format
  if(httr2::resp_headers(resp)$`content-type` != "application/json") {
    message("API did not return json")
    return(NULL)
  }

  # Parse content
  page_content <- try(httr2::resp_body_json(resp, simplifyVector = TRUE))
  if(inherits(page_content,"try-error")) {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  parsed <- page_content$data
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
    ### depending on whether amax are missing or not data columns are read in in a different order, so ensure all outputs have the same columns
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
                        # "grid-reference.ngr", "grid-reference.easting", "grid-reference.northing",
                        # "lat-long.string", "lat-long.latitude", "lat-long.longitude",
                        "peak-flow-rejected-amax-years")]
  }
  parsed
}

# get_cd_int(39001, fields = "feh")


get_pot_int <-function(stid, getAmax){
  typeget <- ifelse(getAmax,"get","none")
  # Set a user agent
  ua <- "https://github.com/ilapros/winfaReader"
  ### construct call
  root_entry_point <- "https://nrfaapps.ceh.ac.uk/nrfa/ws/"
  params <- list(station=stid,
                 format="feh-data",
                 `data-type`="pot-flow")
  resp <- httr2::request(root_entry_point) |>
      httr2::req_url_path_append("time-series") |>
      httr2::req_user_agent(ua) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_error(is_error = function(resp) FALSE)|>
      httr2::req_perform()
  # Check response
  if(httr2::resp_is_error(resp)) {
    message(sprintf("maybe station %s does not exist", stid))
    return(NULL)
  }
  # Check output format
  resp <- resp
  if(httr2::resp_headers(resp)$`content-type` != "text/csv") {
    message("API did not return text", call. = FALSE)
    return(NULL)
  }
  page_content <- try(httr2::resp_body_string(resp, encoding = "UTF-8"))
  if(inherits(page_content,"try-error")) {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  read_pot_int(textConnection(page_content), getAmax = typeget)
}

# get_pot_int(71, getAmax= TRUE)

### zz <- httr::GET("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series/39001.am?format=feh-data&data-type=amax-stage", httr::write_disk(file.path("wffls","thef4.txt"), overwrite = TRUE))
### the code above downloads the files into the specified path
## could be an additional functionality - not for the moment
get_amax_int <-function(stid){
  # Set a user agent
  ua <- "https://github.com/ilapros/winfaReader"
  ### construct call
  root_entry_point <- "https://nrfaapps.ceh.ac.uk/nrfa/ws/"

  params <- list(station=stid,
                 format="feh-data",
                 `data-type`="amax-flow")
  resp <- httr2::request(root_entry_point) |>
        httr2::req_url_path_append("time-series") |>
        httr2::req_user_agent(ua) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_error(is_error = function(resp) FALSE)|>
        httr2::req_perform()
  # Check response
  if(httr2::resp_is_error(resp)) {
    message(sprintf("maybe station %s does not exist", stid))
    return(NULL)
  }
  # Check output format
  if(httr2::resp_headers(resp)$`content-type` != "text/csv") {
    message("API did not return text", call. = FALSE)
    return(NULL)
  }
  page_content <- try(httr2::resp_body_string(resp, encoding = "UTF-8"))
  if(inherits(page_content,"try-error")) {
    errs <- geterrmessage()
    message(paste("An unknwon error occurred when accessing the data - with error message:",errs))
    return(NULL)
  }
  read_amax_int(textConnection(page_content))
}

# get_amax_int(1001)



