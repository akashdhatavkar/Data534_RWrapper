##----- Creating Function for API Get Request -----##
#' @title fireball_data
#'
#' @description Wrapper for downloading NASA's Fireball Data
#'
#' @param date_min Exclude data earlier than this date YYYY-MM-DD or date/time YYYY-MM-DDThh:mm:ss
#' @param date_max Exclude data later than this date YYYY-MM-DD or date/time YYYY-MM-DDThh:mm:ss
#' @param min_energy Exclude data with Total-Radiated-Energy less than this positive value in joules (e.g.,0.3)
#' @param max_energy Exclude data with Total-Radiated-Energy less than this positive value
#' @param min_velocity Exclude data with velocity-at-peak-brightness less than this positive value in km/s (e.g., 18.5)
#' @param max_velocity Exclude data with velocity-at-peak-brightness greater than this positive value in km/s (e.g., 20)
#' @param lim Number of rows to be displayed
#'
#' @import httr jsonlite sf chron
#' @export
#' @usage fireball_data(date_min, date_max, min_energy, max_energy,
#'                min_velocity, max_velocity, lim)
#' @examples
#' fireball_data(date_min = "2015-01-01", min_energy = 0.6,
#'                lim = 10)

##----- Default parameters are set to NULL, incase does not want to enter all the fields -----##
fireball_data <- function(date_min = NULL, date_max = NULL, min_energy = NULL, max_energy = NULL,
                          min_velocity = NULL, max_velocity = NULL, lim = NULL) {

##------ In case any of the parameters have no input, setting some default values to run query -----##
  if(is.null(date_min)){
    date_min <- as.Date("1900-01-01", format = "%Y-%m-%d")
  }else{
    if(nchar(date_min) > 10){
      gsub(" %s", "",strptime(date_min, "%Y-%m-%d %H:%M:%S"))
    }else{
      date_min <- as.Date(date_min, "%Y-%m-%d")
    }
  }

  if(is.null(date_max)){
    date_max <- as.Date("2050-01-01", format = "%Y-%m-%d")
  }else{
    if(nchar(date_max) > 10){
      gsub(" %s", "",strptime(date_max, "%Y-%m-%d %H:%M:%S"))
    }else{
      date_max <- as.Date(date_max, "%Y-%m-%d")
    }
  }

  if(is.null(min_energy)){
    min_energy <- "0.1"
  }

  if(is.null(max_energy)){
    max_energy <- "1000"
  }

   if(is.null(lim)){
     lim <- 1000
   }


  url <- httr::modify_url("https://ssd-api.jpl.nasa.gov/fireball.api")
  if(is.null(min_velocity) & is.null(max_velocity)){
    resp <- httr::GET(url, query  = list(`date-min` = date_min, `date-max` = date_max, `energy-min` = min_energy,
                                         `energy-max` = max_energy, limit = lim))
  }else if(!is.null(min_velocity)){
    resp <- httr::GET(url, query  = list(`date-min` = date_min, `date-max` = date_max, `energy-min` = min_energy,
                                 `energy-max` = max_energy, `vel-min` = min_velocity, limit = lim))
  }else if(!is.null(max_velocity)){
    resp <- httr::GET(url, query  = list(`date-min` = date_min, `date-max` = date_max, `energy-min` = min_energy,
                                         `energy-max` = max_energy, `vel-max` = max_velocity,
                                         limit = lim))
  }

##----- Check if request was correct -----##
  if(resp$status_code == 400){
    return("The request contained invalid keywords and/or content: details returned")
  }else if(resp$status_code == 405){
    return("The request used a method other than GET or POST")
  }else if(resp$status_code == 500){
    return("The database is not available at the time of the request")
  }else if(resp$status_code == 503){
    return("The server is currently unable to handle the request due to a temporary overloading or maintenance
    of the server, which will likely be alleviated after some delay")
  }

  parsed <- jsonlite::fromJSON(httr::content(resp,"text", encoding = "UTF-8"))

##----- Check if API version is 1.0 -----##
  if(parsed$signature[["version"]] != "1.0"){
    print("The API version version does not match the version in the document,
          there is no guarantee that the format has not changed!")
  }

  resultdf <- as.data.frame(parsed[["data"]])

  ##----- If no Data returned. Exit Function and ask user to Change Parameters -----##
  if(nrow(resultdf) == 0){
    return("Please Change Input Parameters as there is no data matching Input Parameters!")
  }

  colnames(resultdf) <- parsed[["fields"]]
  resultdf <- na.omit(resultdf)
  #convert any factors to characters
  inds <- sapply(resultdf, is.factor)
  resultdf[inds] <- lapply(resultdf[inds], as.character)
  resultdf <- assign_country(resultdf)
  resultdf$date1 <- as.Date(resultdf$date, "%Y-%m-%d")
  resultdf$time <- format(as.POSIXct(strptime(resultdf$date,"%Y-%m-%d %H:%M",tz=""))
                  ,format = "%H:%M")

  resultdf$date <- resultdf$date1
  resultdf$date1 <- NULL
  resultdf <- resultdf[,c("date", "time","energy", "impact-e", "lat", "lat-dir", "lon", "lon-dir", "alt",
                          "vel", "lon_signed",	"lat_signed", "country")]
  resultdf$time <- chron::chron(times = paste0(resultdf$time,":00"))
  resultdf$energy <- as.numeric(resultdf$energy)
  resultdf$`impact-e` <- as.numeric(resultdf$`impact-e`)
  resultdf$lat <- as.numeric(resultdf$lat)
  resultdf$lon <- as.numeric(resultdf$lon)
  resultdf$alt <- as.numeric(resultdf$alt)
  resultdf$vel <- as.numeric(resultdf$vel)
  resultdf$lon_signed <- as.numeric(resultdf$lon_signed)
  resultdf$lat_signed <- as.numeric(resultdf$lat_signed)
  return(resultdf)
}

