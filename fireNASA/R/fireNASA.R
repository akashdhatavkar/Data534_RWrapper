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
#' @import httr jsonlite
#' @export
#'
#' @examples
#' fireball_data(date_min = "2015-01-01", max_energy = 0.6, lim = 10)
fireball_data <- function(date_min = NULL, date_max = NULL, min_energy = NULL, max_energy = NULL,
                          min_velocity = NULL, max_velocity = NULL, min_altitude = NULL, max_altitude = NULL
                          ,lim = NULL) {

  if(is.null(date_min)){
    date_min <- as.Date("1990-01-01", format = "%Y-%m-%d")
  }else{
    if(nchar(date_min) > 10){
      gsub(" %s", "",strptime(date_min, "%Y-%m-%d %H:%M:%S"))
    }else{
      date_min <- as.Date(date_min, "%Y-%m-%d")
    }
  }

  if(is.null(date_max)){
    date_max <- as.Date("2020-01-01", format = "%Y-%m-%d")
  }else{
    if(nchar(date_max) > 10){
      gsub(" %s", "",strptime(date_max, "%Y-%m-%d %H:%M:%S"))
    }else{
      date_max <- as.Date(date_max, "%Y-%m-%d")
    }
  }

  if(is.null(min_energy)){
    min_energy <- 0.1
  }

  if(is.null(max_energy)){
    max_energy <- 10
  }

  if(is.null(min_velocity)){
    min_velocity <- 1
  }

  if(is.null(max_velocity)){
    max_velocity <- 25
  }

  if(is.null(lim)){
    lim <- 1000
  }

  url <- httr::modify_url("https://ssd-api.jpl.nasa.gov/fireball.api")
  resp <- GET(url, query  = list(`date-min` = date_min, `date-max` = date_max, `energy-min` = min_energy,
                                 `energy-max` = max_energy, `vel-min` = min_velocity, `vel-max` = max_velocity,
                                 limit = lim))

  parsed <- jsonlite::fromJSON(content(resp,"text"))
  as <- as.data.frame(parsed[["data"]])
  colnames(as) <- parsed[["fields"]]
  return(as)
}

