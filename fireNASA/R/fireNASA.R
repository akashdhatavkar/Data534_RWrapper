##----- Creating Function for API Get Request -----##
#' @title fireball_data
#'
#' @description Wrapper for downloading NASA's Fireball Data
#'
#' @param lim Number of Rows needed
#' @param min_e Exclude Data with Total-Radiated-Energy less than this positive value
#'
#' @import httr jsonlite
#' @export
#'
#' @examples
#' fireball_data(lim = 10, min_e = 0.6)
fireball_data <- function(lim, min_e = NULL) {
  url <- httr::modify_url("https://ssd-api.jpl.nasa.gov/fireball.api")
  if(is.null(min_e)){
    resp <- GET(url, query  = list(limit = lim,`energy-min` = 0.6))
  }else{
    resp <- GET(url, query  = list(limit = lim,`energy-min` = min_e))
  }

  parsed <- jsonlite::fromJSON(content(resp,"text"))
  as <- as.data.frame(parsed[["data"]])
  colnames(as) <- parsed[["fields"]]
  return(as)
}
