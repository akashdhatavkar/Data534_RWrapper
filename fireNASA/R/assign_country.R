#' INTERNAL FUNCTION: Assign country name to coordinates of fireballs in a dataframe
#' @title assign_country
#' @param inp_df the input dataframe retrieved from the fireball API
#'
#' @return dataframe (same as input but with country name in new column)
#'
#' @import sf
#'
#'
#'

assign_country <- function(inp_df) {
  #Map data has already been prepped (can see script in the data-raw folder).
  #The planar-transformed version of the worldmap is saved as worldmap_planar.rda

  #Turn the fireball coordinates into numeric values
  #NOTE: If there NA's in the data, the resulting columns will have to be changed to numeric
  inp_df<-inp_df
  inp_df$lon_signed<- mapply(clean_latlon, inp_df$lon, inp_df$"lon-dir")
  inp_df$lon_signed <- as.numeric(inp_df$lon_signed)
  inp_df$lat_signed<- mapply(clean_latlon, inp_df$lat, inp_df$"lat-dir")
  inp_df$lat_signed <- as.numeric(inp_df$lat_signed)
  #Finally apply the function to find the country for the coordinates
  try(inp_df$country<-mapply(find_country, inp_df$lon_signed,inp_df$lat_signed))
  return(inp_df)

}


#' INTERNAL FUNCTION: Clean Latitude or Longitude values
#' @title clean_latlon
#' @param x coordinate point to process (can be latitude or longitude)
#' @param direction N/S for Latitude, E/W for Longitude
#'
#' @return numeric with correct sign applied relative to equator (south=negative) or prime meridan (west=negative)
#'
#'
#'
clean_latlon <- function(x, direction) {
  x <- as.numeric(x)
  if (is.na(x) | is.na(direction)){
    val <- abs(x)
  } else {
    val <- abs(x)
    if(direction=="S"|direction=="W") {
      val <- -abs(val)} #ignore whatever the original sign is, apply correct sign
  }
  return(val)

}

#' INTERNAL FUNCTION: Helper function for finding country of coordinate pair based on polygons
#' @title find_country
#' @param x longitude point (expected as numeric)
#' @param y latitude point (expected as numeric)
#'
#' @return character object representing country name (i.e. "Canada") or the ocean ("OCEAN"), or an unknown location ("UNKNOWN")
#'
#'
#'

find_country <- function(x,y) {
  #Handle missing coordinates by assigning UNKNOWN country
  if (is.na(x)|is.na(y)){
    result <- "UNKNOWN"
  } else {
    x<-as.numeric(x)
    y<-as.numeric(y)

    #Create a simple feature
    temp_pnt <- sf::st_point(c(x,y))
    #Create a simple feature geometry list column
    #Using EPSG:4326 which is the EPSG definition of WGS84
    temp_sfglc <- sf::st_sfc(temp_pnt, crs=4326)
    #Do a planar transform on the data to prep it for use in the st_intersects function
    temp_sfglc_planar <- sf::st_transform(temp_sfglc, 2163)

    #Find the polygon which the given coordinates intersect.
    country_value<-worldmap_planar[which(sf::st_intersects(temp_sfglc_planar, worldmap_planar, sparse = FALSE)), ]$NAME_LONG

    #If it didn't find a country, it must be out in an ocean. This could be improved with a more detailed map that has polygons for the oceans.
    if(length(country_value)==0){
      country_value<-"OCEAN"
    }
    result<-country_value
    #pnt_sf <- st_transform(st_sfc(st_point(c(x,y))))
  }
  return(result)
}
