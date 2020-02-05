#Prep map data by loading and transforming to planar (for later use with st_intersects)
#Data is from Natural Earth. Free vector and raster map data @ naturalearthdata.com.
#More information can be found in the included ne_110m_admin_0_countries.README.html file

library(sf)

worldmap_raw <- read_sf("data-raw/mapdata/ne_110m_admin_0_countries.shp")
worldmap_planar <- st_transform(worldmap_raw, 2163)

usethis::use_data(worldmap_planar)
