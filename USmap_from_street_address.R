# we will map the street address on the US map using mapbox and leaflet
# we will use the street address to get the latitude and longitude
# read in the street address

library(tidyverse)
library(mapboxapi)
# setup mapboxapi
# you will need to get an api from mapbox.com if you want to run this your self
# and you will need to set the token = "your token"
source("token.R")
mapboxapi::mb_access_token(token,install = TRUE)

library(readxl)
library(htmlwidgets)
address<- read_excel("streetaddress_tomap.xlsx")

# get the latitude and longitude from the street address using map box
# write a function to get latitude and longitude from Mapbox api
get_geocoding <- function(i) {
  NCORP_name = address$NCORP[i]
  street_adress = address$Address[i]
  lat_long = mb_geocode(street_adress,access_token = token)
  longitude = lat_long[1]
  latitude = lat_long[2]

  
  mapdata = bind_cols(
    'NCORP_name' = NCORP_name,
    'street_adress' = street_adress,
    # 'lat_long' = lat_long,
    'lat' = latitude,
    'lon'=longitude
  )
return(mapdata)
}

# loop through the list of addresses
results = map((1:nrow(address)),get_geocoding)

# combine the results
map_df = results %>% 
  bind_rows() %>%
  as.data.frame()

# save RDS to play later
map_df %>% saveRDS(.,"map_df.rds")

# load the RDS file to plot
map_df = readRDS("map_df.rds")
# now create map use mapbox 
library(plotly)

# will need to run from there every time when changing parameter below
fig <- map_df
# define basic map
fig <- fig %>%
  plot_ly(
    lat = ~lat,
    lon = ~lon,
    mode = "markers",
    marker = list(color = "blue",size = 10),
    # marker = {'size': 10},
    type = 'scattermapbox',
    hovertext = map_df[,"NCORP_name"] ) 

# add layout and base-map
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -88, lat = 34))) 
# display the map
fig
# save the map for future use
saveWidget(fig, "map.html", selfcontained = T, libdir = "lib")
