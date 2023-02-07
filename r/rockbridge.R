# Rockbridge County data prep

library(tidyverse)
library(sf)
library(mapview)

# Import Rockbridge County parcel shapefile

rock_parcels <- st_read("data/rockbridge/ParcelsNovember2022.shp")

# Import Rockbridge County zoning shapefile

rock_zoning <- st_read("data/rockbridge/RockbridgeZoning20221003.shp")

mapview(rock_zoning)

