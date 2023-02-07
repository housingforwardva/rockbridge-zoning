# Buena Vista City data prep

library(tidyverse)
library(sf)
library(mapview)

# Import Buena Vista City parcel shapefile

bv_parcels <- st_read("data/buena-vista/Parcel20211201.shp")

# Import Buena Vista City zoning shapefile

bv_zoning <- st_read("data/buena-vista/Zoning20211201.shp")

mapview(bv_zoning)

