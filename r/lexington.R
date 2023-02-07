# Lexington City data prep

library(tidyverse)
library(sf)
library(mapview)

# Import Lexington City parcel shapefile

lex_parcels <- st_read("data/lexington/Lex_Parcels.shp")

# Import Lexington City zoning shapefile

lex_zoning <- st_read("data/exington/Zoning.shp")

mapview(lex_parcels)

