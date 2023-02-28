# -------------------------------------
#
# Buena Vista City data prep
#
# -------------------------------------

# Load libraries ----------------------

library(tidyverse)
library(sf)
library(mapview)

# Import data -------------------------

# Parcels

bv_parcels <- st_read("data/buena-vista/Parcel20211201.shp") |>
  
  # Remove streets and river
  
    filter(!OBJECTID %in% c(3621, 3622, 3648, 3649))

# Zoning

bv_zoning <- st_read("data/buena-vista/Zoning20211201.shp")

# View data ---------------------------

mapview(bv_parcels, zcol = "CLASS")

