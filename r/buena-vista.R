## -------------------------------------
##
## Buena Vista City data prep
##
## -------------------------------------

## Setup -------------------------------

# Load libraries
library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(units)
library(leaflet)

# Turn spherical geometry off
sf_use_s2(FALSE)

# Import data -------------------------

# Parcels
bv_parcels <- st_read("data/buena-vista/Parcel20211201.shp") |>
  
  # Remove streets and river
  filter(!OBJECTID %in% c(3621, 3622, 3648, 3649)) |> 
  
  # Keep only necessary columns
  select(1, 2, 6, 15:20) |> 
  
  # Project to NAD83
  st_transform(4269)

# Zoning
bv_zoning <- st_read("data/buena-vista/Zoning20211201.shp") |> 

  # Keep only necessary columns
  select(2, 4:5, 8) |>

  # Project to NAD83
  st_transform(4269)
  
# View data ---------------------------

mapview(bv_parcels, zcol = "CLASS")

## Prep parcels for analysis -----------

