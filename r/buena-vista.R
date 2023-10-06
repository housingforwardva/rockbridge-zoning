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
library(lwgeom)

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
  st_transform(4269) |> 
  
  # Clean up zoning abbreviations
  mutate(ZONING = case_when(
    Zoning_Cod == "R1" ~ "R-1",
    Zoning_Cod == "R2" ~ "R-2",
    Zoning_Cod == "R3" ~ "R-3",
    Zoning_Cod == "R4" ~ "R-4",
    Zoning_Cod == "R5" ~ "R-5",
    Zoning_Cod == "R6" ~ "R-6",
    Zoning_Cod == "B1" ~ "B-1",
    Zoning_Cod == "B2" ~ "B-2",
    TRUE ~ Zoning_Cod
  ))
  
# View data ---------------------------

mapview(bv_parcels, zcol = "ZONING")

## Prep parcels for analysis -----------

bv_parcels_clean <- bv_parcels |> 
  mutate(ZONING = case_when(
    ZONING == "B-1MU" ~ "B-1/MU",
    ZONING == "B1MU" ~ "B-1/MU",
    ZONING == "I1" ~ "INST",
    ZONING == "LM &" ~ "LM",
    ZONING == "MUB-1" ~ "B-1/MU",
    ZONING == "MUR-3" ~ "R-3/MU",
    ZONING == "MXB-H" ~ "MXB-HT",
    ZONING == "R1" ~ "R-1",
    ZONING == "R2" ~ "R-2",
    ZONING == "R3" ~ "R-3",
    ZONING == "MXR" ~ "PUD-RES-HT",
    ZONING == "MXR-H" ~ "PUD-RES-HT",
    TRUE ~ ZONING)) 

bv_parcels_clean$area <- as.numeric(sf::st_area(bv_parcels_clean))

bv_parcels_clean_area <- bv_parcels_clean |> 
  mutate(acres = area*0.00024710538146717)

# unique(bv_parcels_clean$ZONING)
# 
# mapview(bv_parcels_clean, zcol = "ZONING")


## Parcel area

bv_area <- bv_parcels_clean_area |> 
  st_drop_geometry()


# Write Data ---------------------------

write_rds(bv_zoning, "data/buena-vista/bv_zoning.rds")
write_rds(bv_parcels_clean_area, "data/buena-vista/bv_parcels.rds")
    