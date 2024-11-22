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
library(readxl)
library(janitor)

# Turn spherical geometry off
sf_use_s2(FALSE)


## Import parcel data -----------------

# Parcel assessment data

bv_assmt <- read_xlsx("data/buena-vista/bv-assessment-2023-01-31.xlsx") |> 
  
  # Clean up column names
  clean_names() |> 
  
  # Keep relevant columns only
  select(2:4, 13:17, 20:23) |> 
  
  # Remove all rows with non-valid map numbers
  filter(str_detect(mapnum, "^[0-9]"))

# Parcel shapefile

bv_parcels_raw <- st_read("data/buena-vista/Parcel20211201.shp") |>
  
  # Clean up column names
  clean_names() |> 
  
  # Remove streets and river
  filter(!objectid %in% c(3621, 3622, 3648, 3649)) |> 
  
  # Keep relevant columns only
  select(
    mapnum, acct_num, owner1, zoning, class, impvalue,
    landvalue, total_prop_val = total_prop_v,
    address, direction, street, deed_book, geometry
  ) |> 
  
  # Project to NAD83
  st_transform(4269)


## Join parcel data -------------------

# Parcel geos with valid map numbers

bv_parcels_valid <- bv_parcels_raw |> 
  filter(!is.na(mapnum))

# Parcel geos with non-valid map numbers

bv_parcels_na <- bv_parcels_raw |> 
  filter(is.na(mapnum)) |> 
  mutate(
    across(c(mapnum, zoning, class), ~ "NA")
  )

# Anti join to find mismatches

bv_join_anti <- bv_parcels_valid |> 
  select(mapnum, geometry) |> 
  anti_join(bv_assmt, by = "mapnum")

  # 1 mislabeled parcel that actually matches: "31- 1- - - 19"

# Fix mislabeled parcel

bv_parcels_valid <- bv_parcels_valid |> 
  mutate(
    mapnum = str_replace(mapnum, "31- 1-  -  -  19", "31- 1- - - 19")
  )

# Inner join to get full matches

bv_join_inner <- bv_parcels_valid |> 
  select(mapnum, geometry) |> 
  inner_join(bv_assmt, by = "mapnum")

# Add back bv_parcels_na rows

bv_join_all <- bv_join_inner |> 
  # Change class and address columns to text
  mutate(
    across(c(class, address), ~ as.character(.x))
  ) |>
  bind_rows(bv_parcels_na)


bv_join_clean <- bv_join_all |> 
  mutate(
    case_match(
      zoning,
      "B1MU" ~ "B1",
      "B-1MU" ~ "B1",
      "B-2" ~ "B2",
      "I1" ~ "INST",
      "LM &" ~ "LM",
      "MXB-H" ~ 
      "MXR-H" ~ "PUD-RES-HT"
      
    )
  )

bv_z <- bv_zoning |> 
  st_drop_geometry() |> 
  summarise(
    ct = n(),
    .by = Zoning_Cod
  )



## Import zoning data -----------------

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

## Prep parcels for analysis ----------

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
  

unique(bv_parcels_clean$ZONING)
# 
# mapview(bv_parcels_clean, zcol = "ZONING")

# Land use classes --------------------

1 = SF
2 = SF
3 = MF
4 = Commercial
5 = Vacant
6 = Vacant

71 = VA/US gov
74 = Park
76 = Church
77 = Instit
78 = Edu
79 = Other
  
## Parcel area

bv_area <- bv_parcels_clean_area |> 
  st_drop_geometry()


# Write Data --------------------------

write_rds(bv_zoning, "data/buena-vista/bv_zoning.rds")
write_rds(bv_parcels_clean_area, "data/buena-vista/bv_parcels.rds")
    