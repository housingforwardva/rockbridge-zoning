## -------------------------------------
##
## Buena Vista City data prep
##
## -------------------------------------


## SETUP -------------------------------

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

# Lead get_dominant_zone() fn

source("r/fns/get-dom-zone.R")

## ZONING -----------------------------

# Import zoning data

bv_zoning_raw <- st_read("data/buena-vista/Zoning20211201.shp") |> 
  
  # Keep only necessary columns
  select(zoning = 4, desc = 5, comment = 8,  11) |>
  
  # Fix R4 description
  mutate(
    desc = case_match(
      zoning,
      "R4" ~ "Medium Density Residential",
      .default = desc
    )
  ) |> 
  
  # Project to NAD83
  st_transform(4269)

# Make summary table

bv_zoning_tbl <- bv_zoning_raw |> 
  st_drop_geometry() |> 
  summarise(
    n = n(),
    .by = c(zoning, desc)
  ) |> 
  arrange(zoning)


## PARCELS ----------------------------

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

# Parcel geos with valid map numbers

bv_parcels_valid <- bv_parcels_raw |> 
  filter(!is.na(mapnum))

# Parcel geos with non-valid map numbers

bv_parcels_na <- bv_parcels_raw |> 
  filter(is.na(mapnum)) |> 
  mutate(
    across(c(mapnum, class), ~ "NA"),
    address = as.character(address)
  ) |> 
  select(-zoning)

# Get zoning from spatial join

bv_parcels_na_fix <- get_dominant_zone(
  bv_parcels_na,
  zoning = select(bv_zoning_raw, zoning, geometry)
) |> 
  select(1:3, 12, 3:11, 13)

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
  bind_rows(bv_parcels_na_fix)


## Clean up parcel data ---------------

bv_multi <- bv_join_all |> 
  filter(owner1 == "REHL DONALD A ET UX") |> 
  st_cast("POLYGON") |> 
  mutate(id = row_number()) |> 
  mutate(
    zoning = case_when(
      id == 4 ~ "GM",
      .default = "PUD-R6"
    )
  ) |> 
  st_cast("MULTIPOLYGON") |> 
  select(-id)

bv_join_clean <- bv_join_all |>
  mutate(
    zoning = case_match(
      zoning,
      "B-1" ~ "B1",
      "B1MU" ~ "B1",
      "B-1MU" ~ "B1",
      "B-2" ~ "B2",
      "C-1" ~ "C1",
      "I1" ~ "INST",
      "LM &" ~ "LM",
      "MUB-1" ~ "MU",
      "MUR-3" ~ "MU",
      "MXB-H" ~ "MXB-HT",
      "MXR" ~ "PUD-RES-HT",
      "MXR-H" ~ "PUD-RES-HT",
      "PUD" ~ "PUD-R6",
      "R-1" ~ "R1",
      "R-2" ~ "R2",
      "R-3" ~ "R3",
      "R-4" ~ "R4",
      "R-5" ~ "R3",
      "R-6" ~ "PUD-R6",
      .default = zoning
    )
  ) |> 
  mutate(
    zoning = case_when(
      deed_book == "RC 306/219" ~ "REC",
      deed_book == "24 373" ~ "C1",
      mapnum == "8- 1-  -  -   -   5A" ~ "GM",
      mapnum == "3- 1- 4-  -   A" ~ "C1",
      owner1 == "SOUTHERN VIRGINIA COLLEGE" ~ "INST",
      owner1 == "SOUTHERN VIRGINIA UNIVERSITY" ~ "INST",
      owner1 == "SOUTHERN SEMINARY JUNIOR" ~ "INST",
      owner1 == "UNITED STATES OF AMERICA" ~ "INST",
      owner1 == "GREENHILL CEMETERY" ~ "C1",
      .default = zoning
    )
  ) |> 
  mutate(id = row_number(), .before = 1) |>
  filter(!id %in% c(2940, 2945)) |> 
  select(-id) |> 
  bind_rows(bv_multi)

# bv_join_clean |> filter() |> mapview(zcol = "zoning")
  
bv_c <- bv_join_clean |>
  st_drop_geometry() |>
  summarise(
    ct = n(),
    .by = class
  )


## Prep parcels for analysis ----------

# Define land uses
bv_parcels_class <- bv_join_clean |> 
  mutate(
    class = case_match(
      class,
      "1" ~ "Single-family",
      "2" ~ "Single-family",
      "3" ~ "Multifamily",
      "4" ~ "Commercial",
      "5" ~ "Vacant",
      "6" ~ "Vacant",
      "71" ~ "State/federal government",
      "74" ~ "Park",
      "76" ~ "Church",
      "77" ~ "Institutional",
      "78" ~ "Education",
      "79" ~ "Other",
      "NA" ~ "No data"
    )
  )

bv_parcels_area <- bv_parcels_class |> 
  mutate(
    area = as.numeric(
      st_area(bv_parcels_class) / 4047
    ),
    .before = 13
  )



# Write Data --------------------------

bv_parcels_area |> 
  write_rds("data/buena-vista/bv_parcels.rds")

bv_zoning_raw |> 
  write_rds("data/buena-vista/bv_zoning.rds")
    