# -------------------------------------
#
# Rockbridge County data prep
#
# -------------------------------------

# Load libraries ----------------------

library(tidyverse)
library(sf)
library(mapview)
library(units)
library(leaflet)

# Import data -------------------------

# Parcels
rock_parcels <- st_read("data/rockbridge/Rockbridge_ParcelsFebruary2023.shp") |> 
  filter(!str_detect(LABEL, "CITY"))

# Occupancy codes lookup table
rock_occ <- read_csv("data/rockbridge/rb-occupancy-codes.csv")

# Zoning
rock_zoning <- st_read("data/rockbridge/Rockbridge_Zoning20230126.shp")

# Future land uses
rock_suburb <- st_read("data/rockbridge/Rockbridge_SuburbanArea.shp")

rock_village <- st_read("data/rockbridge/Rockbridge_VillageAreas.shp")

rock_rural <- st_read("data/rockbridge/Rockbridge_RuralArea.shp")

rock_ruralvillage <- st_read("data/rockbridge/Rockbridge_RuralVillageAreas.shp")

# View data ---------------------------

rock_parcels |> filter(MOCCUP == 0) |> 
mapview()

# Calculate land use by area ----------

rock_landuse <- rock_parcels |> 
  select(LABEL, MOCCUP, Shape_Area) |> 
  left_join(rock_occ, by = join_by(MOCCUP == code)) |> 
  mutate(description = case_when(
    LABEL == "LEXINGTON CITY"
    LABEL == "GWNF" ~ "GWNF",
    str_detect(LABEL, "River") ~ "Water",
    
  ))

  st_drop_geometry() |> 
  group_by(category, description) |> 
  summarise(parcels = n(),
            area = sum(Shape_Area)) |> 
  ungroup() |> 
  mutate(parcels_pct = parcels/sum(parcels),
         area_pct = area/sum(area))






# Join parcels to zoning.

rock_parcels_z <- rock_parcels |> 
  st_join(left = FALSE, rock_zoning["ZONING"])

rock_parcels_lu <- rock_parcels_z |> 
  rowid_to_column("ID") |> 
  select(ID, OWNERNAME, MZONE, ZONING) |> 
  mutate(area = set_units(st_area(rock_parcels_z), "acre")) |> 
  filter(ZONING != "City/Town") |> 
  mutate(LU = case_when(
    str_detect(MZONE, "A") ~ "Agricultural",
    str_detect(MZONE, "R") ~ "Residential",
    str_detect(MZONE, "C") ~ "Conservation",
    str_detect(MZONE, "B") ~ "Business",
    str_detect(MZONE, "I") ~ "Industrial")) |> 
  st_drop_geometry() |> 
  group_by(LU) |> 
  summarise(area = sum(area),
            count = n_distinct(ID))

    
  #   MZONE == "A1" ~ "A-1",
  #   MZONE == "A2" ~ "A-2",
  #   MZONE =="A22" ~ "A-2",
  #   MZONE == "AG" ~ "A-1",
  #   MZONE == "AL" ~ "A-1",
  #   MZONE == "AT" ~ "A-T",
  #   MZONE == "B1" ~ "B-1",
  #   MZONE == "B2" ~ "B-2",
  #   MZONE == "RG" ~ "R-1",
  #   MZONE == "R" ~ "R-1",
  #   MZONE == "BG" ~ "B-1",
  #   MZONE == "C1" ~ "C-1",
  #   MZONE == "C-1" ~ "C-1",
  #   
  # mutate(match = case_when(
  #   ZONING == MZONE ~ "Match",
  #   ZONING != MZONE ~ "Mismatch"
  # )) |> 
  # select(match, zoning = ZONING, parcel_zoning = MZONE, OWNERNAME)

# Below dissolves polygons by zoning district and sums the acre field, which may be incorrect.

rock_zsimplified <- rock_zoning |> 
  group_by(ZONING) |> 
  summarise(acres = sum(ACRES)) |> 
  filter(ZONING != "City/Town") # Filter out the towns and cities.
  

# Below creates a column that calculates area based on the units set.

rock_zsimplified$area <- set_units(st_area(rock_zsimplified), "acre")

rock_zsimplified_pct <- rock_zsimplified |> 
  mutate(pct = area/sum(area))
  
mapview(rock_zsimplified, zcol = "ZONING")

rock_zmap <- rock_zsimplified |> st_transform(crs = 4326)

leaflet(rock_zmap) |> 
  addTiles() |> 
  addPolygons()

# Import Rockbridge County PSA features

rock_psa_layers <- st_layers("data/rockbridge/RockbridgePSA.gdb")

rock_water <- st_read(dsn = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Water_20130522")

rock_sewer <- st_read(ds,
                      n = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Sewer_20130522")

mapview(rock_water, color = "blue") +
  mapview(rock_sewer, color = "lightblue")  +
  mapview(rock_parcels, zcol = "MZONE")
