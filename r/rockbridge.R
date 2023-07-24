## -------------------------------------
##
## Rockbridge County data prep
##
## -------------------------------------

## Load libraries ----------------------

library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(units)
library(leaflet)

## Import data -------------------------

# Parcels

rock_parcels <- st_read("data/rockbridge/Rockbridge_ParcelsFebruary2023.shp") |> 
  
  # Remove Lexington, Buena Vista, and river polygons
  
  filter(!str_detect(LABEL, "CITY|River"))

# Occupancy codes lookup table

rock_occ <- read_csv("data/rockbridge/rb-occupancy-codes.csv")

# Zoning

rock_zoning <- st_read("data/rockbridge/Rockbridge_Zoning20230126.shp")

# Future land uses

rock_suburb <- st_read("data/rockbridge/Rockbridge_SuburbanArea.shp")
rock_village <- st_read("data/rockbridge/Rockbridge_VillageAreas.shp")
rock_rural <- st_read("data/rockbridge/Rockbridge_RuralArea.shp")
rock_ruralvillage <- st_read("data/rockbridge/Rockbridge_RuralVillageAreas.shp")

# Utility coverage

rock_psa_layers <- st_layers("data/rockbridge/RockbridgePSA.gdb")
rock_water <- st_read(dsn = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Water_20130522")
rock_sewer <- st_read(dsn = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Sewer_20130522")

# Census boundaries

rock_outline <- counties("51") |> 
  filter(NAME == "Rockbridge")

## View data ---------------------------

mapview(rock_outline) + mapview(rock_zoning)

## Create town masks -------------------

rock_towns <- rock_zoning |> 
  filter(ZONING == "City/Town") |> 
  mutate(name = case_when(
    OBJECTID_1 == 190 ~ "Glasgow",
    OBJECTID_1 == 3528 ~ "Goshen"
  )) |> 
  drop_na(name) |> 
  select(name, ZONING, ACRES)

## Prep parcels for analysis -----------

# Assign land uses to parcels; keep zoning field

rock_landuse <- rock_parcels |> 
  
  # Drop duplicate polygons
  distinct(geometry, .keep_all = TRUE) |> 
  
  # Keep only relevant columns
  select(LABEL, OWNERNAME, MOCCUP,
         MZONE, CALCACRES, Shape_Area) |>  
  
  # Join with county parcel occupancy codes
  left_join(rock_occ, by = join_by(MOCCUP == code)) |> 
  
  # Classify publicly-owned parcels
  mutate(group = case_when(
    str_detect(LABEL, "GWNF") ~ "Federal land", # George Washington National Forest
    str_detect(OWNERNAME, "GEO WASH") ~ "Federal land", # George Washington National Forest
    str_detect(OWNERNAME, "JEFFERSON") ~ "Federal land", # Jefferson National Forest
    str_detect(LABEL, "Parkway") ~ "Federal land", # Blue Ridge Parkway
    str_detect(LABEL, "85-A-23|85-A-24|94-8-A") ~ "State land", # Short Hills WMA
    str_detect(LABEL, "21-A-11|15-99-A") ~ "State land", # Goshen - Little North Mountain WMA
    str_detect(LABEL, "57-A-37") ~ "State land", # Lake Robertson WMA
    LABEL == "83-A-1" ~ "State land", # Moore's Creek State Forest
    str_detect(LABEL, "23-A-8A") ~ "State land", # Goshen Pass Natural Area Preserve
    str_detect(LABEL, "COMMONWEALTH|GAME") ~ "State land", # Other state-owned parcels
    LABEL %in% c("83-A-1B", "83-A-3") ~ "City property", # Lexington Reservoir
    str_detect(LABEL, "74-A-1") ~ "City property", # Bushy Hill Preserve
    str_detect(OWNERNAME, "COUNTY") ~ "County property", # All county-owned parcels
    TRUE ~ group
  )) |> 
  
  # Create new public property category
  mutate(category = case_when(
    str_detect(group, "Federal|State|City|County") ~ "Public property",
    TRUE ~ category
  ))

# Clean up and classify zoning

rock_landuse_zoning <- rock_landuse |> 

  # Fix zoning codes
  mutate(MZONE = case_when(
    MZONE == "A1" ~ "A-1",
    MZONE == "A2" ~ "A-2",
    MZONE == "AT" ~ "A-T",
    MZONE == "A22" ~ "A-2",
    MZONE == "AG" ~ "A-1",
    MZONE == "AL" ~ "A-1",
    MZONE == "B1" ~ "B-1",
    MZONE == "B2" ~ "B-2",
    MZONE == "BG" ~ "B-1",
    MZONE == "C" ~ "C-1",
    MZONE == "C1" ~ "C-1",
    MZONE == "I1" ~ "I-1",
    MZONE == "R" ~ "R-1",
    MZONE == "R1" ~ "R-1",
    MZONE == "R2" ~ "R-2",
    MZONE == "R11" ~ "R-1",
    MZONE == "RG" ~ "R-1",
    is.na(MZONE) ~ "NA",
    TRUE ~ MZONE
  )) |> 
  
  # Remove parcels with town zoning designation
  filter(!MZONE == "TG") |> 
  
  # Filter out all other parcels within town boundaries
  st_join(rock_towns, join = st_within) |> 
  filter(is.na(ZONING))

# View results
# mapview(rock_landuse_zoning, zcol = "MZONE", col.regions = RColorBrewer::brewer.pal(10, "Paired"))
# rock_landuse_zoning |> filter(MZONE == "NA") |> mapview()

# Summarize area by zoning and land use  
  
rock_area <- rock_landuse_zoning |> 
  
  # Drop geometry
  st_drop_geometry() |> 
  
  # Convert sqft into acres
  mutate(acres = Shape_Area/43560) |> 
  
  # Summarize number of parcels and area by groups
  group_by(category, group, MZONE) |> 
  summarise(parcels = n(),
            area = sum(acres)) |> 
  ungroup() |> 
  
  # Calculate percentage shares for parcels and area
  mutate(parcels_pct = parcels/sum(parcels),
         area_pct = area/sum(area))

## Prep future land uses --------------

rock_flu_ssa <- rock_suburb |> 
  st_union() |> 
  st_sf() |> 
  mutate(future_land_use = "Suburban Service Area", .before = 1) |> 
  select(1, geometry = 2)

rock_flu_vsa <- rock_village |> 
  mutate(future_land_use = "Village Service Area", .before = 1) |> 
  select(1)

rock_flu_rva <- rock_ruralvillage |> 
  mutate(future_land_use = "Rural Village Area", .before = 1) |> 
  select(1)

rock_flu_rpa <- rock_rural |> 
  st_union() |> 
  st_sf() |> 
  mutate(future_land_use = "Rural Planning Area", .before = 1) |> 
  select(1, geometry = 2)

rock_flu <- bind_rows(rock_flu_ssa, rock_flu_vsa, rock_flu_rva, rock_flu_rpa) |> 
  select(1) |> 
  st_difference() |> 
  st_cast("POLYGON") |> 
  rowid_to_column() |> 
  filter(!rowid %in% c(9, 25)) |> 
  mutate(
    future_land_use = case_match(
      rowid,
      11 ~ "Interstate Interchange",
      12 ~ "Interstate Interchange",
      .default = future_land_use
    ),
    name = case_match(
      rowid,
      10 ~ "Natural Bridge",
      11 ~ "Exit 200 (Fairfield)",
      12 ~ "Exit 205 (Raphine)",
      14 ~ "Arnolds Valley",
      15 ~ "Fancy Hill",
      16 ~ "Effinger",
      17 ~ "Collierstown",
      18 ~ "Cornwall",
      19 ~ "Kerrs Creek",
      20 ~ "Rockbridge Baths",
      21 ~ "Vesuvius",
      22 ~ "Brownsburg",
      23 ~ "Brownsburg",
      24 ~ "Raphine"
    ),
    .after = 2
  ) |> 
  st_difference(rock_towns) |> 
  filter(
    `name.1` == "Glasgow",
    rowid != 13
    ) |> 
  select(1, 2, 3) |>
  st_cast("POLYGON") |> 
  mutate(acres = as.numeric(st_area(rock_flu)/43560), .after = 3)
  
rock_flu_area <- rock_flu |> 
  st_drop_geometry() |> 
  summarise(
    acres = sum(acres),
    .by = future_land_use
  ) |> 
  mutate(acres_pct = acres/sum(acres))

# mapview(rock_flu, zcol = "future_land_use", layer.name = "Future land use")

## Prep utilities ---------------------

rock_water_area <- rock_water |> 
  filter(!str_detect(LINETYPE, "Abandon")) |> 
  st_union() |> 
  st_sf() |> 
  st_buffer(dist = 500) |> 
  select(geometry = 1) |> 
  mutate(
    layer = "County_Water_20130522",
    buffer_ft = 500,
    .before = 1
    )

rock_sewer_area <- rock_sewer |> 
  st_union() |> 
  st_sf() |> 
  st_buffer(dist = 500) |> 
  select(geometry = 1) |> 
  mutate(
    layer = "County_Sewer_20130522",
    buffer_ft = 500,
    .before = 1
  )

rock_ws_only <- st_intersection(rock_water_area, rock_sewer_area) |> 
  select(1, 2) |> 
  mutate(
    layer = "Derived",
    label = "Both",
    .after = 1
    )

rock_water_only <- st_difference(rock_water_area, rock_ws_only) |> 
  select(1, 2) |> 
  mutate(
    label = "Water only",
    .after = 1
    )
  
rock_sewer_only <- st_difference(rock_sewer_area, rock_ws_only) |> 
  select(1, 2) |> 
  mutate(
    label = "Sewer only",
    .after = 1
  )

rock_ws <- bind_rows(rock_ws_only, rock_water_only, rock_sewer_only)

mapview(rock_ws, zcol = "label")

# mapview(rock_ws_area, col.regions =  "white") +
#   mapview(rock_sewer_only, col.regions = "brown") +
#   mapview(rock_water_only, col.regions = "blue")

## Save data --------------------------

# Land use and zoning

rock_landuse_zoning |> 
  st_transform(4326) |> 
  write_rds("data/rockbridge/rock_landuse_zoning.rds")

write_rds(rock_area, "data/rockbridge/rock_area.rds")

# Future land use

rock_flu |> 
  st_transform(4326) |> 
  write_rds("data/rockbridge/rock_flu.rds")

write_rds(rock_flu_area, "data/rockbridge/rock_flu_area.rds")

# Utilities

rock_ws |> 
  st_transform(4326) |> 
  write_rds("data/rockbridge/rock_ws.rds")

