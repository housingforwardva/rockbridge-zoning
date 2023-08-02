## -------------------------------------
##
## Lexington City data prep
##
## -------------------------------------

## Load libraries ----------------------

library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(units)
library(leaflet)

sf_use_s2(FALSE)

## Import data -------------------------

# Parcels

lex_parcels <- st_read("data/lexington/Lex_Parcels_01172023.shp") |> 
  st_transform(4269) |> 
  select(3, 8, 9, 18:22, 27:32, 41)

# Zoning

lex_zoning <- st_read("data/Lexington/Zoning.shp") |> 
  st_transform(4269) |> 
  select(1, 4, 7)

# Census boundaries

lex_outline <- counties("51") |> 
  filter(NAME == "Lexington") |> 
  st_transform(4269)

# Floodplain

lex_fp <- st_read("data/Lexington/lex-nfhl/S_FLD_HAZ_AR.shp") |> 
  filter(FLD_ZONE %in% c("A", "AE")) |> 
  st_union() |> 
  st_as_sf() |> 
  st_intersection(lex_outline) |> 
  st_transform(4269)

## View data ---------------------------

mapview(lex_parcels_zoning, zcol = "MZONE") +
  mapview(lex_zoning_centroid, zcol = "Zoning2016")

lex_parcels_fix |> 
  filter(str_detect(group, "Not vacant")) |> 
  mapview(zcol = "category", layer.name = "Not vacant")

## Prep parcels for analysis -----------

# Fix land use and vacancy designations

lex_parcels_fix <- lex_parcels |> 
  
  # Create T/F vacancy field (under $10,000 improvements value)
  mutate(
    VACANT = case_when(
      MIMPRV < 10000 ~ TRUE,
      .default = FALSE
    ),
    .after = 6
  ) |> 
  
  # Create T/F exempt field
  mutate(
    EXEMPT = case_when(
      str_detect(MOCCUP, "EXEMPT") ~ TRUE,
      .default = FALSE
      ),
    .after = 6
  ) |> 
  
  # Remove exempt tag
  mutate(
    MOCCUP = str_remove_all(MOCCUP, "-EXEMPT")
  ) |> 
  
  # Create land use category field, fix university parcels, group public property
  mutate(
    category = case_when(
      str_detect(MLNAM, "MILITARY") ~ "Educational",
      str_detect(MLNAM, "WASHINGTON & LEE") ~ "Educational",
      str_detect(MLUSE, "GOVERNMENT") ~ "Public property",
      .default = str_to_sentence(MLUSE)
    ),
    .before = 5
  ) |> 
  
  # Create land use group field, assign correct vacancy status
  mutate(
    group = case_when(
      VACANT == TRUE ~ "Vacant",
      VACANT == FALSE & str_detect(MOCCUP, "VACANT") ~ "Not vacant",
      .default = MOCCUP
    ),
    .before = 6
  ) |> 
  
  # Assign parking use to parking lots
  mutate(
    group = case_when(
      str_detect(MREM1, "PARKING") ~ "Parking",
      str_detect(MREM2, "PARKING") ~ "Parking",
      str_detect(MDESC1, "PARKING") ~ "Parking",
      str_detect(MDESC2, "PARKING") ~ "Parking",
      .default = group
    )
  ) |> 
  
  # Fixes
  mutate(
    group = case_when(
      str_detect(MLNAM, "STONEWALL JACKSON HOSPITAL") ~ "Hospital",
      str_detect(MDESC2, "CAMPUS") ~ "Campus",
      category == "Public property" & str_detect(MREM1, "PLAYGROUND") ~ "Park",
      str_detect(MDESC1, "HOPKINS GREEN") ~ "Park",
      str_detect(MDESC2, "ROY SMITH PK") ~ "Park",
      str_detect(MDESC2, "PARK DIAMOND & LEWIS") ~ "Park",
      str_detect(group, "Not vacant") ~ "Other",
      .default = str_to_sentence(group)
    )
  )

# Summarize area by zoning and land use  
  
lex_occ <- lex_parcels_fix |> 
  st_drop_geometry() |> 
  group_by(category, group) |> 
  summarise(parcels = n())
  
lex_parcels_zoning <- lex_parcels |> 
  
  select(3, 9, 11:15, 17:20, 27:32, 40:41) |> 
  
  distinct(geometry, .keep_all = TRUE) |> 
  
  # Fix zoning field
  mutate(MZONE =
           case_match(
             MZONE,
             "C1" ~ "C-1",
             "C2" ~ "C-2",
             "C3" ~ "C-2",
             "I1" ~ "I-1",
             "R" ~ "R-1",
             "R1" ~ "R-1",
             "R1A" ~ "R-2",
             "R1C" ~ "R-1",
             "R1C1" ~ "R-1",
             "RCL" ~ "R-LC",
             "RLC" ~ "R-LC",
             "RM" ~ "R-M",
             "RMH" ~ "R-1",
             NA ~ "NA",
             .default = MZONE
           )) |> 
  
  left_join(
    st_drop_geometry(lex_zoning),
    by = "MapNo",
    relationship = "many-to-many"
    ) |> 
  
  mutate(MZONE = 
           case_when(
             is.na(Zoning2016) ~ MZONE,
             .default = Zoning2016
           )) |> 
  
  mutate(MZONE = str_replace_all(MZONE, "PCOS", "P-OS")) |> 
  
  select(1:7, 9:17, acres = 20, 24) |> 
  
  distinct(geometry, .keep_all = TRUE) |> 
  
  rowid_to_column()

lex_parcels_fp <- lex_parcels_zoning |> 
  st_intersection(lex_fp) |> 
  # group_by(MapNo) |> 
  # summarise() |> 
  mutate(fp_area = st_area(geometry)) |> 
  select(rowid, fp_area) |> 
  st_drop_geometry()

lex_landuse_zoning <- lex_parcels_zoning |> 
  left_join(lex_parcels_fp, by = "rowid") |> 
  mutate(parcel_area = st_area(geometry),
         fp_pct = as.numeric(fp_area/parcel_area),
         acres = as.numeric(parcel_area)/4047) |> 
  select()

# Summarize area by zoning and land use  

lex_occ <- lex_parcels_fix |> 
  st_drop_geometry() |> 
  group_by(category, group) |> 
  summarise(parcels = n()) 

  
  summarise(parcels = n(),
            area = sum(acres)) |> 
  ungroup()
