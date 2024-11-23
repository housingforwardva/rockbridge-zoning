## -------------------------------------
##
## Lexington City data prep
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
library(janitor)

# Turn spherical geometry off
sf_use_s2(FALSE)

## Import data -------------------------

# Parcels
lex_parcels <- st_read("data/lexington/Lex_Parcels_01172023.shp") |> 
  
  clean_names() |> 
  
  # Project to NAD83
  st_transform(4269) |> 
  
  # Keep only necessary columns
  select(3, 8, 9, 18:22, 27:32, 41) |> 
  
  # Identify parcels that exactly overlap (e.g. separate condo units)
  group_by(geometry) |> 
  mutate(
    n_overlap = n(),
    .before = 1
    ) |> 
  ungroup()

# Zoning
lex_zoning <- st_read("data/Lexington/Zoning.shp") |> 
  clean_names() |> 
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


## Prep parcels for analysis -----------

# Fix land use and vacancy designations
lex_parcels_fix <- lex_parcels |> 
  
  # Create T/F vacancy field (under $10,000 improvements value)
  mutate(
    vacant = case_when(
      mimprv < 10000 ~ TRUE,
      .default = FALSE
    ),
    .after = 7
  ) |> 
  
  # Create T/F exempt field
  mutate(
    exempt = case_when(
      str_detect(moccup, "EXEMPT") ~ TRUE,
      .default = FALSE
      ),
    .after = 7
  ) |> 
  
  # Remove exempt tag
  mutate(
    moccup = str_remove_all(moccup, "-EXEMPT")
  ) |> 
  
  # Create land use category field, fix university parcels, group public property
  mutate(
    category = case_when(
      str_detect(mlnam, "MILITARY") ~ "Educational",
      str_detect(mlnam, "WASHINGTON & LEE") ~ "Educational",
      str_detect(mluse, "GOVERNMENT") ~ "Public property",
      .default = str_to_sentence(mluse)
    ),
    .before = 6
  ) |> 
  
  # Create land use group field, assign correct vacancy status
  mutate(
    group = case_when(
      vacant == TRUE ~ "Vacant",
      vacant == FALSE & str_detect(moccup, "VACANT") ~ "Not vacant",
      .default = str_to_sentence(moccup)
    ),
    .before = 7
  ) |> 
  
  # Assign parking use to parking lots
  mutate(
    group = case_when(
      str_detect(mrem1, "PARKING") ~ "Parking",
      str_detect(mrem2, "PARKING") ~ "Parking",
      str_detect(mdesc1, "PARKING") ~ "Parking",
      str_detect(mdesc2, "PARKING") ~ "Parking",
      .default = group
    )
  ) |>
  
  # Fix residential uses
  mutate(
    group = case_when(
      group == "Townhouse/condo" & n_overlap > 1 ~ "Condo",
      group == "Townhouse/condo" & n_overlap == 1 ~ "Townhouse",
      category == "Commercial" & group == "Dwelling" ~ "Commercial",
      .default = group
    )
  ) |> 
  mutate(
    category = case_when(
      category == "Multi-family" ~ "Residential",
      category == "Religious" & group == "Dwelling" ~ "Residential",
      mluse == "STATE GOVERNMENT" & group == "Dwelling" ~ "Educational",
      .default = category
    )
  ) |> 
  
  # Assign "other" uses
  mutate(
    group = case_when(
      category == "Residential" & group == "Not vacant" ~ "Accessory lot",
      category == "Commercial" & group == "Not vacant" ~ "Commercial",
      category == "Educational" & group == "Not vacant" ~ "Campus",
      category == "Public property" & group == "Not vacant" ~ "Utilities",
      .default = group
    )
  ) |> 
  mutate(
    category = case_when(
      category == "Other" ~ "Charitable",
      .default = category
    )
  ) |> 
  
  # Manual "exempt" assignments
  mutate(
    category = case_when(
      category == "Commercial" & group == "Exempt" ~ "Charitable",
      .default = category
    )
  ) |> 
  mutate(
    group = case_when(
      category == "Charitable" ~ "Charitable",
      category == "Religious" & group == "Exempt" ~ "Religious",
      category == "Public property" & str_detect(mrem1, "PLAYGROUND") ~ "Park",
      category == "Public property" & str_detect(mrem1, "OLD LANDFILL") ~ "Utilities",
      map_no %in% c(
        "22 1 2", "35 1 13", "17 1 135", "25 1 124"
        ) ~ 
        "Public school",
      str_detect(mrem1, "RESIDENCE HALL") ~ "On-campus housing",
      map_no %in% c("16 5 1", "15 1 36") ~ "On-campus housing",
      map_no %in% c("16 1 37", "16 1 38", "16 1 41") ~ "Parking",
      map_no %in% c(
        "11 1 1A", "11 1 2", "11 2 A", "11 2 C", "11 1 3", "18 1 19A",
        "16 1 2", "16 1 4", "16 1 5", "16 1 6", "16 1 7", "16 1 11",
        "16 1 12", "16 1 36", "16 1 35", "16 1 34", "16 3 A",
        "17 1 96", "17 1 95 A", "17 1 90"
        ) ~
        "Campus",
      .default = group
    )
  ) |> 
  mutate(
    group = case_when(
      category == "Public property" & group == "Exempt" ~ "Municipal",
      .default = group
    )
  ) |> 
  
  # Manual land use assignments
  mutate(
    category = case_when(
      map_no %in% c("23 1 94", "19 1 3") ~ "Residential",
      map_no == "25 1 1 3" ~ "Commercial",
      .default = category
    )
  ) |> 
  mutate(
    group = case_when(
      str_detect(mlnam, "WASHINGTON & LEE") &
        group == "Dwelling" &
        mimprv > 300000 ~
        "Campus",
      mluse == "STATE GOVERNMENT" & group == "Dwelling" ~ "Campus",
      mluse == "LOCAL GOVERNMENT" & group == "Dwelling" ~ "Park",
      map_no == "19 1 3" ~ "Mobile home park",
      map_no == "11 1 5" ~ "Park",
      map_no %in% c("39 1 B 1") ~ "Common area",
      mmap %in% c("23   1   177", "23   1    78") ~ "Parking",
      str_detect(mlnam, "STONEWALL JACKSON HOSPITAL") & map_no != "30 1 1#" ~ "Charitable",
      str_detect(mdesc2, "CAMPUS") ~ "Campus",
      str_detect(mdesc1, "HOPKINS GREEN") ~ "Park",
      str_detect(mdesc2, "ROY SMITH PK") ~ "Park",
      str_detect(mdesc2, "PARK DIAMOND & LEWIS") ~ "Park",
      str_detect(mrem1, "COMMON AREA") ~ "Common area",
      .default = group
    )
  ) |> 
  
  # Manual fix NA land use
  mutate(
    category = case_when(
      map_no %in% c(
        "COMMON AREA", "39 1 A", "39 1 6C", "28 2 63",
        "1' SPITE STRIP", "18' PRIVATE DRIVE", "COLEX",
        "17 6 1 10", "25 8 A", "36 1 1", "23 1 102"
        ) ~
        "Residential",
      map_no %in% c("18 1 19A") ~ "Educational",
      map_no %in% c("YELLOW BRICK RD", "35 1 4", "35 1 6A") ~ "Commercial",
      map_no %in% c("16 1 1") ~ "Educational",
      map_no %in% c("24 1 7", "ALLEY") ~ "Public property",
      is.na(category) ~ "Residential",
      .default = category
    )
  ) |> 
  mutate(
    group = case_when(
      map_no %in% c(
        "COMMON AREA", "39 1 A", "39 1 6C",
        "28 2 63"
        ) ~
        "Common area",
      map_no %in% c("YELLOW BRICK RD", "35 1 4", "35 1 6A") ~ "Commercial",
      map_no %in% c("16 1 1") ~ "Campus",
      map_no == "24 1 7" ~ "Municipal",
      .default = group
    )
  )

lex_parcels_test <- lex_parcels_fix |> 
  filter(
    !map_no %in% c(
      "NM 13", "30 1 16", "35 1 1A", "29 1 33A#", "40 1 1A#", "16 1 40",
      "16 1 3A", "23 1 165#", "23 12 7#"
      )
  )

#30 1 16
#35 1 1A

# 183 parcels with NA zoning


## Join zoning data -------------------

lex_parcels_zoning <- lex_parcels_fix |> 
  
  distinct(geometry, .keep_all = TRUE) |> 
  
  # Fix zoning field
  mutate(mzone =
           case_match(
             mzone,
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
             .default = mzone
           )) |> 
  
  left_join(
    distinct(st_drop_geometry(lex_zoning), map_no, .keep_all = TRUE),
    by = "map_no"
    #relationship = "many-to-many"
    ) |> 
  
  filter(
    !map_no %in% c(
      "NM 13", "30 1 16", "35 1 1A", "29 1 33A#", "40 1 1A#", "16 1 40",
      "16 1 3A", "23 1 165#", "23 12 7#", "30 1 8#", "YELLOW BRICK RD"
    )
  ) |> 
  
  mutate(mzone = 
           case_when(
             is.na(zoning2016) ~ mzone,
             .default = zoning2016
           )) |> 
  
  mutate(
    mzone = case_when(
      map_no == "19 1 11" ~ "R-2",
      map_no == "19 1 2" ~ "C-2",
      map_no == "16 3 C" ~ "R-1",
      map_no == "18 1 19A" ~ "R-1",
      map_no == "35 1 6A" ~ "C-2",
      .default = mzone
    )
  ) |> 
  
  mutate(mzone = str_replace_all(mzone, "PCOS", "P-OS")) |> 

  rowid_to_column() |> 
  
  select(1, 3, 5:12, 21)

lex_pz_area <- lex_parcels_zoning |> 
  mutate(
    area = as.numeric(
      st_area(lex_parcels_zoning) / 4047
    ),
    .before = 11
  ) 

## Save data --------------------------

lex_pz_area |> 
  write_rds("data/lexington/lex_parcels.rds")

lex_zoning_area <- lex_pz_area |> 
  group_by(mzone) |> 
  summarise()

lex_zoning_area |> 
  write_rds("data/lexington/lex_zoning.rds")

# Summarize parcels by zoning
#lex_zone <- lex_pz_area |> 
#  st_drop_geometry() |> 
#  #group_by(category, group) |> 
#  group_by(mzone) |> 
#  summarise(parcels = n())

#lex_parcels_fp <- lex_parcels_zoning |> 
#  st_intersection(lex_fp) |> 
#  # group_by(MapNo) |> 
#  # summarise() |> 
#  mutate(fp_area = st_area(geometry)) |> 
#  select(rowid, fp_area) |> 
#  st_drop_geometry()

# lex_landuse_zoning <- lex_parcels_zoning |> 
#   left_join(lex_parcels_fp, by = "rowid") |> 
#   mutate(parcel_area = st_area(geometry),
#          fp_pct = as.numeric(fp_area/parcel_area),
#          acres = as.numeric(parcel_area)/4047) |> 
#   select()

# Summarize area by zoning and land use  

#lex_area <- lex_parcels_fix |> 
#  st_drop_geometry() |> 
#  group_by(category, group) |> 
#  summarise(parcels = n()) 
#  summarise(parcels = n(),
#            area = sum(acres)) |> 
#  ungroup()
