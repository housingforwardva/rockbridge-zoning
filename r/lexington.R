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

# Turn spherical geometry off
sf_use_s2(FALSE)

## Import data -------------------------

# Parcels
lex_parcels <- st_read("data/lexington/Lex_Parcels_01172023.shp") |> 
  
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
  #filter(str_detect(group, "Not vacant")) |> 
  filter(
    category == "Educational",
    #group %in% c("Exempt")
    #MapNo == "24 1 7"
    ) |> 
  mapview(zcol = "group", layer.name = "group")

## Prep parcels for analysis -----------

# Fix land use and vacancy designations
lex_parcels_fix <- lex_parcels |> 
  
  # Create T/F vacancy field (under $10,000 improvements value)
  mutate(
    VACANT = case_when(
      MIMPRV < 10000 ~ TRUE,
      .default = FALSE
    ),
    .after = 7
  ) |> 
  
  # Create T/F exempt field
  mutate(
    EXEMPT = case_when(
      str_detect(MOCCUP, "EXEMPT") ~ TRUE,
      .default = FALSE
      ),
    .after = 7
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
    .before = 6
  ) |> 
  
  # Create land use group field, assign correct vacancy status
  mutate(
    group = case_when(
      VACANT == TRUE ~ "Vacant",
      VACANT == FALSE & str_detect(MOCCUP, "VACANT") ~ "Not vacant",
      .default = str_to_sentence(MOCCUP)
    ),
    .before = 7
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
      MLUSE == "STATE GOVERNMENT" & group == "Dwelling" ~ "Educational",
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
      category == "Public property" & str_detect(MREM1, "PLAYGROUND") ~ "Park",
      category == "Public property" & str_detect(MREM1, "OLD LANDFILL") ~ "Utilities",
      MapNo %in% c(
        "22 1 2", "35 1 13", "17 1 135", "25 1 124"
        ) ~ 
        "Public school",
      str_detect(MREM1, "RESIDENCE HALL") ~ "On-campus housing",
      MapNo %in% c("16 5 1", "15 1 36") ~ "On-campus housing",
      MapNo %in% c("16 1 37", "16 1 38", "16 1 41") ~ "Parking",
      MapNo %in% c(
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
      MapNo %in% c("23 1 94", "19 1 3") ~ "Residential",
      MapNo == "25 1 1 3" ~ "Commercial",
      .default = category
    )
  ) |> 
  mutate(
    group = case_when(
      str_detect(MLNAM, "WASHINGTON & LEE") &
        group == "Dwelling" &
        MIMPRV > 300000 ~
        "Campus",
      MLUSE == "STATE GOVERNMENT" & group == "Dwelling" ~ "Campus",
      MLUSE == "LOCAL GOVERNMENT" & group == "Dwelling" ~ "Park",
      MapNo == "19 1 3" ~ "Mobile home park",
      MapNo == "11 1 5" ~ "Park",
      MapNo %in% c("39 1 B 1") ~ "Common area",
      MMAP %in% c("23   1   177", "23   1    78") ~ "Parking",
      str_detect(MLNAM, "STONEWALL JACKSON HOSPITAL") & MapNo != "30 1 1#" ~ "Charitable",
      str_detect(MDESC2, "CAMPUS") ~ "Campus",
      str_detect(MDESC1, "HOPKINS GREEN") ~ "Park",
      str_detect(MDESC2, "ROY SMITH PK") ~ "Park",
      str_detect(MDESC2, "PARK DIAMOND & LEWIS") ~ "Park",
      str_detect(MREM1, "COMMON AREA") ~ "Common area",
      .default = group
    )
  ) |> 
  
  # Manual fix NA land use
  mutate(
    category = case_when(
      MapNo %in% c(
        "COMMON AREA", "39 1 A", "39 1 6C", "28 2 63",
        "1' SPITE STRIP", "18' PRIVATE DRIVE", "COLEX",
        "17 6 1 10", "25 8 A", "36 1 1", "23 1 102"
        ) ~
        "Residential",
      MapNo %in% c("18 1 19A") ~ "Educational",
      MapNo %in% c("YELLOW BRICK RD", "35 1 4", "35 1 6A") ~ "Commercial",
      MapNo %in% c("16 1 1") ~ "Educational",
      MapNo %in% c("24 1 7", "ALLEY") ~ "Public property",
      is.na(category) ~ "Residential",
      .default = category
    )
  ) |> 
  mutate(
    group = case_when(
      MapNo %in% c(
        "COMMON AREA", "39 1 A", "39 1 6C",
        "28 2 63"
        ) ~
        "Common area",
      MapNo %in% c("YELLOW BRICK RD", "35 1 4", "35 1 6A") ~ "Commercial",
      MapNo %in% c("16 1 1") ~ "Campus",
      MapNo == "24 1 7" ~ "Municipal",
      .default = group
    )
  )

# Summarize by land use  
lex_occ <- lex_parcels_fix |> 
  st_drop_geometry() |> 
  group_by(category, group) |> 
  summarise(parcels = n())

# 183 parcels with NA zoning

## Join zoning data -------------------

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

lex_area <- lex_parcels_fix |> 
  st_drop_geometry() |> 
  group_by(category, group) |> 
  summarise(parcels = n()) 
  summarise(parcels = n(),
            area = sum(acres)) |> 
  ungroup()
