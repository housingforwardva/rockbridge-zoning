# Rockbridge County data prep

library(tidyverse)
library(sf)
library(mapview)
library(units)
library(leaflet)

# Import Rockbridge County parcel shapefile

rock_parcels <- st_read("data/rockbridge/Rockbridge_ParcelsFebruary2023.shp")

# Import Rockbridge County zoning shapefile

rock_zoning <- st_read("data/rockbridge/Rockbridge_Zoning20230126.shp")

# Join parcels to zoning. 
rock_parcels_z <- st_join(rock_parcels, rock_zoning, join = st_within, left = TRUE)


# Below dissolves polygons by zoning district and sums the acre field, which may be incorrect.

rock_zsimplified <- rock_zoning %>% 
  group_by(ZONING) %>% 
  summarise(acres = sum(ACRES)) %>% 
  filter(ZONING != "City/Town") # Filter out the towns and cities.
  

# Below creates a column that calculates area based on the units set.

rock_zsimplified$area <- set_units(st_area(rock_zsimplified), "acre")

rock_zsimplified_pct <- rock_zsimplified %>% 
  mutate(pct = area/sum(area))
  
mapview(rock_zsimplified, zcol = "ZONING")

rock_zmap <- rock_zsimplified %>% st_transform(crs = 4326)

leaflet(rock_zmap) %>% 
  addTiles() %>% 
  addPolygons()

# Import Rockbridge County PSA features

rock_psa_layers <- st_layers("data/rockbridge/RockbridgePSA.gdb")

rock_water <- st_read(dsn = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Water_20130522")

rock_sewer <- st_read(dsn = "data/rockbridge/RockbridgePSA.gdb", layer = "County_Sewer_20130522")

mapview(rock_water, color = "blue") + mapview(rock_sewer, color = "lightblue")  + mapview(rock_parcels, zcol = "MZONE")
