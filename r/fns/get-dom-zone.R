library(tidyverse)
library(sf)

get_dominant_zone <- function(parcels, zoning) {
  # Ensure both datasets are in the same CRS
  if (st_crs(parcels) != st_crs(zoning)) {
    stop("Input datasets must have the same coordinate reference system")
  }
  
  # Get column names excluding geometry
  parcel_cols <- setdiff(names(parcels), "geometry")
  zoning_cols <- setdiff(names(zoning), "geometry")
  
  # Perform intersection to get all overlapping areas
  intersections <- st_intersection(parcels, zoning) %>%
    # Calculate area of each intersection
    mutate(
      intersection_area = st_area(geometry),
      intersection_area = as.numeric(intersection_area)
    )
  
  # Group by parcel identifier and find dominant zone
  dominant_zones <- intersections %>%
    group_by(across(all_of(parcel_cols))) %>%  # Group by non-geometry parcel columns
    mutate(
      total_area = sum(intersection_area),
      coverage_pct = intersection_area / total_area * 100
    ) %>%
    # Keep only the zone with maximum coverage
    slice_max(coverage_pct, n = 1, with_ties = FALSE) %>%
    # Select original parcel columns plus zoning columns
    st_drop_geometry() %>%
    select(all_of(parcel_cols), all_of(zoning_cols), 
           -intersection_area, -total_area, -coverage_pct)
  
  # Join back to original parcels using non-geometry columns
  result <- parcels %>%
    left_join(dominant_zones, by = parcel_cols)
  
  return(result)
}