#### -----------------------------------------
# Calculate fetch for HOBO sites
#### -----------------------------------------

# ---- Load libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(waver)


# ---- Load and prepare data and parameters ----
# View layers in geodatabase
st_layers("C:/Users/aaron/Documents/GIS_Parent/Projects/MassBays/HOBO_fetch/HOBO_fetch.gdb")

# Read NOAA CUSP layer (Merge of N40W070 and N40W075)
shoreline <- st_read(
  dsn = "C:/Users/aaron/Documents/GIS_Parent/Projects/MassBays/HOBO_fetch/HOBO_fetch.gdb",
  layer = "NOAA_CUSP_NE_HoboClip100km"
  )

# Check shoreline coordinate system
st_crs(shoreline)
# GEOGCRS"NAD83"

# Import hobo site coordinates
site_coords <- read.csv(
  "Data/site_coords.csv", 
  na.strings = ""
  ) |> 
  filter(site.id %in% c("AQ", "CB", "CC", "CL", "DC", "GB", "JB", "NK", "NK_prime", "OB", "SH", "WB", "WC", "PV", "NB"))

# Make site points from coordinate table
sites_sf <- st_as_sf(
  site_coords,
  coords = c("longitude", "latitude"),
  crs = 4269 # NAD83 (EPSG:4269) Geographic Coordinate System
)

# # Make sure CRS matches shoreline
sites_sf <- st_transform(sites_sf, st_crs(shoreline))

# Combine shoreline segments into one MULTILINESTRING geometry for faster processing
shoreline_combined <- st_combine(shoreline)

# Visualize shoreline and hobo points
plot(st_geometry(shoreline_combined))
plot(st_geometry(sites_sf), add = TRUE, col = "red", pch = 16)

# Bearings every 5 degrees
bearings <- seq(0, 355, by = 5)

# Max fetch distance: 100 km
dmax <- 100000


# ---- Calculate fetch using waver package ----
# # Calculation takes time. Saved data is read in below. Uncomment for a new fetch calculation.
# # Calculate fetch
# fetch_mat <- fetch_len_multi(
#   pts = sites_sf,
#   bearings = bearings,
#   shoreline = shoreline_combined,
#   dmax = dmax,
#   projected = FALSE
# )
# 
# # Add site IDs as row names
# rownames(fetch_mat) <- site_coords$site.id
# 
# # Convert to km
# fetch_km <- fetch_mat / 1000
# 
# # Build df - Wide format
# fetch_df <- as.data.frame(fetch_km)
# fetch_df$site.id <- rownames(fetch_km) 
# fetch_df <- fetch_df|> 
#   select(site.id, everything())
# 
# # # Save fetch distances as csv
# # write.csv(fetch_df, "Data/fetch_data/fetch_100km_5deg.csv", row.names = FALSE)

# Read saved (previously calculated) fetch data
fetch_df <- read.csv(
  "Data/fetch_data/fetch_100km_5deg.csv",
  check.names = FALSE) # Needed to keep column names as numbers
str(fetch_df)


# ---- Organize Fetch Data and Visualize ----
# Long format
fetch_long <- fetch_df |> 
  pivot_longer(
    cols = -site.id,
    names_to = "bearing",
    values_to = "fetch_km"
  ) |> 
  mutate(
    bearing = as.numeric(bearing)
  )

# Summary stats
summary_stats <- fetch_long |>
  group_by(site.id) |> 
  summarise(
    mean_fetch_km = mean(fetch_km, na.rm = TRUE),
    max_fetch_km  = max(fetch_km, na.rm = TRUE),
    min_fetch_km  = min(fetch_km, na.rm = TRUE),
    .groups = "drop"
  )

# # Save summary statistics as csv
# write.csv(summary_stats, "Data/fetch_data/fetch_100km_5deg_sumstats.csv", row.names = FALSE)

# Viz as polar plot
ggplot(fetch_long, aes(x = bearing, y = fetch_km)) +
  geom_line() +
  geom_area(fill = "lightblue", alpha = 0.5) +
  coord_polar() +
  facet_wrap(~ site.id, scales = "free_y") +
  labs(
    title = "Directional Fetch by Site",
    x = "Bearing (degrees)",
    y = "Fetch (km)"
  ) +
  theme_minimal()


# ---- Export Fetch as Geospatial Object ----
# Project sites to UTM zone 19N (EPSG:26919)
sites_utm <- st_transform(sites_sf, 26919)

# Join fetch data to site geometries
fetch_lines_df <- fetch_long |> 
  left_join(
    sites_utm |>  mutate(site.id = site_coords$site.id),
    by = "site.id"
  )

# Create line geometries
fetch_lines <- fetch_lines_df |>
  rowwise() |>
  mutate(
    # start point
    x0 = st_coordinates(geometry)[1],
    y0 = st_coordinates(geometry)[2],
    
    # convert bearing to radians
    theta = bearing * pi / 180,
    
    # calculate end point
    x1 = x0 + fetch_km * 1000 * sin(theta),
    y1 = y0 + fetch_km * 1000 * cos(theta),
    
    # create LINESTRING
    geom_line = st_sfc(
      st_linestring(matrix(c(x0, y0, x1, y1), ncol = 2, byrow = TRUE)),
      crs = st_crs(sites_utm)
    )
  ) |>
  ungroup()

# Convert to sf object
fetch_lines_sf <- st_as_sf(fetch_lines, sf_column_name = "geom_line") |> 
  select(site.id, bearing, fetch_km)

# Export as geopackage
st_write(
  fetch_lines_sf,
  "Data/fetch_data/fetch_vectors.gpkg",
  delete_dsn = TRUE
)


# ---- Effective Fetch Calculation ----
# Resources
# https://grass-tutorials.osgeo.org/content/tutorials/windfetch/windfetch.html
# https://cdn.coastalscience.noaa.gov/page-attachments/products/WEMo/WEMo_V4_manual.pdf
# https://umesc.usgs.gov/management/dss/wind_fetch_wave/wind_fetch_wave_2012update/wind_wave_2012_update_070814.pdf

