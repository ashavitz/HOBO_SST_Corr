# Load libraries
library(RNetCDF)
library(ncdf4)
library(purrr)
library(dplyr)

# Read site data and extract list of coordinates by site
site_data <- read.csv("Data/site_data.csv")
site_coords <- site_data |>
  select(site.id, latitude, longitude) |>
  distinct()

# Link to NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc THREDDS page
# http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Forecasts/catalog.html?dataset=models/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc

# Set OpenDAP URL
URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc"

# Open netCDF file to read data
nc <- nc_open(URL)

# View file information
print(nc)

# View dimension names and lengths
names(nc$dim)
map_dbl(nc$dim, ~ .x$len)

# View variables names
names(nc$var)

# Check time range
Times <- ncvar_get(nc, "Times")
range(Times)

# Extract all longitude and latitude values and check ranges
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

range(lon, na.rm = TRUE)
range(lat, na.rm = TRUE)

# # Determine the number of time measurements made
times_count <- nc$dim$time$len

# # Extract the matrix of sigma layers (vertical layer through the water column)
# # There are 10 layers. Values in siglay matrix represent represent the normalized vertical position of each layer
# # (sigma coordinate), from ~0 at the surface to ~-1 at the bottom.
# siglay_matrix <- ncvar_get(nc, "siglay")

# Build custom function to determine which node is closeset to a given coordinate point
nearest_node <- function(lon, lat, lon0, lat0) {
  dlon <- lon - lon0
  dlat <- lat - lat0
  
  # Scale lon distance by cos(latitude) so degrees behave roughly like meters
  # This is necessary because we need to calculate the node that is the shortest euclidean distance away. Isolating the closest lat
  # coordinate and closest lon coordinate separately won't work, because the mesh is not a regular grid.
  x <- dlon * cos(lat0 * pi/180)
  y <- dlat
  
  which.min(x^2 + y^2)
}

# Create empty columns for node index and not lat/lon coords
site_coords$nearest_node <- NA_integer_
site_coords$node_lat <- NA
site_coords$node_lon <- NA

# Create an empty list of dfs the length of site_coords (one df for each site)
hourly_temps <- vector("list", nrow(site_coords))

# Iterate through site coordinates df identifying the closest node and corresponding coordinates
for (i in seq_len(nrow(site_coords))) {
  # Determine nearest node index
  lon0 <- site_coords$longitude[i]
  lat0 <- site_coords$latitude[i]
  idx <- nearest_node(lon, lat, lon0, lat0)
  site_coords$nearest_node[i] <- idx
  
  # Use index to determine lat and lon of nearest node
  site_coords$node_lat[i] <- lat[idx]
  site_coords$node_lon[i] <- lon[idx]
  
  # Get all temp measurements for one node at lowest siglay (siglay index = 10)
  ts <- ncvar_get(nc, "temp",
                  start = c(idx, 10, 1),  # (node index, sigma-layer index, first time index)
                  count = c(1, 1, -1))    # (one node, one sigma layer, all times)
  ts <- as.numeric(ts)
  
  # Build an hourly time series table for this site (one row per model timestamp) with temperature at the nearest node
  hourly_temps[[i]] <- data.frame(
    site_id = site_coords$site.id[i],
    time = Times,
    temp = as.numeric(ts)
  )
}

# Calculate daily mean and median temps by site.
daily_temps <- bind_rows(hourly_temps) |> 
  mutate(date = as.Date(time, tz = "UTC")) |> 
  group_by(site_id, date) |> 
  summarize(
    temp_daily_mean = mean(temp, na.rm = TRUE),
    temp_daily_median = median(temp, na.rm = TRUE),
    .groups = "drop"
  )

# Close .nc file
nc_close(nc)
