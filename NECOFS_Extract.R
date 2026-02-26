# Load libraries
# library(RNetCDF)
library(ncdf4)
library(purrr)
library(dplyr)

# ---- Read site data and extract list of coordinates by site ----
site_data <- read.csv("Data/site_data.csv")
site_coords <- site_data |>
  select(site.id, latitude, longitude) |>
  distinct()

# ---- Build custom function to determine which node is closest to a given coordinate point ----
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

# ---- NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST ----
# Link to NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc THREDDS page:
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


# ---- NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST ----
# Link to NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc THREDDS page:
# http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Forecasts/catalog.html?dataset=models/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc

# Set OpenDAP URL. Details:
# lon[0:1:207080],lat[0:1:207080] - Includes full lat/lon range, and therefor all nodes 
# time[0:1:2855],Times[0:1:2855] - Includes all times and time stamps except the last [2856], because the last is midnight on the following day, so a full daily average cannot be calculated for that day
# temp[0:1:2855][44:1:44][0:1:207080] - Temperature measurements for all nodes and all but the last time point (as above), but only for the last (deepest) vertical sigma layer [44]
URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc?lon[0:1:207080],lat[0:1:207080],time[0:1:2855],Times[0:1:2855],temp[0:1:2855][44:1:44][0:1:207080]"

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

# Determine the number of time measurements made
times_count <- nc$dim$time$len
times_count

# Create empty columns for node index and not lat/lon coords
site_coords$nearest_node <- NA_integer_
site_coords$node_lat <- NA
site_coords$node_lon <- NA

# Create an empty list of dfs the length of site_coords (one df for each site)
hourly_temps <- vector("list", nrow(site_coords))

# Set up for loop:
# Establish total time steps and chunk size
nt <- nc$dim[["time"]]$len
chunk_size <- 168 # 7 days

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
  
  # Read in chunks over time to report progress
  for (t0 in seq(1, nt, by = chunk_size)) {
    t1 <- min(t0 + chunk_size - 1, nt) # Set high value in chunk
    n_this <- t1 - t0 + 1 # Determine how many time values for the request
    
    # Get temp measurements 
    vals <- ncvar_get(
      nc, "temp",
      start = c(idx, 1, t0),    # (node index, sigma-layer index is 1 since URL subset includes only the lowest layer [44], first time index for this chunk)
      count = c(1,  1, n_this)  # (one node, one sigma layer, number of time values for chunk)
    )
    
    ts[t0:t1] <- as.numeric(vals)
    
    message(
      "Site ", site_coords$site.id[i],
      " progress: ", t0, "-", t1, " / ", nt,
      " (", Times[t0], " to ", Times[t1], ")"
    )
  }
  
  # Build an hourly time series table for this site (one row per model timestamp) with temperature at the nearest node
  hourly_temps[[i]] <- data.frame(
    site_id = site_coords$site.id[i],
    time = Times,
    temp = as.numeric(ts)
  )
  
  # Update user on progress in Console
  message("Site ", site_coords$site.id[i], " completed.")
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


# ---- Simple Extract of Nodes by Coordinate Location - MassBays ----
url <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc?lon[0:1:98431],lat[0:1:98431]"

nc <- nc_open(url)

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

nc_close(nc)

nodes <- data.frame(
  node_id = seq_along(lon),
  lon = lon,
  lat = lat
)

write.csv(nodes, "MASSBAY_nodes.csv", row.names = FALSE)


# ---- Simple Extract of Nodes by Coordinate Location - NorthEast ----
url <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc?lon[0:1:207080],lat[0:1:207080]"

nc <- nc_open(url)

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

nc_close(nc)

nodes <- data.frame(
  node_id = seq_along(lon),
  lon = lon,
  lat = lat
)

write.csv(nodes, "NORTHEAST_nodes.csv", row.names = FALSE)


# ---- NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST - Version 2 ----
# Link to NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc THREDDS page:
# http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Forecasts/catalog.html?dataset=models/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc

### 1) Find nearest node index for each site

# Set OpenDAP URL. Details:
URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc"

# "?lon,lat" at the end of the URL - Includes full lat/lon range, and therefor all nodes 
URL_ll <- paste0(URL, "?lon,lat")

# Open netCDF file to read data
nc_ll <- nc_open(URL_ll)

# Extract all longitude and latitude values and check ranges
lon <- ncvar_get(nc_ll, "lon")
lat <- ncvar_get(nc_ll, "lat")

nc_close(nc_ll)

# For each row in site_coords, find the index of the nearest FVCOM node.
# vapply() loops over site indices and returns a numeric vector (integer(1) per site).
idx_needed <- vapply(
  seq_len(nrow(site_coords)),
  function(i) {                 # Anonymous function applied to each site index i
    # Call custom nearest_node() function
    nearest_node(lon, lat, site_coords$longitude[i], site_coords$latitude[i])
  },
  integer(1)  # Tell vapply() to expect exactly one integer result per site
)

site_coords$nearest_node <- idx_needed
site_coords$node_lon <- lon[idx_needed]
site_coords$node_lat <- lat[idx_needed]

### 1) Determine the total time range

# "?time,Times" at the end of the URL - Includes full time range
URL_time <- paste0(URL, "?time")
nc_time <- nc_open(URL_time)

time <- ncvar_get(nc_time, "time")
time_units <- ncatt_get(nc_time, "time", "units")$value

nc_close(nc_time)

nt <- length(time)
t_end <- nt - 1

# Convert numeric time to POSIXct (much faster than downloading Times)
unit_part  <- sub(" since.*$", "", time_units)
origin_str <- sub("^.* since ", "", time_units)
origin <- as.POSIXct(origin_str, tz = "UTC")

mult <- switch(
  unit_part,
  "seconds" = 1, "second" = 1,
  "minutes" = 60, "minute" = 60,
  "hours"   = 3600, "hour" = 3600,
  "days"    = 86400, "day" = 86400,
  stop("Unhandled time unit: ", unit_part, " (units=", time_units, ")")
)

Times_use <- origin + time * mult

# 3) One OpenDAP call per site/node for temp across all times

hourly_temps_list <- vector("list", nrow(site_coords))
deep_layer <- 44 # deepest sigma layer (siglay) in the NE forecast URL

for (i in seq_len(nrow(site_coords))) {
  
  # R index (1-based)
  node_idx_r <- site_coords$nearest_node[i]
  
  # Convert to OpenDAP index (0-based)
  node_idx_0 <- node_idx_r - 1
  
  # Build the temp constraint for the URL
  # Format: temp[time_start:stride:time_end][siglay][node]
  temp_constraint <- paste0(
    "temp[0:1:", t_end, "]",
    "[", deep_layer, ":1:", deep_layer, "]",
    "[", node_idx_0, ":1:", node_idx_0, "]"
  )
  
  # Full URL = base + "?" + constraint
  URL_temp_i <- paste0(URL, "?", temp_constraint)
  
  # Open connection and fetch data
  nc_i <- nc_open(URL_temp_i)
  ts   <- ncvar_get(nc_i, "temp")
  nc_close(nc_i)
  
  # Drop singleton dims (common: 1x1xnt) and ensure numeric vector
  ts <- as.numeric(drop(ts))
  
  # Safety check: lengths should match
  if (length(ts) != length(Times_use)) {
    stop(
      "Length mismatch at site ", site_coords$site.id[i],
      ": temp length = ", length(ts),
      ", time length = ", length(Times_use),
      ". Check temp dimension order or t_end."
    )
  }
  
  # Store per-site hourly temps
  hourly_temps_list[[i]] <- tibble(
    site_id = site_coords$site.id[i],
    time    = Times_use,
    temp_c  = ts
  )
  
  message(
    "Site ", site_coords$site.id[i],
    " completed (node ", node_idx_r, ")."
  )
}

hourly_temps <- bind_rows(hourly_temps_list)


# ---- NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST - Version 3 ----
# Link to NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc THREDDS page:
# http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Forecasts/catalog.html?dataset=models/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc

base_URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc"
nc_time <- nc_open(paste0(base_URL, "?time"))
nt <- nc_time$dim$time$len
nc_close(nc_time)

last_idx_0 <- nt - 1        # 0-based last index
first_idx_0 <- max(0, last_idx_0 - 191) # 8 days prior to the latest forecast

time_slice <- paste0("[", first_idx_0, ":1:", last_idx_0, "]")

URL <- paste0(
  base_URL,
  "?lon,lat,time", time_slice,
  ",Times", time_slice,
  ",temp", time_slice, "[44:1:44][0:1:207080]"
)

# Open netCDF file to read data
nc <- nc_open(URL)

# Check time range
Times <- ncvar_get(nc, "Times")

# Extract all longitude and latitude values and check ranges
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

# Determine the number of time measurements made
times_count <- nc$dim$time$len

# Create empty columns for node index and not lat/lon coords
site_coords$nearest_node <- NA_integer_
site_coords$node_lat <- NA
site_coords$node_lon <- NA

# Create an empty list of dfs the length of site_coords (one df for each site)
hourly_temps <- vector("list", nrow(site_coords))

# Set up for loop:
# Establish total time steps and chunk size
nt <- nc$dim[["time"]]$len
chunk_size <- 24 # 1 day

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
  
  temp_ts <- numeric(nt)
  
  # Read in chunks over time to report progress
  for (t0 in seq(1, nt, by = chunk_size)) {
    t1 <- min(t0 + chunk_size - 1, nt) # Set high value in chunk
    n_this <- t1 - t0 + 1 # Determine how many time values for the request
    
    # Get temp measurements 
    vals <- ncvar_get(
      nc, "temp",
      start = c(idx, 1, t0),    # (node index, sigma-layer index is 1 since URL subset includes only the lowest layer [44], first time index for this chunk)
      count = c(1,  1, n_this)  # (one node, one sigma layer, number of time values for chunk)
    )
    
    temp_ts[t0:t1] <- as.numeric(vals)
    
    message(
      "Site ", site_coords$site.id[i],
      " progress: ", t0, "-", t1, " / ", nt,
      " (", Times[t0], " to ", Times[t1], ")"
    )
  }
  
  # Build an hourly time series table for this site (one row per model timestamp) with temperature at the nearest node
  hourly_temps[[i]] <- data.frame(
    site_id = site_coords$site.id[i],
    time = Times,
    temp = temp_ts
  )
  
  # Update user on progress in Console
  message("Site ", site_coords$site.id[i], " completed.")
}

# Create df of hourly temps
hourly_temps <- bind_rows(hourly_temps)

# Close .nc file
nc_close(nc)
