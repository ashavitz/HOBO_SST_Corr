library(RNetCDF)
library(ncdf4)
library(purrr)

# ---- Explore NECOFS_MET_HINDCAST.nc ----
opendap_url <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_MET_HINDCAST.nc"

nc <- nc_open(opendap_url)

# list dim sizes
sapply(nc$dim, function(d) d$len)

# read 1 time string (Times is char[maxStrlen64,Time])
t1 <- ncvar_get(nc, "Times", start = c(1, 1), count = c(64, 1))
cat(paste0(t1, collapse = ""), "\n")


# one scalar
t2_1pt <- ncvar_get(nc, "T2",
                    start = c(100, 100, 1),
                    count = c(1,   1,   1))
t2_1pt


# first time
t_start <- ncvar_get(nc, "Times",
                     start = c(1, 1),
                     count = c(64, 1))
t_start <- paste0(t_start, collapse = "")

# last time
nt <- nc$dim$Time$len
t_end <- ncvar_get(nc, "Times",
                   start = c(1, nt),
                   count = c(64, 1))
t_end <- paste0(t_end, collapse = "")


# all times
nt <- nc$dim$Time$len
Times_raw <- ncvar_get(nc, "Times")


print(nc)

ncatt_get(nc, "T2", "description")

tdata <- ncvar_get(nc, varid = "T2", verbose = TRUE)


nc_close(nc)

# ---- Explore NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc ----

URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc"

nc <- nc_open(URL)

names(nc$dim)
names(nc$var)

Times <- ncvar_get(nc, "Times")
Times_ct <- as.POSIXct(Times, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
range(Times_ct)

# Extract all longitude and latitude values as numeric vectors
lon <- ncvar_get(nc, "lon")   # length = node
lat <- ncvar_get(nc, "lat")   # length = node

range(lon, na.rm = TRUE)
range(lat, na.rm = TRUE)


nearest_node <- function(lon, lat, lon0, lat0) {
  # handle dateline issues if you ever have them (usually not needed for Mass Bay)
  dlon <- lon - lon0
  dlat <- lat - lat0
  
  # scale lon distance by cos(latitude) so degrees behave roughly like meters
  x <- dlon * cos(lat0 * pi/180)
  y <- dlat
  
  which.min(x^2 + y^2)
}

lon0 <- -70.90   # your POI longitude
lat0 <-  42.35   # your POI latitude

idx <- nearest_node(lon, lat, lon0, lat0)

# check what node you actually matched
c(poi_lon = lon0, poi_lat = lat0, node_lon = lon[idx], node_lat = lat[idx])


siglay <- ncvar_get(nc, "siglay")   # length 10 (sometimes negative to 0)
siglay


siglay_mat <- ncvar_get(nc, "siglay")   # [node, 10]

sig_node <- siglay_mat[idx, ]           # length 10, for your node only

k_surf <- which.max(sig_node)           # closest to 0
k_bot  <- which.min(sig_node)           # closest to -1

c(k_surf=k_surf, sig_surf=sig_node[k_surf],
  k_bot =k_bot,  sig_bot =sig_node[k_bot])


nt <- nc$dim$time$len   # 145

temp_surf <- ncvar_get(nc, "temp",
                       start = c(idx, k_surf, 1),
                       count = c(1,   1,      nt))

temp_bot  <- ncvar_get(nc, "temp",
                       start = c(idx, k_bot,  1),
                       count = c(1,   1,      nt))


Times_str <- ncvar_get(nc, "Times")
Times_ct  <- as.POSIXct(Times_str, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")

df <- data.frame(time=Times_ct, temp_surface=temp_surf, temp_bottom=temp_bot)
head(df); tail(df)

nc_close(nc)


# ---- Extract subsetted .nc file for ArcGIS pro ----

# Input OPeNDAP URL
in_url <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_MASSBAY_FORECAST.nc"

# Output file path
out_file <- "Data/NECOFS/MassBay_Forecast_temp_siglay1_20260109T00.nc"

# Open remote dataset
nc <- nc_open(in_url)

# --- Choose subset indices ---
time_i <- 1
sig_i  <- 1   # "lowest" siglay as first index; change to 10 if you want the top layer

# --- Read coordinates ---
lon <- ncvar_get(nc, "lon")  # [node]
lat <- ncvar_get(nc, "lat")  # [node]

# Read numeric time value (CF time coordinate)
time_val <- ncvar_get(nc, "time", start = time_i, count = 1)
time_units <- ncatt_get(nc, "time", "units")$value

# --- Read subset of temperature: temp[node, siglay, time] ---
# temp dims are [node, siglay, time] per your printout
temp_1 <- ncvar_get(
  nc, "temp",
  start = c(1, sig_i, time_i),
  count = c(-1, 1, 1)
)

# ncvar_get will return a vector/array; make it [node, time] for CF-ish output
temp_1 <- as.vector(temp_1)

nc_close(nc)

# ----------------------------
# Create a new small netCDF
# ----------------------------

# Define dimensions
dim_node <- ncdim_def(name = "node", units = "", vals = as.integer(1:length(lon)))
dim_time <- ncdim_def(name = "time", units = time_units, vals = time_val, unlim = FALSE)  # creates time dimvar

# Define coordinate variables (lon/lat)
var_lon <- ncvar_def(
  name = "lon", units = "degrees_east", dim = list(dim_node),
  missval = NA_real_, longname = "nodal longitude", prec = "float"
)
var_lat <- ncvar_def(
  name = "lat", units = "degrees_north", dim = list(dim_node),
  missval = NA_real_, longname = "nodal latitude", prec = "float"
)

# Define temperature variable
var_temp <- ncvar_def(
  name = "temp", units = "degrees_C", dim = list(dim_node, dim_time),
  missval = -9999.0, longname = "sea_water_temperature", prec = "float"
)

# Create file
nc_out <- nc_create(out_file, vars = list(var_lon, var_lat, var_temp), force_v4 = FALSE)

# Write data
ncvar_put(nc_out, "lon", lon)
ncvar_put(nc_out, "lat", lat)

# Write time into the auto-created dimvar "time"
ncvar_put(nc_out, "time", time_val)

# temp must be [node, time]; make matrix with 1 column
ncvar_put(nc_out, "temp", matrix(temp_1, ncol = 1))

# Attributes ArcGIS/CF often likes
ncatt_put(nc_out, "lon", "standard_name", "longitude")
ncatt_put(nc_out, "lat", "standard_name", "latitude")
ncatt_put(nc_out, "temp", "standard_name", "sea_water_temperature")
ncatt_put(nc_out, "temp", "coordinates", "time lat lon")
ncatt_put(nc_out, 0, "Conventions", "CF-1.0")
ncatt_put(nc_out, 0, "title", "Subset: temp at time=1, siglay=1 (FVCOM)")

nc_close(nc_out)

# ---- Explore NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc ----
URL <- "http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Forecasts/NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc?lon[0:1:207080],lat[0:1:207080],siglay[0:1:44][0:1:207080],time[0:1:2856],Times[0:1:2856],temp[0:1:2856][0:1:44][0:1:207080]"

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


nc_close(nc)

# ---- Explore NECOFS_FVCOM_OCEAN_NORTHEAST_FORECAST.nc with python via reticulate package ----
# Load reticulate package to enable a python environment
library(reticulate)


