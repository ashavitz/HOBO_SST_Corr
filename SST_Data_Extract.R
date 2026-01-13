# Purpose: Extract SST data from various satellite SST data sets.
# The following code employs the ERDDAP Interpolate Service via rerddapXtracto::rxtracto
# Info on ERDDAP interpolate service: https://coastwatch.pfeg.noaa.gov/erddap/convert/interpolate.html
# The following interpolation methods are used:
#   - Nearest neighbor interpolation: The nearest n grid cells are checked (including the cell in which the coordinate point falls) and returns the nearest non-NaN cell value.
#   - Inverse distance squared interpolation: Returns the inverse distance interpolation of the nearest n non-NaN data values.
#     The weight for each nearby data value is w = 1/(D^2) where D is the cell indexed distance.

# ---- Load Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr) #for plot function code; map function
library(zoo) #approx nas from neighbors

# ERRDAP Data Access
library(rerddap) # For reading ERDDAP data
library(rerddapXtracto)

# THREDDS NetCDF file access
library(RNetCDF)
library(ncdf4)

# ---- Prepare request df ----
site_data <- read.csv("Data/site_data.csv")


# ---- NASA JPL Multi-scale Ultra-high Resolution (MUR) SST----
  
## Extract ERDDAP JPL satellite SST data 
### jplMURSST41
  
# - NASA JPL 
# - Acknowledgement: These data were provided by JPL under support by NASA MEaSUREs program. 
# - Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1, Global, 0.01°, 2002-present, Daily 
# - (https://coastwatch.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html) 
# - Metadata: https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1

# Extract metadata from NOAA ERDDAP
# NOTE - jplMURSST41 is hosted at https://coastwatch.pfeg.noaa.gov/erddap, however this erddap
# appears to be intermittently inaccessible, causing script to crash.
jplMURSST41_info <-
  rerddap::info("jplMURSST41",
                url="https://coastwatch.pfeg.noaa.gov/erddap"
  )

# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'analysed_sst'
xcoord <- site_data$longitude
ycoord <- site_data$latitude
tcoord <- site_data$date

# NOTE:
# - rxtracto() returns a list of statistics around the points provided via xcoord and ycoord
# - xlen and ylen denote the size (degrees) of the surrounding box from which you
#   want statistics. The default for these is 0, which will extract data for the grid cell
#   in which each coordinate provided falls.

# Extract SST data with no interpolation
extract_jpl <- rxtracto(jplMURSST41_info,
                    parameter = parameter,
                    tcoord = tcoord,
                    xcoord = xcoord, ycoord = ycoord,
                    xlen = 0.0, ylen = 0.0,
                    # interp = c("Nearest", "4"),
                    progress_bar = TRUE)

# saveRDS(extract_jpl, "Data/ERDDAP_Data/sst_JPL.rds")
# saveRDS(extract_jpl_0.05mean, "Data/ERDDAP_Data/sst_JPL_0.05mean.rds")

# Extract SST data using nearest neighbor interpolation for 4 grid cells.
extract_jpl_n4 <- rxtracto(jplMURSST41_info,
                           parameter = parameter,
                           tcoord = tcoord,
                           xcoord = xcoord, ycoord = ycoord,
                           xlen = 0.0, ylen = 0.0,
                           interp = c("Nearest", "4"),
                           progress_bar = TRUE)

# saveRDS(extract_jpl_n4, "Data/ERDDAP_Data/sst_JPL_n4.rds")

# Extract SST data using inverse distance squared interpolation for 16 cells.
extract_jpl_ID2_16 <- rxtracto(jplMURSST41_info,
                          parameter = parameter,
                          tcoord = tcoord,
                          xcoord = xcoord, ycoord = ycoord,
                          xlen = 0.0, ylen = 0.0,
                          interp = c("InverseDistance2", "16"),
                          progress_bar = TRUE)

# saveRDS(extract_jpl_ID2_16, "Data/ERDDAP_Data/sst_JPL_ID2_16.rds")

# Read data
extract_jpl <- readRDS("Data/ERDDAP_Data/sst_JPL.rds")
extract_jpl_n4 <- readRDS("Data/ERDDAP_Data/sst_JPL_near4.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_jpl_n4 = analysed_sst)
extract_jpl_ID2_16 <- readRDS("Data/ERDDAP_Data/sst_JPL_ID2_16.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_jpl_ID2_16 = analysed_sst)


# Extract relevant data
sst_jpl <- extract_jpl |> 
  as.data.frame() |> 
  mutate(
    site.id = site_data$site.id,
    date = as.Date(requested.date),
    sst_jpl = mean.analysed_sst,
    .keep = "none")

# ---- NOAA L3S-LEO ACSPO Daily Global 0.02° Gridded Super-collated SST ----

# Extract ERDDAP NOAA ACSPO satellite SST data
## noaacwLEOACSPOSSTL3SCDaily - Reanslysis ("Reserach grade"), *2 month delay*
# - Acknowledgement: These data were provided by Group for High Resolution Sea Surface Temperature (GHRSST) and the National Oceanic and Atmospheric Administration (NOAA). 
# - NOAA/NESDIS/STAR 
# - Sea-Surface Temperature, NOAA ACSPO Daily Global 0.02° Gridded Super-collated SST and Thermal Fronts Reanalysis, 2012-present, Daily (L3S-LEO degrees C) 
# - (https://coastwatch.noaa.gov/erddap/info/noaacwLEOACSPOSSTL3SCDaily/index.html)


## noaacwLEOACSPOSSTL3SnrtCDaily - Near real time
# - Sea-Surface Temperature, NOAA ACSPO Daily Global 0.02° Gridded Super-collated SST and Thermal Fronts, Near real-time, Daily (L3S-LEO degrees C)
# - (https://coastwatch.noaa.gov/erddap/griddap/noaacwLEOACSPOSSTL3SnrtCDaily.html)


# ReanalysisExtract metadata from NOAA ERDDAP - Reanalysis
noaacwLEOACSPOSSTL3SCDaily_info <-
  rerddap::info(
    "noaacwLEOACSPOSSTL3SCDaily",
    url="https://coastwatch.noaa.gov/erddap"
  )

# Extract metadata from NOAA ERDDAP - Near Real Time
noaacwLEOACSPOSSTL3SnrtCDaily_info <-
  rerddap::info(
    "noaacwLEOACSPOSSTL3SnrtCDaily",
    url="https://coastwatch.noaa.gov/erddap"
  )

# As of 12/22/25, noaacwLEOACSPOSSTL3SCDaily only includes data through 2025-10-10
reanalysis_date <- "2025-10-10"
site_data_ra <- filter(site_data, date <= reanalysis_date)
site_data_rt <- filter(site_data, date > reanalysis_date)

# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'sea_surface_temperature'
xcoord_ra <- site_data_ra$longitude
xcoord_rt <- site_data_rt$longitude
ycoord_ra <- site_data_ra$latitude
ycoord_rt <- site_data_rt$latitude
tcoord_ra <- site_data_ra$date
tcoord_rt <- site_data_rt$date

# Extract data from NOAA ERDDAP - Reanalysis
extract_acspo_ra <- rxtracto(noaacwLEOACSPOSSTL3SCDaily_info,
                    parameter = parameter,
                    tcoord = tcoord_ra,
                    xcoord = xcoord_ra, ycoord = ycoord_ra,
                    xlen = 0.0, ylen = 0.0,
                    # interp = c("Nearest", "4"),
                    progress_bar = TRUE)

# saveRDS(extract_acspo_ra, "Data/ERDDAP_Data/sst_ACSPO_ra.rds")
# saveRDS(extract_acspo_ra_0,05mean, "Data/ERDDAP_Data/sst_ACSPO_ra_0.05mean.rds")

# Extract SST data using nearest neighbor interpolation for 4 grid cells.
extract_acspo_ra_n4 <- rxtracto(noaacwLEOACSPOSSTL3SCDaily_info,
                       parameter = parameter,
                       tcoord = tcoord_ra,
                       xcoord = xcoord_ra, ycoord = ycoord_ra,
                       xlen = 0.0, ylen = 0.0,
                       interp = c("Nearest", "4"),
                       progress_bar = TRUE)

# saveRDS(extract_acspo_ra_n4, "Data/ERDDAP_Data/sst_ACSPO_ra_near4.rds")

# Extract SST data using inverse distance squared interpolation for 16 cells.
extract_acspo_ra_ID2_16 <- rxtracto(noaacwLEOACSPOSSTL3SCDaily_info,
                          parameter = parameter,
                          tcoord = tcoord_ra,
                          xcoord = xcoord_ra, ycoord = ycoord_ra,
                          xlen = 0.0, ylen = 0.0,
                          interp = c("InverseDistance2", "16"),
                          progress_bar = TRUE)

# saveRDS(extract_acspo_ra_ID2_16, "Data/ERDDAP_Data/sst_ACSPO_ra_ID2_16.rds")

# Read data
extract_acspo_ra <- readRDS("Data/ERDDAP_Data/sst_ACSPO_ra.rds")
extract_acspo_ra_n4 <- readRDS("Data/ERDDAP_Data/sst_ACSPO_ra_near4.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_acspo_n4 = sea_surface_temperature)
extract_acspo_ra_ID2_16 <- readRDS("Data/ERDDAP_Data/sst_ACSPO_ra_ID2_16.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_acspo_ID2_16 = sea_surface_temperature)


# Extract data from NOAA ERDDAP - Near Real Tine
extract_acspo_rt <- rxtracto(noaacwLEOACSPOSSTL3SnrtCDaily_info,
                    parameter = parameter,
                    tcoord = tcoord_rt,
                    xcoord = xcoord_rt, ycoord = ycoord_rt,
                    xlen = 0.0, ylen = 0.0,
                    # interp = c("Nearest", "4"),
                    progress_bar = TRUE)

# saveRDS(extract_acspo_rt, "Data/ERDDAP_Data/sst_ACSPO_rt.rds")
# saveRDS(extract_acspo_rt_0.05mean, "Data/ERDDAP_Data/sst_ACSPO_rt_0.05mean.rds")

# Extract SST data using nearest neighbor interpolation for 4 grid cells.
extract_acspo_rt_n4 <- rxtracto(noaacwLEOACSPOSSTL3SnrtCDaily_info,
                       parameter = parameter,
                       tcoord = tcoord_rt,
                       xcoord = xcoord_rt, ycoord = ycoord_rt,
                       xlen = 0.0, ylen = 0.0,
                       interp = c("Nearest", "4"),
                       progress_bar = TRUE)

# saveRDS(extract_acspo_rt_n4, "Data/ERDDAP_Data/sst_ACSPO_rt_near4.rds")

# Extract SST data using inverse distance squared interpolation for 16 cells.
extract_acspo_rt_ID2_16 <- rxtracto(noaacwLEOACSPOSSTL3SnrtCDaily_info,
                       parameter = parameter,
                       tcoord = tcoord_rt,
                       xcoord = xcoord_rt, ycoord = ycoord_rt,
                       xlen = 0.0, ylen = 0.0,
                       interp = c("InverseDistance", "16"),
                       progress_bar = TRUE)

# saveRDS(extract_acspo_rt_ID2_16, "Data/ERDDAP_Data/sst_ACSPO_rt_ID2_16.rds")

# Read data
extract_acspo_rt <- readRDS("Data/ERDDAP_Data/sst_ACSPO_rt.rds")
extract_acspo_rt_n4 <- readRDS("Data/ERDDAP_Data/sst_ACSPO_rt_near4.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_acspo_n4 = sea_surface_temperature)
extract_acspo_rt_ID2_16 <- readRDS("Data/ERDDAP_Data/sst_ACSPO_rt_ID2_16.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_acspo_ID2_16 = sea_surface_temperature)

# Extract relevant data
sst_acspo_ra <- extract_acspo_ra |> 
  as.data.frame() |> 
  mutate(
    site.id = site_data_ra$site.id,
    date = as.Date(requested.date),
    sst_acspo = mean.sea_surface_temperature,
    .keep = "none")

sst_acspo_rt <- extract_acspo_rt |> 
  as.data.frame() |> 
  mutate(
    site.id = site_data_rt$site.id,
    date = as.Date(requested.date),
    sst_acspo = mean.sea_surface_temperature,
    .keep = "none")

# Bind ACSPO data
sst_acspo <- bind_rows(sst_acspo_ra, sst_acspo_rt)
sst_acspo_n4 <- bind_rows(extract_acspo_ra_n4, extract_acspo_rt_n4)
sst_acspo_ID2_16 <- bind_rows(extract_acspo_ra_ID2_16, extract_acspo_rt_ID2_16)


# ---- NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp) ----

## Extract ERDDAP NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp)
### noaacrwsstDaily

# - Acknowledgement: NOAA Coral Reef Watch Program 
# - NOAA/NESDIS/STAR Coral Reef Watch Program 
# - Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily 
# - (https://coastwatch.noaa.gov/erddap/info/noaacrwsstDaily/index.html)

# Extract metadata from NOAA ERDDAP.
noaacrwsstDaily_info <-
  rerddap::info(
    "noaacrwsstDaily",
    url="https://coastwatch.noaa.gov/erddap"
  )

# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'analysed_sst'
xcoord <- site_data$longitude
ycoord <- site_data$latitude
tcoord <- site_data$date

# Extract data from NOAA ERDDAP
extract_crw <- rxtracto(noaacrwsstDaily_info,
                    parameter = parameter,
                    tcoord = tcoord,
                    xcoord = xcoord, ycoord = ycoord,
                    xlen = 0.0, ylen = 0.0,
                    progress_bar = TRUE)

# saveRDS(extract_crw, "Data/ERDDAP_Data/sst_crw.rds")
# saveRDS(extract_crw_0.05mean, "Data/ERDDAP_Data/sst_crw_0.05mean.rds")

# Extract SST data using nearest neighbor interpolation for 4 grid cells.
extract_crw_n4 <- rxtracto(noaacrwsstDaily_info,
                        parameter = parameter,
                        tcoord = tcoord,
                        xcoord = xcoord, ycoord = ycoord,
                        xlen = 0.0, ylen = 0.0,
                        interp = c("Nearest", "4"),
                        progress_bar = TRUE)

saveRDS(extract_crw_n4, "Data/ERDDAP_Data/sst_crw_near4.rds")

# extract_crw_ID2_16 <- rxtracto(noaacrwsstDaily_info,
#                         parameter = parameter,
#                         tcoord = tcoord,
#                         xcoord = xcoord, ycoord = ycoord,
#                         xlen = 0.0, ylen = 0.0,
#                         interp = c("InverseDistance", "16"),
#                         progress_bar = TRUE)
# 
# saveRDS(extract_crw_ID2_16, "Data/ERDDAP_Data/sst_crw_ID2_16.rds")

# Read data
extract_crw <- readRDS("Data/ERDDAP_Data/sst_crw.rds")
extract_crw_n4 <- readRDS("Data/ERDDAP_Data/sst_crw_near4.rds") |> 
  as.data.frame() |> 
  rename(date = time,
         sst_crw_n4 = analysed_sst)

# Extract relevant data
sst_crw <- extract_crw |> 
  as.data.frame() |> 
  mutate(
    site.id = site_data$site.id,
    date = as.Date(requested.date),
    sst_crw = mean.analysed_sst,
    .keep = "none")


# ---- NOAA Geo-polar Blended Analysis Day+Night, GHRSST, Near Real-Time, Global 5km, 2019-Present, Daily ----

## Extract ERDDAP satellite SST data 
### noaacwBLENDEDsstDNDaily

# - Acknowledgement: NOAA/NESDIS
# - Sea-Surface Temperature, NOAA Geo-polar Blended Analysis Day+Night, GHRSST, Near Real-Time,
#   Global 5km, 2019-Present, Daily
# - The Geo-Polar Blended Sea Surface Temperature (SST) Analysis combines multi-satellite
#   retrievals of sea surface temperature into a single analysis of SST. This analysis
#   includes only nighttime data.
# - (https://coastwatch.noaa.gov/erddap/info/noaacwBLENDEDsstDNDaily/index.html)

noaacwBLENDEDsstDNDaily_info <-
  rerddap::info("noaacwBLENDEDsstDNDaily",
                url="https://coastwatch.noaa.gov/erddap"
  )

# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'analysed_sst'
xcoord <- site_data$longitude
ycoord <- site_data$latitude
tcoord <- site_data$date

# Extract SST data with no interpolation
extract_gpbcw <- rxtracto(noaacwBLENDEDsstDNDaily_info,
                          parameter = parameter,
                          tcoord = tcoord,
                          xcoord = xcoord, ycoord = ycoord,
                          xlen = 0.0, ylen = 0.0,
                          progress_bar = TRUE)

# saveRDS(extract_gpbcw, "Data/ERDDAP_Data/sst_gpbcw.rds")

# Read data
extract_gpbcw <- readRDS("Data/ERDDAP_Data/sst_gpbcw.rds")

# Extract relevant data
sst_gpbcw <- extract_gpbcw |>
  as.data.frame() |> 
  mutate(
    site.id = site_data$site.id,
    date = as.Date(requested.date),
    sst_gpbcw = mean.analysed_sst,
    .keep = "none")


# ---- NOAA Geo-polar Blended Analysis Diurnal Correction (Day+Night), GHRSST, Near Real-Time, Global 5km, 2019-Present, Daily ----

## Extract ERDDAP satellite SST data 
### noaacwBLENDEDsstDLDaily

# - Acknowledgement: NOAA/NESDIS
# - Sea-Surface Temperature, NOAA Geo-polar Blended Analysis Diurnal Correction (Day+Night),
#   GHRSST, Near Real-Time, Global 5km, 2019-Present, Daily
# - The Geo-Polar Blended Sea Surface Temperature (SST) Analysis combines multi-satellite
#   retrievals of sea surface temperature into a single analysis of SST. This analysis
#   includes only nighttime data.
# - (https://coastwatch.noaa.gov/erddap/info/noaacwBLENDEDsstDLDaily/index.html)

noaacwBLENDEDsstDLDaily_info <-
  rerddap::info("noaacwBLENDEDsstDLDaily",
                url="https://coastwatch.noaa.gov/erddap"
  )

# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'analysed_sst'
xcoord <- site_data$longitude
ycoord <- site_data$latitude
tcoord <- site_data$date

# Extract SST data with no interpolation
extract_gpbcw_dc <- rxtracto(noaacwBLENDEDsstDLDaily_info,
                            parameter = parameter,
                            tcoord = tcoord,
                            xcoord = xcoord, ycoord = ycoord,
                            xlen = 0.0, ylen = 0.0,
                            progress_bar = TRUE)

# saveRDS(extract_gpbcw_dc, "Data/ERDDAP_Data/sst_gpbcw_dc.rds")

# Read data
extract_gpbcw_dc <- readRDS("Data/ERDDAP_Data/sst_gpbcw_dc.rds")

# Extract relevant data
sst_gpbcw_dc <- extract_gpbcw_dc |>
  as.data.frame() |> 
  mutate(
    site.id = site_data$site.id,
    date = as.Date(requested.date),
    sst_gpbcw_dc = mean.analysed_sst,
    .keep = "none")


# ---- ACSPO Global SST from ABI ----

# Create function to identifies the index of coordinate nearest to a given target coordinate
nearest_index <- function(coord_list, target_coord) {
  which.min((coord_list - target_coord)^2)
}

# Set data URL from OPeNDAP Dataset Access Form
# The date of this URL is arbitrary, but is used to access the lat/lon coords for this overall data set
URL <- "https://www.star.nesdis.noaa.gov/thredds/dodsC/gridG16ABIRANL3CWW00/2025/019/20250119000000-STAR-L3C_GHRSST-SSTsubskin-ABI_G16-ACSPO_V2.90-v02.0-fv01.0.nc"

# Open .nc file
nc <- nc_open(URL)

# Extract all longitude and latitude values as numeric vectors
lats <- ncvar_get(nc, "lat")
lons <- ncvar_get(nc, "lon")

# Create data frame with closeset lat and lons for each site
site_coords_acspo_abi <- site_data |>
  select(site.id, latitude, longitude) |>
  distinct() |>
  mutate(
    lat_idx = map_int(latitude,  ~ nearest_index(lats, .x)),
    lon_idx = map_int(longitude, ~ nearest_index(lons, .x)),
    # Determine closest lat and lons for each site
    lat_nearest = lats[lat_idx],
    lon_nearest = lons[lon_idx]
  )

# Close .nc file
nc_close(nc)

# Create function that builds hourly OPeNDAP Data URL
make_url <- function(dt_utc) {
  yr    <- format(dt_utc, "%Y")
  jday  <- sprintf("%03d", yday(dt_utc))
  stamp <- format(dt_utc, "%Y%m%d%H%M%S")
  
  paste0(
    "https://www.star.nesdis.noaa.gov/thredds/dodsC/gridG16ABIRANL3CWW00/",
    yr, "/", jday, "/",
    stamp, "-STAR-L3C_GHRSST-SSTsubskin-ABI_G16-ACSPO_V2.90-v02.0-fv01.0.nc"
  )
}

# Establish start and end days for data collection
# start_day <- min(as.Date(site_data$date))
start_day <- as.Date("2024-09-04")
# The first full day of GOES 19 is 2025-04-03, so ending GOES 16 request after 2025-04-02
# end_day <- as.Date("2025-04-03")
end_day <- start_day + 30

# Create sequence of hours through which to iterate
hours_seq <- seq(
  from = as.POSIXct(start_day, tz = "UTC"),
  to   = as.POSIXct(end_day, tz = "UTC") - hours(1),
  by   = "1 hour"
)

# Preallocate a list to store one data frame per hourly file
all_hourly <- vector("list", length(hours_seq))

# Store the number of hours for this run in n_hours
n_hours <- length(hours_seq)

# Iterate through each hour in the hourly sequence
for (h in seq_along(hours_seq)) {
  
  dt  <- hours_seq[h]
  # Build the URL based on the date & hour
  url <- make_url(dt)
  
  #Build message to display to keep track of progress
  message(sprintf("[%d / %d] %s",
                  h, n_hours, format(dt, "%Y-%m-%d %H:%M UTC")))
  
  # Open nc file
  nc <- tryCatch(nc_open(url), error = function(e) NULL)
  if (is.null(nc)) {
    message("   -> failed to open")
    next
  }
  
  # Extract SST for every site (lon, lat, time=1)
  sst_K <- vapply(seq_len(nrow(site_coords_acspo_abi)), function(i) {
    tryCatch(
      ncvar_get(
        nc,
        "sea_surface_temperature",
        start = c(site_coords_acspo_abi$lon_idx[i], site_coords_acspo_abi$lat_idx[i], 1L),
        count = c(1L, 1L, 1L)
      ),
      error = function(e) NA_real_
    )
  }, numeric(1))
  
  nc_close(nc)
  
  # Store hourly sst df and convert K to Celsius
  all_hourly[[h]] <- site_coords_acspo_abi |>
    mutate(
      datetime_utc = dt,
      date = as.Date(dt),
      sst_C = as.numeric(sst_K) - 273.15
    ) |>
    select(site.id, latitude, longitude, lat_nearest, lon_nearest,
           datetime_utc, date, sst_C)
}

# Create hourly df from list of dfs
sst_hourly_all_sites <- bind_rows(all_hourly)
# saveRDS(sst_hourly_all_sites, "Data/ACSPO_ABI_GOES/sst_hourly_all_sites/20240904-20241003.rds")

# Calculate daily mean sst
sst_daily <- sst_hourly_all_sites |>
  group_by(site.id, latitude, longitude, lat_nearest, lon_nearest, date) |>
  summarise(
    sst_daily_mean_C = mean(sst_C, na.rm = TRUE),
    n_hours = sum(!is.na(sst_C)),
    .groups = "drop"
  )

sst_daily
# saveRDS(sst_daily, "Data/ACSPO_ABI_GOES/sst_daily/20240904-20241003.rds")


# ---- Join all data ----

# # Join data with site_data
# site_sst <- site_data |> 
#   left_join(sst_jpl, by = c("site.id", "date")) |> 
#   left_join(sst_acspo, by = c("site.id", "date")) |> 
#   left_join(sst_crw, by = c("site.id", "date")) |> 
#   select(-c(min.T, max.T, delta.T)) |> 
#   mutate(date = as.Date(date))

# Write data
# write.csv(site_sst, "Data/site_sst.csv", row.names = FALSE)
# saveRDS(site_sst, "Data/site_sst.rds")
# write.csv(site_sst, "Data/site_sst_0.05mean.csv", row.names = FALSE)
# saveRDS(site_sst, "Data/site_sst_0.05mean.rds")

# Read data
# site_sst <- read.csv("Data/site_sst.csv") |> mutate(date = as.Date(date))
# site_sst <- readRDS("Data/site_sst.rds")
# site_sst <- read.csv("Data/site_sst_0.05mean.csv") |> mutate(date = as.Date(date))
# site_sst <- readRDS("Data/site_sst_0.05mean.rds")


# # Join data with site_data - data set specific
sst_hobo_sites <- site_data |> 
  mutate(date = as.Date(date)) |> 
  select(-c(min.T, max.T, delta.T)) |> 
  # JPL
  left_join(sst_jpl, by = c("site.id", "date")) |> 
  left_join(extract_jpl_n4, by = c("latitude", "longitude", "date")) |> 
  left_join(extract_jpl_ID2_16, by = c("latitude", "longitude", "date")) |> 
  group_by(site.id) |> 
  arrange(date, .by_group = TRUE) |> 
  mutate(
    sst_jpl_interp = na.approx(
      coalesce(sst_jpl, sst_jpl_n4, sst_jpl_ID2_16),
      na.rm = FALSE
    )
  ) |> 
  ungroup() |> 
  # ACSPO
  left_join(sst_acspo, by = c("site.id", "date")) |> 
  left_join(sst_acspo_n4, by = c("latitude", "longitude", "date")) |> 
  left_join(sst_acspo_ID2_16, by = c("latitude", "longitude", "date")) |> 
  group_by(site.id) |> 
  arrange(date, .by_group = TRUE) |> 
  mutate(
    sst_acspo_interp = na.approx(
      coalesce(sst_acspo, sst_acspo_n4, sst_acspo_ID2_16),
      na.rm = FALSE
    )
  ) |> 
  ungroup() |>  
  # CRW
  left_join(sst_crw, by = c("site.id", "date")) |> 
  left_join(extract_crw_n4, by = c("latitude", "longitude", "date")) |> 
  group_by(site.id) |> 
  arrange(date, .by_group = TRUE) |> 
  mutate(
    sst_crw_interp = na.approx(
      coalesce(sst_crw, sst_crw_n4),
      na.rm = FALSE
    )
  ) |> 
  ungroup() |> 
  # gpbcw
  left_join(sst_gpbcw, by = c("site.id", "date")
  ) |> 
  # gpbcw_dc
  left_join(sst_gpbcw_dc, by = c("site.id", "date")
  )

# Write data
# saveRDS(sst_hobo_sites, "Data/sst_hobo_sites.rds")
# Read data
sst_hobo_sites <- readRDS("Data/sst_hobo_sites.rds")


# Visualize data
visdat::vis_dat(sst_hobo_sites) +
  theme(
    axis.text.x = element_text(
      angle = 90, 
      vjust = 0.3
    )
  )


