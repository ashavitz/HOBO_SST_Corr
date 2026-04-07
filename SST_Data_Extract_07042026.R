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
## Using the following sites, and not including HOBO data:
# Sites "AQ" "CB" "DC" "SH" "WB" "CC" "OB" "WC" "GB" "NK" "JB" "CL" "PV" "NB"
# sites <- c("AQ", "CB", "DC", "SH", "WB", "CC", "OB", "WC", "GB", "NK", "JB", "CL", "PV", "NB")
sites <- c("PV")
site_data <- read.csv("Data/site_coords.csv") |>
  filter(site.id %in% sites)

date_range <- seq(as.Date("2024-04-16"), as.Date("2024-06-22"), by = "day")

# Create the full grid of sites and dates
sites_date_range <- expand_grid(
  site.id = sites, 
  date = as.character(date_range)
)

# Join back your latitude and longitude
site_data <- site_data |> 
  left_join(sites_date_range, by = "site.id")

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

# # saveRDS(extract_jpl, "Data/ERDDAP_Data/sst_JPL.rds")
# # saveRDS(extract_jpl_0.05mean, "Data/ERDDAP_Data/sst_JPL_0.05mean.rds")
# 
# # Extract SST data using nearest neighbor interpolation for 4 grid cells.
# extract_jpl_n4 <- rxtracto(jplMURSST41_info,
#                            parameter = parameter,
#                            tcoord = tcoord,
#                            xcoord = xcoord, ycoord = ycoord,
#                            xlen = 0.0, ylen = 0.0,
#                            interp = c("Nearest", "4"),
#                            progress_bar = TRUE)
# 
# # saveRDS(extract_jpl_n4, "Data/ERDDAP_Data/sst_JPL_n4.rds")
# 
# # Extract SST data using inverse distance squared interpolation for 16 cells.
# extract_jpl_ID2_16 <- rxtracto(jplMURSST41_info,
#                                parameter = parameter,
#                                tcoord = tcoord,
#                                xcoord = xcoord, ycoord = ycoord,
#                                xlen = 0.0, ylen = 0.0,
#                                interp = c("InverseDistance2", "16"),
#                                progress_bar = TRUE)
# 
# # saveRDS(extract_jpl_ID2_16, "Data/ERDDAP_Data/sst_JPL_ID2_16.rds")
# 
# # Read data
# extract_jpl <- readRDS("Data/ERDDAP_Data/sst_JPL.rds")
# extract_jpl_n4 <- readRDS("Data/ERDDAP_Data/sst_JPL_near4.rds") |> 
#   as.data.frame() |> 
#   rename(date = time,
#          sst_jpl_n4 = analysed_sst)
# extract_jpl_ID2_16 <- readRDS("Data/ERDDAP_Data/sst_JPL_ID2_16.rds") |> 
#   as.data.frame() |> 
#   rename(date = time,
#          sst_jpl_ID2_16 = analysed_sst)


# Extract relevant data
sst_jpl <- extract_jpl |> 
  as.data.frame() |> 
  mutate(
    site.id = site_data$site.id,
    date = as.Date(requested.date),
    sst_jpl = mean.analysed_sst,
    .keep = "none")



write.csv(sst_jpl, "Data/ERDDAP_Data/sst_JPL_PV_1.csv")
