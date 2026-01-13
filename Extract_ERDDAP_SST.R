# Packages
# ERRDAP Data Access
library(rerddap) # For reading ERDDAP data
library(rerddapXtracto)

#---- NASA JPL Multi-scale Ultra-high Resolution (MUR) SST ----
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
info_obj <-
  rerddap::info("jplMURSST41",
                url="https://coastwatch.pfeg.noaa.gov/erddap"
                )

sites <- d.t.daily |> distinct(site.id, latitude, longitude)
t0 <- as.character(min(d.t.daily$date))
t1 <- as.character(max(d.t.daily$date))

fetch_site_sst_csv <- function(site_id, lat, lon, t0, t1) {
  out <- griddap(
    info_obj,
    field = "analysed_sst",
    time = c(t0, t1),
    latitude = c(lat, lat), 
    longitude = c(lon, lon),
    fmt = "csv"
  )
  
  df <- out$data
  
  df |>
    transmute(site.id = site_id,
              date = as.Date(time),
              analysed_sst = analysed_sst
    )
}


# Make it robust: don't fail the whole run if one site errors
safe_fetch <- safely(fetch_site_sst_csv, otherwise = tibble(
  site.id = character(), date = as.Date(character()), analysed_sst = numeric()
))

sst_by_site <- pmap(
  list(sites$site)
)













# set parameters for use in rerddapXtracto::rxtracto()
parameter <- 'analysed_sst'
xcoord <- d.t.daily$longitude
ycoord <- d.t.daily$latitude
tcoord <- d.t.daily$date

# NOTE:
# - rxtracto() returns a list of statistics around the points provided via xcoord and ycoord
# - xlen and ylen denote the size (degrees) of the surrounding box from which you
#   want statistics. The default for these is 0, which will extract data for the grid cell
#   in which each coordinate provided falls.
# - Using xlen = 0.1 and ylen = 0.1 below, which will extract a mean sst from a square of
#   grid cells around each hobo buoy. This is helpful for when data is missing in the exact
#   grid cell the buoy falls within. An alternative option to consider is pulling sst for
#   the single containing grid cell, and only extracting an average of surrounding cells for
#   those buoys that return NA data.

extract <- rxtracto(jplMURSST41_info,
                    parameter = parameter,
                    tcoord = tcoord,
                    xcoord = xcoord, ycoord = ycoord,
                    xlen = 0.0, ylen = 0.0,
                    progress_bar = TRUE)

mean.SST_jpl <- extract$`mean analysed_sst`

# Export merged data to local files. (To avoid delays with ERDDAP access in the future.
# User can run the code above to re-download the latest data.
saveRDS(extract$`mean analysed_sst`, "Data/ERDDAP_Data/sst_JPL.rds")