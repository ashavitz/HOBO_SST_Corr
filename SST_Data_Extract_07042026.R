# Purpose: Extract daily SST from NASA JPL MUR via ERDDAP (rerddapXtracto).
#          Runs in weekly chunks for robustness and saves incrementally as CSVs.
#          Output directory and filenames are dynamic based on selected site(s).

# ---- Load Packages ----
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(rerddap)
library(rerddapXtracto)

# ---- User Inputs ----
sites <- c("NB")  # change this to any site(s)
date_range <- seq(as.Date("2024-04-16"), as.Date("2025-10-22"), by = "day")

# Create a label for folder/file naming (e.g., "PV" or "PV_WB_CB")
site_label <- paste(sites, collapse = "_")

# ---- Prepare Site + Date Grid ----
site_coords <- read.csv("Data/site_coords.csv") |>
  filter(site.id %in% sites)

site_data <- expand_grid(
  site.id = sites,
  date = date_range
) |>
  left_join(site_coords, by = "site.id") |>
  mutate(date = as.Date(date))

# ---- ERDDAP Dataset Info ----
jplMURSST41_info <- rerddap::info(
  "jplMURSST41",
  url = "https://coastwatch.pfeg.noaa.gov/erddap"
)

# ---- Output Directory ----
out_dir <- file.path("Data/ERDDAP_Data/JPL", paste0("jpl_chunks_", site_label))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Define Weekly Chunks ----
chunk_starts <- seq.Date(min(site_data$date), max(site_data$date), by = "7 days")

# ---- Loop Over Chunks ----
for (i in seq_along(chunk_starts)) {
  
  start_date <- chunk_starts[i]
  end_date <- min(start_date + 6, max(site_data$date))
  
  file_name <- file.path(
    out_dir,
    sprintf(
      "sst_%s_%s.csv",
      format(start_date, "%Y%m%d"),
      format(end_date, "%Y%m%d")
    )
  )
  
  if (file.exists(file_name)) next  # skip completed chunks
  
  chunk_data <- site_data |>
    filter(date >= start_date, date <= end_date)
  
  tryCatch({
    
    out <- rxtracto(
      jplMURSST41_info,
      parameter = "analysed_sst",
      tcoord = as.character(chunk_data$date),
      xcoord = chunk_data$longitude,
      ycoord = chunk_data$latitude,
      xlen = 0,
      ylen = 0,
      progress_bar = TRUE
    )
    
    out_df <- as.data.frame(out) |>
      mutate(
        site.id = chunk_data$site.id,
        date = as.Date(requested.date),
        sst_jpl = round(mean.analysed_sst, 3), # precision is only 3 decimal places
        .keep = "none"
      )
    
    readr::write_csv(out_df, file_name)
    message("saved: ", file_name)
    
  }, error = function(e) {
    message("failed: ", file_name, " -- ", e$message)
  })
}

# ---- Combine All Chunks ----
sst_jpl <- list.files(
  out_dir,
  full.names = TRUE,
  pattern = "\\.csv$"
) |>
  map_dfr(readr::read_csv, show_col_types = FALSE)

# Optional: save combined file
# readr::write_csv(sst_jpl, file.path(out_dir, paste0("sst_jpl_", site_label, "_all.csv")))