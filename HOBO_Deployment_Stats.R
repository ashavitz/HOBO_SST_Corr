


# ---- Packages ----
library(tidyr)
library(dplyr)
library(lubridate)


# ---- Data upload Hobos (temp/light) for sites with full 2025 data ----
d.t.AQ.1 <- read.csv("Data/Hobo/AQ_21177827.csv")



# ---- Data upload Hobos (temp/light) for sites with full 2025 data ----
d.t.AQ.1 <- read.csv("Data/Hobo/AQ_21177827.csv") |>
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))
         
  #        ,
  #        date.time = mdy_hm(date.time),
  #        date = as.Date(date.time)) |>
  # filter(date <= first(date) + 13) |> 
  # filter(date > first(date))

d.t.AQ.2 <- read.csv("Data/Hobo/AQ_21258480.csv") |> 
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.AQ.3 <- read.csv("Data/Hobo/AQ_10625902.csv") |> 
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))

d.t.CB.1 <- read.csv("Data/Hobo/CB_21258498.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.CB.2 <- read.csv("Data/Hobo/CB_21258495.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.CB.3 <- read.csv("Data/Hobo/CB_21258487.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))

d.t.DC.1 <- read.csv("Data/Hobo/DC_21258490.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.DC.2 <- read.csv("Data/Hobo/DC_21258491.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.DC.3 <- read.csv("Data/Hobo/DC_21258483.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))

d.t.SH.1 <- read.csv("Data/Hobo/SH_21177828.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.SH.2 <- read.csv("Data/Hobo/SH_21258490.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.SH.3 <- read.csv("Data/Hobo/SH_21982356.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))

d.t.WB.1 <- read.csv("Data/Hobo/WB_21177828.csv") |> 
  mutate(site.id = "WB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.WB.2 <- read.csv("Data/Hobo/WB_21258496.csv") |> 
  mutate(site.id = "WB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.WB.3 <- read.csv("Data/Hobo/WB_22288916.csv") |> 
  mutate(site.id = "WB",
         date.deployment = date(mdy_hm(first(date.time))))

# Bind all data and include only days 2 through 13 post deployment
light_data <- bind_rows(
  d.t.AQ.1,
  d.t.AQ.2,
  d.t.AQ.3,
  d.t.CB.1,
  d.t.CB.2,
  d.t.CB.3,
  d.t.DC.1,
  d.t.DC.2,
  d.t.DC.3,
  d.t.SH.1,
  d.t.SH.2,
  d.t.SH.3,
  d.t.WB.1,
  d.t.WB.2,
  d.t.WB.3
) |>
  select(-c("reading.no", "temp.C"))
  mutate(date.time = mdy_hm(date.time),
         date = as.Date(date.time)) |>
  group_by(site.id, date.deployment) |> 
  filter(date <= first(date) + 13) |>
  filter(date > first(date)) |> 
  ungroup()


light_data_summary <- light_data |> 
  group_by(site.id, date.deployment) |> 
  summarize(
    mean.lum.per.sqrft = mean(Intensity.lum.per.sqft),
    .groups = ("drop")
  )
  
  


# ---- Data Carpentry Hobos ----





# Determine deployment date(s) for each site

# Filter only days 2-14 post-deployment, for each site

# Calculate summary statistics (mean, median, min, max) for each deployment

# Columns: Site ID, Deployment Instance, Deployment Date, Start Date, End Date, Min, Max, Mean, Median