# ---- Packages ----
library(tidyr)
library(dplyr)
library(lubridate)
library(hms)

# ---- Data upload Hobos (temp/light) for WHOI sites ----
# AQ
d.t.AQ.1 <- read.csv("Data/Hobo/AQ_21177827.csv") |>
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.AQ.2 <- read.csv("Data/Hobo/AQ_21258480.csv") |> 
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.AQ.3 <- read.csv("Data/Hobo/AQ_10625902.csv") |> 
  mutate(site.id = "AQ",
         date.deployment = date(mdy_hm(first(date.time))))

# CB
d.t.CB.1 <- read.csv("Data/Hobo/CB_21258498.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.CB.2 <- read.csv("Data/Hobo/CB_21258495.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.CB.3 <- read.csv("Data/Hobo/CB_21258487.csv") |> 
  mutate(site.id = "CB",
         date.deployment = date(mdy_hm(first(date.time))))

# CI
d.t.CI.1 <- read.csv("Data/Hobo/CI_20536645.csv") |> 
  mutate(site.id = "CI",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.CI.2 <- read.csv("Data/Hobo/CI_21982358.csv") |> 
  mutate(site.id = "CI",
         date.deployment = date(mdy_hm(first(date.time))))

# DC
d.t.DC.1 <- read.csv("Data/Hobo/DC_21258490.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.DC.2 <- read.csv("Data/Hobo/DC_21258491.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.DC.3 <- read.csv("Data/Hobo/DC_21258483.csv") |> 
  mutate(site.id = "DC",
         date.deployment = date(mdy_hm(first(date.time))))

# LH
d.t.LH.1 <- read.csv("Data/Hobo/LH_10942817.csv") |> 
  mutate(site.id = "LH",
         date.deployment = date(mdy_hm(first(date.time))))

# PC
d.t.PC.1 <- read.csv("Data/Hobo/PC_21177829.csv") |> 
  mutate(site.id = "PC",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.PC.2 <- read.csv("Data/Hobo/PC_21177827.csv") |> 
  mutate(site.id = "PC",
         date.deployment = date(mdy_hm(first(date.time))))

# SH
d.t.SH.1 <- read.csv("Data/Hobo/SH_21177828.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.SH.2 <- read.csv("Data/Hobo/SH_21258490.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))
d.t.SH.3 <- read.csv("Data/Hobo/SH_21982356.csv") |> 
  mutate(site.id = "SH",
         date.deployment = date(mdy_hm(first(date.time))))

# WB
d.t.WB.0 <- read.csv("Data/Hobo/May2024_21258496_WB.csv") |> 
  mutate(site.id = "WB",
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


# ---- Calculate Summary Statistics  ----
# Bind all data and include only days 2 through 14 post deployment
# (Exclude day 1 to exclude accidental measurements taken prior to deployment)
light_data <- bind_rows(
  d.t.AQ.1, d.t.AQ.2, d.t.AQ.3,
  d.t.CB.1, d.t.CB.2, d.t.CB.3,
  d.t.CI.1, d.t.CI.2,
  d.t.DC.1, d.t.DC.2, d.t.DC.3,
  d.t.LH.1,
  d.t.PC.1, d.t.PC.2,
  d.t.SH.1, d.t.SH.2, d.t.SH.3,
  d.t.WB.0, d.t.WB.1, d.t.WB.2, d.t.WB.3
) |>
  select(-c("reading.no", "temp.C")) |> 
  mutate(date.time = mdy_hm(date.time),
         date = as.Date(date.time)) |>
  group_by(site.id, date.deployment) |> 
  filter(date <= first(date) + 13) |>
  filter(date > first(date)) |> 
  ungroup()

# Calculate summary statistics for each HOBO deployment
light_data_summary <- light_data |> 
  group_by(site.id, date.deployment) |> 
  summarize(
    mean.lum.per.sqrft = mean(Intensity.lum.per.sqft),
    med.lum.per.sqrft = median(Intensity.lum.per.sqft),
    min.lum.per.sqrft = min(Intensity.lum.per.sqft),
    max.lum.per.sqrft = max(Intensity.lum.per.sqft),
    .groups = ("drop")
  )
  
# Filter light data to only include daytime values, between 6am and 6pm, to avoid the majority of nighttime 0 values
light_data_6am_6pm <- light_data |>
  filter(
    as_hms(date.time) >= as_hms("06:00:00"),
    as_hms(date.time) <= as_hms("18:00:00")
  )

# Calculate summary statistics for each HOBO deployment, including only values between 6am and 6pm 
light_data_summary_6am_6pm <- light_data_6am_6pm |> 
  group_by(site.id, date.deployment) |> 
  summarize(
    mean.lum.per.sqrft = mean(Intensity.lum.per.sqft),
    med.lum.per.sqrft = median(Intensity.lum.per.sqft),
    min.lum.per.sqrft = min(Intensity.lum.per.sqft),
    max.lum.per.sqrft = max(Intensity.lum.per.sqft),
    .groups = ("drop")
  )


# ---- Export Data ----
write.csv(light_data_summary, "Data/Hobo/Light_Data_Summary/whoi_hobo_light_summary.csv")
write.csv(light_data_summary_6am_6pm, "Data/Hobo/Light_Data_Summary/whoi_hobo_light_summary_6am_6pm.csv")
