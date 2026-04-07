# Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpmisc) # for annotating plots with p & R2 of fitted polynomial via stat_poly_eq()
library(performance)

# Read in site and satellite data (sst_hobo_sites.rds)
temp_data_daily <- readRDS("Data/sst_hobo_sites.rds") |> 
  select(site.id, latitude, longitude, date, median.T, sst_jpl_interp, sst_acspo_interp, sst_crw) |> 
  rename(
    sst_jpl = sst_jpl_interp,
    sst_leol3s_acspo = sst_acspo_interp
  ) |> 
  # Filter out sites where Coral Reef Watch sst column has no data
  filter(!is.na(sst_crw))


# Compare daily values of interpolated data sets to HOBO data. Determine which has tightest correlation.

# Plot HOBO and JPL satellite SST against each other
ggplot(temp_data_daily, aes(x = sst_jpl, y = median.T)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# Plot HOBO and LEO_L3S ACSPO satellite SST against each other
ggplot(temp_data_daily, aes(x = sst_leol3s_acspo, y = median.T)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# Plot HOBO and CRW satellite SST against each other
ggplot(temp_data_daily, aes(x = sst_crw, y = median.T)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# Calculate 2025 GDD at Tbase = 5 degrees
temp_data_daily_2025 <- temp_data_daily |>
  # Filter data to post 2024 to calculate 2025 GDD
  filter(date >= "2025-01-01") |> 
  # Calculate GDD for each data point
  mutate(
    gdd.hobo.5 = pmax(median.T - 5, 0),
    gdd.jpl.5 = pmax(sst_jpl - 5, 0),
    gdd.leol3s_acspo.5 = pmax(sst_leol3s_acspo - 5, 0),
    gdd.crw.5 = pmax(sst_crw - 5, 0)
  ) |> 
  # Calculate GDD for each data set
  group_by(site.id) |> 
  mutate(
    gdd.hobo.5 = cumsum(gdd.hobo.5),
    gdd.jpl.5 = cumsum(gdd.jpl.5),
    gdd.leol3s_acspo.5 = cumsum(gdd.leol3s_acspo.5),
    gdd.crw.5 = cumsum(gdd.crw.5),
  ) |> 
  ungroup()

# Compare GDD of sat data to HOBO data. Determine which has tightest correlation.
#JPL
ggplot(temp_data_daily_2025, aes(x = gdd.jpl.5, y = gdd.hobo.5)) +
  geom_point(color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# LEO_L3S ACSPO
ggplot(temp_data_daily_2025, aes(x = gdd.leol3s_acspo.5, y = gdd.hobo.5)) +
  geom_point(color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# CRW
ggplot(temp_data_daily_2025, aes(x = gdd.crw.5, y = gdd.hobo.5)) +
  geom_point(color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))



# Filter out sites without both bathymetry and tidal range data 
temp_data_daily_2025_MA <- temp_data_daily_2025 |> 
  filter(!is.na(depth_m), !is.na(tidal_range_FT))

# Compare GDD of sat data to HOBO data, including depth data. Determine which has tightest correlation.
#JPL
ggplot(temp_data_daily_2025_MA, aes(x = gdd.jpl.5, y = gdd.hobo.5, color = depth_m)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# LEO_L3S ACSPO
ggplot(temp_data_daily_2025_MA, aes(x = gdd.leol3s_acspo.5, y = gdd.hobo.5, color = depth_m)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# CRW
ggplot(temp_data_daily_2025_MA, aes(x = gdd.crw.5, y = gdd.hobo.5, color = depth_m)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))


######


# Filter out sites without tidal range data 
temp_data_daily_2025_t <- temp_data_daily_2025 |> 
  filter(!is.na(tidal_range_FT))

# Compare GDD of sat data to HOBO data, including tidal range data. Determine which has tightest correlation.
#JPL
ggplot(temp_data_daily_2025_t, aes(x = gdd.jpl.5, y = gdd.hobo.5, color = tidal_range_FT)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# LEO_L3S ACSPO
ggplot(temp_data_daily_2025_t, aes(x = gdd.leol3s_acspo.5, y = gdd.hobo.5, color = tidal_range_FT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# CRW
ggplot(temp_data_daily_2025_t, aes(x = gdd.crw.5, y = gdd.hobo.5, color = tidal_range_FT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))


# Models with only sat temp
temp_lm_1 <- lm(gdd.hobo.5 ~ gdd.jpl.5, data = temp_data_daily_2025)
summary(temp_lm_1)
temp_lm_2 <- lm(gdd.hobo.5 ~ gdd.leol3s_acspo.5, data = temp_data_daily_2025)
summary(temp_lm_2)
temp_lm_3 <- lm(gdd.hobo.5 ~ gdd.crw.5, data = temp_data_daily_2025)
summary(temp_lm_3)

# Factor only depth into models
temp_lm_d1 <- lm(gdd.hobo.5 ~ gdd.jpl.5 + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_d1)
temp_lm_d2 <- lm(gdd.hobo.5 ~ gdd.leol3s_acspo.5 + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_d2)
temp_lm_d3 <- lm(gdd.hobo.5 ~ gdd.crw.5 + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_d3)

# Factor only tidal exchange into models
temp_lm_t1 <- lm(gdd.hobo.5 ~ gdd.jpl.5 + tidal_range_FT, data = temp_data_daily_2025_t)
summary(temp_lm_t1)
temp_lm_t2 <- lm(gdd.hobo.5 ~ gdd.leol3s_acspo.5 + tidal_range_FT, data = temp_data_daily_2025_t)
summary(temp_lm_t2)
temp_lm_t3 <- lm(gdd.hobo.5 ~ gdd.crw.5 + tidal_range_FT, data = temp_data_daily_2025_t)
summary(temp_lm_t3)

# Factor tidal exchange and depth into models 
temp_lm_td1 <- lm(gdd.hobo.5 ~ gdd.jpl.5 + tidal_range_FT + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_td1)
temp_lm_td2 <- lm(gdd.hobo.5 ~ gdd.leol3s_acspo.5 + tidal_range_FT + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_td2)
temp_lm_td3 <- lm(gdd.hobo.5 ~ gdd.crw.5 + tidal_range_FT + depth_m, data = temp_data_daily_2025_MA)
summary(temp_lm_td3)


# TODO Complete draft plots of lms

# Check models of interest

check_model(temp_lm_t2)

# Plot + label for temp_lm_t2
s <- summary(temp_lm_t2)
b <- coef(temp_lm_t2)

label_txt <- paste0(
  "gdd.hobo.5 = ", round(b[1], 3),
  " + ", round(b["gdd.leol3s_acspo.5"], 3), "·gdd.leol3s_acspo.5",
  " + ", round(b["tidal_range_FT"], 3), "·tidal_range_FT\n",
  "R² = ", round(s$r.squared, 3),
  " | p(gdd) = ", signif(s$coefficients["gdd.leol3s_acspo.5","Pr(>|t|)"], 3),
  " | p(tidal) = ", signif(s$coefficients["tidal_range_FT","Pr(>|t|)"], 3)
)

ggplot(temp_data_daily_2025_t,
       aes(x = gdd.leol3s_acspo.5, y = gdd.hobo.5, color = tidal_range_FT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = Inf, y = Inf, label = label_txt,
           hjust = 1.05, vjust = 1.2, size = 3.5) +
  theme_bw()


# Predict hobo gdd with model of interest
temp_data_daily_2025_t$gdd_hobo_pred <- predict(temp_lm_t2)

# Plot hobo gdd against predicted gdd
ggplot(temp_data_daily_2025_t, aes(x = gdd_hobo_pred, y = gdd.hobo.5)) +
  geom_point(color = "grey") + 
  geom_smooth(method = "lm", color = "red") +
  # Annotate plot with simple linear model p-values and R2 values
  stat_poly_eq(use_label("eq", "R2", "p"))

# TODO - Pivot long and observe how predicted data tracks with hobo gdd over time
temp_data_daily_2025_t_long <- temp_data_daily_2025_t |>
  pivot_longer(
    cols = -c(site.id, latitude, longitude, date),
    names_to = "variable",
    values_to = "value"
  )

plot_data <- temp_data_daily_2025_t |>
  pivot_longer(
    cols = c(gdd.hobo.5, gdd_hobo_pred),
    names_to = "data_source",
    values_to = "gdd"
  )

ggplot(plot_data, aes(x = date, y = gdd, color = data_source)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ site.id, scales = "free_y") +
  labs(
    x = "Date",
    y = "GDD",
  ) +
  theme(legend.position = "top")





# Plot + label for temp_lm_td3
s <- summary(temp_lm_td3)
b <- coef(temp_lm_td3)

label_txt <- paste0(
  "gdd.hobo.5 = ", round(b[1], 3),
  " + ", round(b["gdd.crw.5"], 3), "·gdd.crw.5",
  " + ", round(b["tidal_range_FT"], 3), "·tidal_range_FT",
  " + ", round(b["depth_m"], 3), "·depth_m\n",
  "R² = ", round(s$r.squared, 3),
  " | p(crw) = ", signif(s$coefficients["gdd.crw.5","Pr(>|t|)"], 3),
  " | p(tidal) = ", signif(s$coefficients["tidal_range_FT","Pr(>|t|)"], 3),
  " | p(depth) = ", signif(s$coefficients["depth_m","Pr(>|t|)"], 3)
)

ggplot(temp_data_daily_2025_MA,
       aes(x = gdd.crw.5, y = gdd.hobo.5, color = tidal_range_FT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = Inf, y = Inf, label = label_txt,
           hjust = 1.05, vjust = 1.2, size = 3.5) +
  theme_bw()