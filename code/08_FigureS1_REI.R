# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 08_FigureS1_REI.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-10-19
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Calculates Receiver Efficiency Index (REI) on qualified 
#              detections using the glatos package, calculates Species 
#              Richness (Sr), and generates Figure S1.
# ===================================================================
# ---------------------------------------------------------
# 1. Setup & Package Loading
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(tidyr)
library(glatos)
library(sf)
library(ggplot2)
library(ggnewscale)

## --- Load the processed, cleaned and filtered files if not loaded already

# Receiver deployments
SAB_deploys_FINAL <- readRDS("data/processed/SAB_deploys_FINAL.rds")

# Qualified detections with species metadata
SAB_glatos_Q25_FINAL <- readRDS("data/processed/SAB_glatos_Q25_FINAL.rds")


## --- Or load in all these prepared files and jump to step 3.
SAB_det_ready <- readRDS("data/processed/SAB_det_ready_Q.rds")
SAB_dep_ready <- readRDS("data/processed/SAB_dep_ready_Q.rds")

# -- set the geographic mapping parameters
#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

## --- Load shapefiles
# SAB NS basemap
basemap <- readRDS("data/shapefiles/basemap.rds")

sab <- read_sf("data/shapefiles/SAB_boundary_zones_2017.shp")%>%
  st_transform(latlong)

# Benthoscape habitat classifications clipped by the SAB MPA boundary
sab_bs_clipped <- readRDS("data/shapefiles/sab_bs_clipped.rds")

## --- Load the plotting color palette
habitat_cols <- readRDS("data/processed/habitat_cols.rds")

# ---------------------------------------------------------
# 2. Format Detections and Deployments for glatos if not done already (see scripts 01-03)
# ---------------------------------------------------------
# Note the data should already be in glatos format. See previous scripts for details.

# Format Detections for glatos REI
SAB_det_ready <- SAB_glatos_Q25_FINAL %>%
  separate(transmitter_id, into = c("transmitter_codespace", "tag_ID"), 
           sep = "-(?=[^-]+$)", remove = FALSE) %>%
  transmute(
    animal_id = as.character(animal_id),
    detection_timestamp_utc = as.POSIXct(detection_timestamp_utc, tz = "UTC"),
    deploy_lat = as.numeric(deploy_lat),
    deploy_long = as.numeric(deploy_long),
    station = as.character(station),
    common_name_e = as.character(common_name),
    release_date = as.Date(release_date, tz = "UTC"),
    release_latitude = as.numeric(RELEASE_LATITUDE),
    release_longitude = as.numeric(RELEASE_LONGITUDE),
    capture_location = as.character(RELEASE_LOCATION),
    receiver_sn = as.character(station_SN),
    transmitter_codespace = as.character(transmitter_codespace),
    transmitter_id = as.character(tag_ID),
    # Assign specific array designs for independent REI calculations
    glatos_array = case_when(
      grepl("^2021_", station) ~ "Single Gate (2021-2025)",
      grepl("^N|^S", station) ~ "Double Gate (2015-2021)",
      TRUE ~ "Other"
    )
  )

# Format Deployments for glatos REI
SAB_dep_ready <- SAB_deploys_FINAL %>%
  transmute(
    station = as.character(station),
    deploy_date_time = as.POSIXct(deploy_date, tz = "UTC"),
    recover_date_time = as.POSIXct(recovery_date, tz = "UTC"),
    deploy_lat = as.numeric(stn_lat),
    deploy_long = as.numeric(stn_long),
    glatos_array = case_when(
      grepl("^2021_", station) ~ "Single Gate (2021-2025)",
      grepl("^N|^S", station) ~ "Double Gate (2015-2021)",
      TRUE ~ "Other"
    )
  ) %>%
  # Cap unrecovered receivers to current date for calculations if not removed already. This dataset 
  # already has receivers not recovered filtered out. See Script 01 data cleaning.
  mutate(recover_date_time = replace(recover_date_time, is.na(recover_date_time), Sys.time()))
    # NOTE: Your computer system time needs to be reasonably accurate for this to work as 
    # intended. Alternatively, you could remove by the 'RECOVERED' column or by the specific date.
    
# ---------------------------------------------------------
# 3. Calculate Independent REI & Species Richness
# ---------------------------------------------------------
# Run REI for the Double Gate array
rei_double <- glatos::REI(
  SAB_det_ready %>% filter(glatos_array == "Double Gate (2015-2021)"), 
  SAB_dep_ready %>% filter(glatos_array == "Double Gate (2015-2021)")
) %>%
  mutate(array_design = "Double Gate (2015-2021)",
         rei_pct_within_era = (rei / sum(rei, na.rm = TRUE)) * 100)

# Run REI for the Single Gate array
rei_single <- glatos::REI(
  SAB_det_ready %>% filter(glatos_array == "Single Gate (2021-2025)"), 
  SAB_dep_ready %>% filter(glatos_array == "Single Gate (2021-2025)")
) %>%
  mutate(array_design = "Single Gate (2021-2025)",
         rei_pct_within_era = (rei / sum(rei, na.rm = TRUE)) * 100)

# Combine REI results
SAB_rei_final_comparison <- bind_rows(rei_double, rei_single)

# Extract Station-Specific Species Richness (Sr)
station_richness <- SAB_det_ready %>%
  group_by(station) %>%
  summarise(Sr = n_distinct(common_name_e), .groups = "drop")

# ---------------------------------------------------------
# 4. Prepare Spatial Object for Mapping
# ---------------------------------------------------------
# Merge REI, Species Richness, and convert to sf object
array_split <- SAB_rei_final_comparison %>%
  left_join(station_richness, by = "station") %>%
  # Fill silent stations with 0 species richness
  mutate(Sr = tidyr::replace_na(Sr, 0)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # Assuming lat/long WGS84

# Update Zone labels for mapping
sab <- sab %>%
  mutate(Zone = case_when(
    Zone == "2a" ~ "2",
    Zone == "2b" ~ "3",
    Zone == "2c" ~ "4",
    TRUE ~ as.character(Zone)
  ))

# ---------------------------------------------------------
# 5. Generate the Final Full-Page Map
# ---------------------------------------------------------

# Set the plotting window
# Define the spatial bounds (West longitudes must be negative)

plot_boundaries <- c(c(-59.7,-58.3,45.7,46.5))

p1_rei_full_page <- ggplot() +
  # 1. Base Layers
  geom_sf(data = basemap) +
  # 'sab_bs_clipped' was created using st_make_valid/st_intersection
  geom_sf(data = sab_bs_clipped, aes(fill = Assigned_c), color = NA, alpha = 1) +
  scale_fill_manual(name = "Benthoscape habitat", values = habitat_cols) +
  
  
  # 2. Bold MPA Border
  geom_sf(data = sab, fill = NA, color = "gray23", linewidth = 1) +
  
  # 3. REI Bubbles (Sized by REI%, Colored by Species Richness)
  ggnewscale::new_scale_color() + 
  geom_point(data = array_split, 
             aes(geometry = geometry, size = rei_pct_within_era, color = Sr), 
             stat = "sf_coordinates", alpha = 0.8) +
  scale_color_viridis_c(name = "Species richness", option = "viridis", direction = -1) +
  scale_size_continuous(name = "REI %", range = c(1, 8)) +
  
  # 4. Station Positions (Red/Black dots)
  ggnewscale::new_scale_color() + 
  geom_point(data = array_split, 
             aes(geometry = geometry, color = array_design), 
             stat = "sf_coordinates", shape = 20, size = 1, stroke = 1) +
  scale_color_manual(name = "Array design", 
                     values = c("Single Gate (2021-2025)" = "black", 
                                "Double Gate (2015-2021)" = "red"),
                     guide = "none") + # Hidden in final map
  
  # 5. Zone Labels
  geom_sf_text(data = sab %>% filter(Zone != "4"), aes(label = Zone), 
               size = 5, fontface = "bold", color = "black") +
  geom_sf_text(data = sab %>% filter(Zone == "4"), aes(label = Zone), 
               size = 5, fontface = "bold", color = "black",
               nudge_x = 0.08, nudge_y = -0.05) +
  
  # 6. Theme and Layout
  theme_bw() +
  guides(
    fill = guide_legend(ncol = 3, byrow = TRUE, order = 1),
    size = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    color = guide_colorbar(order = 3)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim=plot_boundaries[c(1,2)],ylim=plot_boundaries[c(3,4)],expand=0)

print(p1_rei_full_page)

# ---------------------------------------------------------
# 6. Export Final Figure
# ---------------------------------------------------------
# PNG for GitHub README
ggsave("output/Figure_S1_REI_Q.png", #save as .tiff for publication quality
       plot = p1_rei_full_page, 
       width = 8.5, 
       height = 9, 
       units = "in", 
       dpi = 150) # save at 300 dpi for publication quality