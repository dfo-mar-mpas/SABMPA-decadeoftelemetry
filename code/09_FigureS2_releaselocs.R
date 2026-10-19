# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 09_FigureS2_releaselocs.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-10-19
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Generates Figure S2 (Supplemental) mapping the 
#              historical acoustic tagging release locations and 
#              receiver array within the St. Anns Bank MPA.
# ===================================================================

# ---------------------------------------------------------
# 0. Setup & Package Loading
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(ggplot2)
library(sf)
library(smoothr) 

## --- Load data and files if not already loaded
# SABMPA fish tagging data
SAB_tag_metadata_M <- readRDS("data/processed/SAB_tag_metadata_M.rds")
# Receiver deployment data
SAB_deploys_FINAL <- readRDS("data/processed/SAB_deploys_FINAL.rds")

## --- Shapefiles
# SAB NS basemap
basemap <- readRDS("data/shapefiles/basemap.rds")
# SAB MPA shapefile
sab <- readRDS("data/shapefiles/sab_mpa_sf.rds")
# SAB shapefile in WGS84 format
sab_wgs84 <- readRDS("data/shapefiles/sab_wgs84.rds")
# SAB zone labels shapefile
sab_label_data <- readRDS("data/shapefiles/sab_label_data.rds")
# 200m Shelfbreak shapefile
shelfbreak <- readRDS("data/shapefiles/shelfbreak.rds")

# ---------------------------------------------------------
# 1. Data Preparation
# ---------------------------------------------------------
# Extract the unique background receiver coordinates to plot the array
receiver_background <- SAB_deploys_FINAL %>%
  distinct(station, stn_long, stn_lat)

# Safely smooth the depth contour line using the 'chaikin' algorithm
shelfbreak_smooth <- smoothr::smooth(shelfbreak, method = "chaikin")

# Crop the smoothed line to your exact plot boundaries to prevent rendering issues
plot_box <- st_bbox(c(xmin = -59.75, xmax = -58.25, ymin = 45.7, ymax = 46.55), 
                    crs = st_crs(shelfbreak))
shelfbreak_cropped <- st_crop(shelfbreak_smooth, plot_box)

# ---------------------------------------------------------
# 2. Generate Final Supplemental Map
# ---------------------------------------------------------
p_match_rel <- ggplot() +
  # Base map
  geom_sf(data = basemap) +
  
  # Receiver array (Red markers)
  geom_point(data = receiver_background,
             aes(x = stn_long, y = stn_lat), 
             shape = 20, color = "red", size = 3.5, alpha = 0.6) +
  
  # All tagging release locations
  geom_point(data = SAB_tag_metadata_M25, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = common_name), 
             size = 2, alpha = 0.7) +
  
  # Bold MPA Borders
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = sab_wgs84, fill = NA, color = "black", linewidth = 1) +
  
  # Smoothed & Cropped Depth Contour (no aes mapping to ignore legend)
  geom_sf(data = shelfbreak_cropped, fill = NA, color = "gray40", 
          linewidth = 0.5, linetype = "solid") +
  
  # Zone Labels
  geom_sf_label(data = sab_label_data, aes(label = Id), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  
  # Formatting and Scales
  scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(-59.75, -58.25), ylim = c(45.7, 46.55), expand = FALSE) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank(), 
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  labs(x = "Longitude", y = "Latitude")

print(p_match_rel)

# ---------------------------------------------------------
# 3. Export Figure
# ---------------------------------------------------------

# Save lightweight PNG for the GitHub repository README
ggsave("output/Figure_Release_Locs_README.png", # save as tiff and dpi 300 for high quality
       plot = p_match_rel, 
       width = 6.5,             
       height = 4.5,             
       units = "in", 
       dpi = 150)