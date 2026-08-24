### SABMPA_decadeoftelemetry ###
# Script 0#: RI plots - Residenct Index on SABMPA tagged fish
# Author: Harri Pettitt-Wade
# contact: harri.pettitt-wade@dfo-mpo.gc.ca
# Date Updated: 2026-08-24

# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# TODO: structure the code
# TODO: tidy the code and remove repeats
# TODO: test run the code
# 
# ---------------------------------------------------------
# Required packages (run 00_setup.R). 
# library(patchwork), library(ggplot2), library(dplyr)
# ---------------------------------------------------------
# 0. Load project configuration and packages
source("code/00_setup.R")
# ---------------------------------------------------------
# ---------------------------------------------------------
# 1. Load species Residency Index (RI) Data from matched detections
# ---------------------------------------------------------
residency_data <- read.csv("data/processed/all_ri_data_SAB.csv")
# ---------------------------------------------------------
# 2. Define Shared Theme for Publication
# ---------------------------------------------------------
# Defining this as an object keeps the code clean and ensures consistency
pub_theme <- theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
# ---------------------------------------------------------
# 3. Top Plot: Latitude (Keeps the facet strips)
# ---------------------------------------------------------
Restimeplot_lat <- ggplot(residency_data, aes(x = mean_latitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Latitude", y = "Residency Index (RI)") +
  pub_theme
# ---------------------------------------------------------
# 4. Bottom Plot: Longitude (Removes the facet strips)
# ---------------------------------------------------------
Restimeplot_lon <- ggplot(residency_data, aes(x = mean_longitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Longitude", y = "Residency Index (RI)") +
  pub_theme +
  # Add overrides specifically for the bottom plot
  theme(
    strip.background = element_blank(), # Removes the white box
    strip.text.x = element_blank()      # Removes the text
  )
# ---------------------------------------------------------
# 5. Combine Plots
# ---------------------------------------------------------
combined_res_plot <- Restimeplot_lat / Restimeplot_lon

# Display in R
print(combined_res_plot)
# ---------------------------------------------------------
# 6. Save Figure 2
# ---------------------------------------------------------
ggsave("output/Figure2_RI.jpg", 
       plot = combined_res_plot, 
       width = 10,      
       height = 7,       
       units = "in", 
       dpi = 300)           
       #compression = "lzw")  # compression if needed

# ---------------------------------------------------------
## Exploring other options for visualization
# ---------------------------------------------------------
# 2D Spatial Map (not included in CJFAS publication)
# ---------------------------------------------------------
map_plot <- ggplot(residency_data, aes(x = mean_longitude, y = mean_latitude)) +
  # Use a slight jitter so individuals at the same station don't perfectly overlap
  geom_point(aes(fill = residency_index, size = residency_index), 
             shape = 21, color = "black", alpha = 0.7,
             position = position_jitter(width = 0.02, height = 0.02)) +
  # Use a colorblind-friendly gradient (viridis) to show RI intensity
  scale_fill_viridis_c(name = "Residency\nIndex", option = "plasma") +
  scale_size_continuous(name = "Residency\nIndex", range = c(1, 5)) +
  facet_wrap(~species, scales = "free") +
  # coord_quickmap() ensures the aspect ratio of Lat/Lon is geographically accurate
  coord_quickmap() + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    # Bring the legend back to show the RI scale
    legend.position = "right" 
  )

print(map_plot)
