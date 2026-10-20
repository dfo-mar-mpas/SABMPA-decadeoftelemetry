# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 05_Figure2_RI.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-10-17
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Residency Index (RI) on SABMPA tagged fish (matched detections) 
#              and Figure 2 plot. 
# ===================================================================
# ---------------------------------------------------------
# Required packages (run 00_setup.R). 
# ---------------------------------------------------------
# 0. Load project configuration and packages
source("code/00_setup.R")
# ---------------------------------------------------------
library(patchwork)
library(ggplot2)
library(dplyr)
library(glatos)

# ---------------------------------------------------------
# 1. Load clean filtered matched detections and run glatos::detection events()
# ---------------------------------------------------------
# Note: The data should already be in the format required for glatos. 
# See previous scripts for details.

SAB_core_M25_FINAL <- readRDS("data/processed/SAB_core_M25_FINAL.rds")

# Split the data by species

cod_final_M <- SAB_core_M25_FINAL %>%
  filter(commonName == "Atlantic cod") 

hal_final_M <- SAB_core_M25_FINAL %>%
  filter(commonName == "Atlantic halibut")

wolf_final_M <- SAB_core_M25_FINAL %>%
  filter(commonName == "Atlantic striped wolffish")

# NOTE: This was also completed using script 03_glatos_summaries.R on all species.
# Here we conduct it separately on the core species only. 

cod_events_M <- detection_events(cod_final_M, 
                                 location_col = "station", 
                                 time_sep = 43200)

hal_events_M <- detection_events(hal_final_M, 
                                   location_col = "station", 
                                   time_sep = 43200)

wolf_events_M <- detection_events(wolf_final_M, 
                                 location_col = "station", 
                                 time_sep = 43200)

# Combine for a complete dataset of detection events on core species.
SAB_detection_events_core_M <- bind_rows(
  cod_events_M %>% mutate(species = "Atlantic cod"),
  hal_events_M %>% mutate(species = "Atlantic halibut"),
  wolf_events_M %>% mutate(species = "Atlantic striped wolffish")
)

# Or load the detections events run on all species combined (Script 03) to run
# RI on all species. For this publication, we ran it on the three core species only.
#SAB_glatos_detection_events_M <- readRDS("data/processed/SAB_glatos_detection_events_M.rds")

# ---------------------------------------------------------
# 2. Calculate the Residency Index (RI) for each species (by individual)
# ---------------------------------------------------------
# Calculated individually on each species, then combined
cod_ri_M <- residence_index(cod_events_M, 
                                calculation_method = "kessel")

hal_ri_M <- residence_index(hal_events_M, 
                                calculation_method = "kessel")

wolf_ri_M <- residence_index(wolf_events_M, 
                                 calculation_method = "kessel")

core_ri_SAB_M <- bind_rows(
  cod_ri_M %>% mutate(species = "Atlantic cod"),
  hal_ri_M %>% mutate(species = "Atlantic halibut"),
  wolf_ri_M %>% mutate(species = "Atlantic striped wolffish")
) %>%
  filter(residency_index > 0) # filter out zeros for plotting (no valid RI)

# Or load your RI data for plotting if you already have it saved
#core_ri_SAB_M <- readRDS("data/processed/core_ri_SAB_M.rds")

# ---------------------------------------------------------
# 3. Define Shared Theme for plotting
# ---------------------------------------------------------
# Defining this as an object keeps the code clean and ensures consistency
ri_plot_theme <- theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
# ---------------------------------------------------------
# 4. Top Plot: Latitude (Keeps the facet strips)
# ---------------------------------------------------------
Restimeplot_lat_M <- ggplot(core_ri_SAB_M, aes(x = mean_latitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Latitude", y = "Residency Index (RI)") +
  ri_plot_theme
# ---------------------------------------------------------
# 5. Bottom Plot: Longitude (Removes the facet strips)
# ---------------------------------------------------------
Restimeplot_lon_M <- ggplot(core_ri_SAB_M, aes(x = mean_longitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Longitude", y = "Residency Index (RI)") +
  ri_plot_theme +
  # Add overrides specifically for the bottom plot
  theme(
    strip.background = element_blank(), # Removes the white box
    strip.text.x = element_blank()      # Removes the text
  )
# ---------------------------------------------------------
# 6. Combine Plots
# ---------------------------------------------------------
combined_res_plot_M <- Restimeplot_lat_M / Restimeplot_lon_M

# Display in R
print(combined_res_plot_M)
# ---------------------------------------------------------
# 7. Save Figure 2
# ---------------------------------------------------------
ggsave("output/Figure2_RI_M.png", # save as .tiff and dpi 300 for high quality
       plot = combined_res_plot_M, 
       width = 10,      
       height = 7,       
       units = "in", 
       dpi = 150)

# ===================================================================
## Exploring other options for visualization
# ---------------------------------------------------------
# 2D Spatial Map (not included in CJFAS publication)
# ---------------------------------------------------------
RI_map_plot_M <- ggplot(core_ri_SAB_M, aes(x = mean_longitude, y = mean_latitude)) +
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

print(RI_map_plot_M)


