# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 03_glatos_summaries.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-09-22
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Generates standard glatos summaries (by location and 
#              by animal) and interpolates daily positions for 
#              core species.
# ===================================================================
# ---------------------------------------------------------
# 0. Setup & Packages
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(readr)
library(glatos)

# Load the finalized, fully-filtered detections and the receiver metadata
SAB_glatos_FINAL <- readRDS("data/processed/SAB_glatos_FINAL.rds")
SAB_deploys <- readRDS("data/processed/SAB_deploys_final.rds")

# Ensure the object retains its specific glatos class for the functions to work
SAB_glatos_FINAL <- as_glatos_detections(SAB_glatos_FINAL)

# ---------------------------------------------------------
# 1. Glatos Summaries: By Location (Station)
# ---------------------------------------------------------
# Duplicate and rename columns to match strict glatos formatting
SAB_deploys <- SAB_deploys %>%
  mutate(
    station = as.character(STATION_NO3), # make sure the station naming structure matches that used in the detection file
    deploy_lat = as.numeric(stn_lat),    # Update 'stn_lat' if your column is named differently
    deploy_long = as.numeric(stn_long)   # Update 'stn_long' if your column is named differently
  )

# We use the deployment metadata for 'receiver_locs' so that stations 
# that detected zero fish are still included in the final summary table as 0s.
station_summary <- summarize_detections(
  det = SAB_glatos_FINAL,
  location_col = 'station',
  summ_type = 'location',
  receiver_locs = SAB_deploys
)

write_csv(station_summary, "output/Table_GLATOS_Station_Summary.csv")

# ---------------------------------------------------------
# 2. Glatos Summaries: By Animal
# ---------------------------------------------------------
# Summarize total detections, days detected, and station count per individual tag
animal_summary <- summarize_detections(
  det = SAB_glatos_FINAL,
  summ_type = 'animal'
)

# Merge back the common names to make the table readable for the manuscript
animal_summary_clean <- animal_summary %>%
  left_join(
    SAB_glatos_FINAL %>% distinct(animal_id, common_name), 
    by = "animal_id"
  ) %>%
  select(common_name, animal_id, everything()) %>%
  arrange(common_name, desc(num_dets))

write_csv(animal_summary_clean, "output/Table_GLATOS_Animal_Summary.csv")

# ---------------------------------------------------------
# 3. Path Interpolation (Daily Positions for Core Species)
# ---------------------------------------------------------
# Filter for core species tagged by SABMPA
core_species <- c("Atlantic cod", "Atlantic halibut", "Atlantic striped wolffish")

# Create a clean list to store the interpolated paths
interpolated_paths <- list()

for(sp in core_species) {
  
  # Subset data for the specific species
  sp_data <- SAB_glatos_FINAL %>% filter(common_name == sp)
  
  # Only run if there is data for that species
  if(nrow(sp_data) > 0) {
    message("Interpolating path for: ", sp)
    
    # Calculate daily (86400 seconds) interpolated positions
    sp_interp <- interpolate_path(
      det = sp_data, 
      trans = NULL, 
      int_time_stamp = 86400
    )
    
    # Save the individual species interpolation
    sp_filename <- paste0("data/processed/interpolated_", gsub(" ", "_", sp), ".rds")
    saveRDS(sp_interp, sp_filename)
    
    # Store in the list in case you want to bind them later
    interpolated_paths[[sp]] <- sp_interp
  }
}