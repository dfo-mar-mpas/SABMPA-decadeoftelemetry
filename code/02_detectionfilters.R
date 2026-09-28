# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 02_detectionfilters.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-08-20
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Applies MinLag, singleton, predation, mortality, and 
#              velocity filters to finalize the telemetry dataset.
# ===================================================================
# ---------------------------------------------------------
# 0. Setup & Packages
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(glatos)

# Load the enriched qualified dataset from Script 01
# Assuming this is what was referred to as SAB_glatos_Q25
SAB_glatos_Q25 <- readRDS("data/processed/SAB_enriched_final.rds")

# ---------------------------------------------------------
# 1. False Detections (MinLag) & Singletons
# ---------------------------------------------------------
# Apply glatos false detection filter (1 hour threshold)
SAB_minlag <- false_detections(SAB_glatos_Q25, tf = 3600) %>%
  filter(passed_filter == 1) %>%
  select(-passed_filter)

# Remove singletons (tags with only 1 detection overall)
SAB_step1_clean <- SAB_minlag %>%
  group_by(animal_id) %>%
  filter(n() > 1) %>%
  ungroup()

# ---------------------------------------------------------
# 2. Predation Tag Filter (Digestion Sensor)
# ---------------------------------------------------------
# Identify IDs that triggered the 'D' (Digestion) sensor
predated_animal_ids <- SAB_step1_clean %>%
  filter(Sensor == "D") %>%
  pull(animal_id) %>%
  unique()

# Isolate predated individuals for supplementary case studies
SAB_predation_case_studies <- SAB_step1_clean %>%
  filter(animal_id %in% predated_animal_ids)
write.csv(SAB_predation_case_studies, "outputs/SAB_predation_case_studies.csv", row.names = FALSE)

# Remove these individuals completely from the main dataset
SAB_step2_clean <- SAB_step1_clean %>%
  filter(!(animal_id %in% predated_animal_ids))

message("Removed ", length(predated_animal_ids), " predated individuals. Main dataset now contains ", n_distinct(SAB_step2_clean$animal_id), " individuals.")

# ---------------------------------------------------------
# 3. Stationary Mortality Filter
# ---------------------------------------------------------
# Identify Cod stationary at 1 station for > 10 days
stationary_morts <- SAB_step2_clean %>%
  filter(common_name == "Atlantic cod") %>%
  group_by(animal_id) %>%
  summarise(
    n_stations = n_distinct(station),
    duration_days = as.numeric(difftime(max(detection_timestamp_utc), min(detection_timestamp_utc), units = "days")),
    .groups = "drop"
  ) %>%
  filter(n_stations == 1 & duration_days > 10) %>%
  pull(animal_id)

message("Found and removed ", length(stationary_morts), " suspected stationary/mort Cod tags.")

# Remove stationary morts from dataset
SAB_step3_clean <- SAB_step2_clean %>%
  filter(!(animal_id %in% stationary_morts))

# ---------------------------------------------------------
# 4. Velocity Calculation (USER INPUT REQUIRED)
# ---------------------------------------------------------
# Calculate distance and time between consecutive detections here.
# The resulting dataframe MUST be named 'SAB_speed_eval' and contain 
# a column named 'velocity_ms' for the next step to work.

# Example placeholder:
# SAB_speed_eval <- SAB_step3_clean %>% 
#   arrange(animal_id, detection_timestamp_utc) %>%
#   ... (insert distance/velocity calculation) ...

# ---------------------------------------------------------
# 5. Speed Threshold Filter
# ---------------------------------------------------------
# Categorize detections based on swimming velocities
SAB_speed_categorized <- SAB_speed_eval %>%
  mutate(final_status = case_when(
    is.na(velocity_ms) ~ "Keep: Initial",
    
    velocity_ms > 35 ~ "Flag: Ghost",
    
    velocity_ms > 5 & common_name %in% c("Atlantic cod", "Atlantic salmon", "Atlantic halibut",
                                         "American eel", "Atlantic mackerel",
                                         "Snow crab", "Atlantic sturgeon") ~ "Flag: Predator/Error",
    
    velocity_ms > 10 & !common_name %in% c("White shark", "Grey seal", "Shortfin mako", 
                                           "Leatherback turtle", "Porbeagle shark",
                                           "Bluefin tuna", "Blue shark") ~ "Flag: Predator/Error",
    
    velocity_ms > 10 & common_name %in% c("Bluefin tuna", "Blue shark") ~ "Keep: High-Speed",
    TRUE ~ "Keep: Valid Biological"
  ))

# ---------------------------------------------------------
# 6. Behavioural Shift Predation Filter
# ---------------------------------------------------------
# Identify individuals with sustained high-speed behavior (>20% of detections speeding)
predation_suspects <- SAB_speed_categorized %>%
  filter(common_name %in% c("Atlantic cod", "Atlantic salmon", "American eel")) %>%
  group_by(animal_id) %>%
  summarise(
    total_dets = n(),
    speeding_events = sum(velocity_ms > 2 & velocity_ms < 10, na.rm = TRUE),
    pct_speeding = (speeding_events / total_dets) * 100,
    .groups = "drop"
  ) %>%
  filter(pct_speeding > 20 & total_dets > 5) %>%
  pull(animal_id)

message("Removed ", length(predation_suspects), " individuals due to sustained high-speed behavioural shifts.")

# ---------------------------------------------------------
# 7. Finalize and Export
# ---------------------------------------------------------
# Create final master dataset
SAB_glatos_FINAL <- SAB_speed_categorized %>%
  filter(grepl("Keep", final_status)) %>%
  filter(!(animal_id %in% predation_suspects))

# Generate summary table for supplementary materials
speed_summary_table <- SAB_speed_categorized %>%
  group_by(common_name) %>%
  summarise(
    Total_n = n(),
    Removed_n = sum(grepl("Flag", final_status)),
    Kept_n = sum(grepl("Keep", final_status)),
    Pct_Removed = round((Removed_n / Total_n) * 100, 1),
    Pct_Kept = round((Kept_n / Total_n) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    Detections_Removed = paste0(Removed_n, " (", Pct_Removed, "%)"),
    Detections_Kept = paste0(Kept_n, " (", Pct_Kept, "%)")
  ) %>%
  select(common_name, Total_n, Detections_Removed, Detections_Kept)

write.csv(speed_summary_table, "outputs/Table_SAB_SpeedFilter_Summary.csv", row.names = FALSE)

# Export finalized dataset for all downstream modeling and plotting
saveRDS(SAB_glatos_FINAL, "data/processed/SAB_glatos_FINAL.rds")