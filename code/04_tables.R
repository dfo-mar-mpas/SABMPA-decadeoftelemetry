# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 04_tables.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-09-23
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Generates core tagging and detection demographic tables
#              (Table 1, Table 2, Table 3, Table S1) for SABMPA project tags.
# ===================================================================

source("code/00_setup.R")

library(dplyr)
library(lubridate)
library(tidyr)

# Load final qualified detections
SAB_glatos_FINAL <- readRDS("data/processed/SAB_glatos_FINAL.rds")

# Load final matched detections (for our SABMPA tags)
# TODO: update this with the filtered matched dataset, this one still includes singletons so is before filters
matched_SAB_25_glatos <- readRDS("data/processed/matched_SAB_25_glatos.rds") 

# Load our SABMPA tagging metadata
SAB_tag_metadata <- read.csv("data/processed/fish_SABtagged_clean_Aug2023_no16cod.csv") 

# ---------------------------------------------------------
# 1. Table 1: Tagging Demographics by Species
# Total counts, body length (mean ± S.D. (min-max)), tag type, tag life
# ---------------------------------------------------------
table_1 <- SAB_tag_metadata %>%
  group_by(Common.Name) %>%
  summarise(
    Total_Individuals = n_distinct(animal_id),
    # Mean and Range of length for the manuscript
    Avg_Length_M = round(mean(LENGTH_M, na.rm = TRUE), 2),
    sd_Length_M = round(sd(LENGTH_M, na.rm = TRUE), 2),
    Min_Length_M = min(LENGTH_M, na.rm = TRUE),
    Max_Length_M = max(LENGTH_M, na.rm = TRUE),
    # List unique tag models used for each species
    Tag_Models = paste(unique(TAG_MODEL), collapse = ", "),
    # Create the counts for each Estimated Tag Life category
    # use 'n()' within each species-life group
    .groups = "drop"
  ) %>%
  
  # join the counts for Tag Life and pivot them into columns
  left_join(
    SAB_tag_metadata %>%
      count(Common.Name, EST_TAG_LIFE) %>%
      mutate(EST_TAG_LIFE = paste0("Life_", EST_TAG_LIFE, "_days")) %>%
      pivot_wider(names_from = EST_TAG_LIFE, values_from = n, values_fill = 0),
    by = "Common.Name"
  ) %>%
  mutate(Body_Length = paste0(Avg_Length_M, " ± ", sd_Length_M, " (", Min_Length_M, " - ", Max_Length_M, ")"))  %>%
  select(
    Species = Common.Name, 
    n = Total_Individuals, 
    `Total Length (m)` = Body_Length, 
    `Tag type` = Tag_Models, 
    starts_with("Life_") # Use starts_with() to grab all the tag life columns
  )

write.csv(table_1, "output/Table_1_SAB_tags_Demographics.csv", row.names = FALSE)

# ---------------------------------------------------------
# 2. Table 2: Array Detections Summary - matched dets
# Fish counts, total detections, first/last det, mean lat/lon by array
# ---------------------------------------------------------

table_2_array_M25 <- matched_SAB_25_glatos %>%
  # Combine project and local area columns before grouping
  mutate(`Array Project code - local area` = paste0(detectedBy, " - ", localArea)) %>%
  # Group by species and the newly combined string
  group_by(commonName, `Array Project code - local area`) %>% 
  summarise(
    n = n_distinct(animal_id),
    Total_Detections = n(),
    First_Detection = min(detection_timestamp_utc),
    Last_Detection = max(detection_timestamp_utc),
    Mean_Latitude = round(mean(deploy_lat, na.rm = TRUE), 4),
    Mean_Longitude = round(mean(deploy_long, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(commonName, desc(Total_Detections)) %>%
  # format final columns
  select(
    Species = commonName,
    `Array Project code - local area`,
    `Fish count` = n,
    Detections = Total_Detections,
    `First detection (Y-M-D H-M-S) UTC` = First_Detection,
    `Last detection (Y-M-D H-M-S) UTC` = Last_Detection,
    `Mean latitude (DD)` = Mean_Latitude,
    `Mean longitude (DD)` = Mean_Longitude
  )
  
write.csv(table_2_array_M25, "output/Table_2_array_M25.csv", row.names = FALSE)

# ---------------------------------------------------------
# 3. Table 3: Qualified summary table 
# Tagged individuals detected annually vs released annually
# ---------------------------------------------------------

SAB_qualified_summary <- SAB_glatos_FINAL %>%
  group_by(common_name) %>%
  summarise(
    Total_Detections = n(),
    Unique_Individuals = n_distinct(animal_id),
    # Calculate Mean Bottom Depth for the species
    Mean_Bottom_Depth_m = round(mean(mean_depth, na.rm = TRUE), 1),
    # Max Years Active: Span between first and last detection year per ID
    Max_Years_Active = max(sapply(split(yearcollected, animal_id), 
                                  function(x) max(x) - min(x) + 1)),
    .groups = "drop"
  ) %>%
  # Join Habitat Distribution (Detections per habitat)
  left_join(
    SAB_glatos_FINAL %>%
      count(common_name, Assigned_c) %>%
      pivot_wider(names_from = Assigned_c, values_from = n, values_fill = 0, names_prefix = "Hab_"),
    by = "common_name"
  ) %>%
  # Join Zone Distribution (Detections per zone)
  left_join(
    SAB_glatos_FINAL %>%
      count(common_name, zone) %>%
      mutate(zone = paste0("Zone_", zone)) %>%
      pivot_wider(names_from = zone, values_from = n, values_fill = 0),
    by = "common_name"
  )

print(SAB_qualified_summary)

write.csv(SAB_qualified_summary,"output/Table3_SAB_qualified_summary.csv")

# ---------------------------------------------------------
# TODO: Update the yearly release code to match MS table S1
# 4. Table S1: Annual Detections vs Releases - UPDATE SO THIS IS FOR QDETS (SAB_tag_metadata is our project tags)
# Tagged individuals detected annually vs released annually
# ---------------------------------------------------------
# Calculate yearly releases from metadata
yearly_releases <- SAB_tag_metadata %>%
  mutate(year = year(utc_release_date_time)) %>%
  group_by(common_name, year) %>%
  summarise(Released = n_distinct(animal_id), .groups = "drop")

# Calculate yearly detections from main dataset
yearly_detections <- SAB_glatos_FINAL %>%
  mutate(year = year(detection_timestamp_utc)) %>%
  group_by(common_name, year) %>%
  summarise(Detected = n_distinct(animal_id), .groups = "drop")

# Join them together for Table S1 format: Detected (Released)
table_s1 <- yearly_detections %>%
  full_join(yearly_releases, by = c("common_name", "year")) %>%
  replace_na(list(Detected = 0, Released = 0)) %>%
  mutate(Detected_vs_Released = paste0(Detected, " (", Released, ")")) %>%
  select(common_name, year, Detected_vs_Released) %>%
  pivot_wider(names_from = year, values_from = Detected_vs_Released, values_fill = "0 (0)")

write.csv(table_s1, "output/Table_S1_Annual_Det_vs_Rel.csv", row.names = FALSE)


# ---------------------------------------------------------
# NOT USED IN MANUSCRIPT
# ---------------------------------------------------------
# Table S2: Array Detections Summary - Qdets
# Fish counts, total detections, first/last det, mean lat/lon by array
# ---------------------------------------------------------
table_array_Q25 <- SAB_glatos_FINAL %>%
  group_by(common_name, glatos_array) %>% # or 'collectioncode' depending on your array column
  summarise(
    n = n_distinct(animal_id),
    Total_Detections = n(),
    First_Detection = min(detection_timestamp_utc),
    Last_Detection = max(detection_timestamp_utc),
    Mean_Latitude = round(mean(deploy_lat, na.rm = TRUE), 4),
    Mean_Longitude = round(mean(deploy_long, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(common_name, desc(Total_Detections))
