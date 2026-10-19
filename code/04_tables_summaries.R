# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 04_tables.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-10-17
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Generates core tagging and detection demographic tables
#              (Table 1, Table 2, Table 3, Table S1, Tabls S2) for SABMPA project tags.
# ===================================================================
# ---------------------------------------------------------
# 0. Setup & Packages
# ---------------------------------------------------------
source("code/00_setup.R")

library(lubridate)
library(formattable)
library(dplyr)
library(tidyr)
# ---------------------------------------------------------
## Load the finalized, cleaned and filtered detections and tagging metadata
# ---------------------------------------------------------
# Load final qualified detections (with species info)

SAB_model_data_Q <- readRDS("data/processed/SAB_model_data_Q.rds") 
# This is the same as the SAB_glatos_Q25_FINAL file from previous scripts 
# with zone and habitat data added.

# Load final matched detections (for our SABMPA tags)
SAB_core_M25_FINAL <- readRDS("data/processed/SAB_core_M25_FINAL.rds")

# Load SABMPA tagging metadata
SAB_tag_metadata_M25 <- readRDS("data/processed/SAB_tag_metadata_M25.rds")

# ---------------------------------------------------------
# 1. Table 1: Tagging Demographics by Species for SABMPA tags (i.e.
# associated with Matched detections). 
# Total counts, body length (mean ± S.D. (min-max)), tag type, tag life
# Note that 58 Atlantic cod tagged by SABMPA in 2016 were not included in this 
# total because at the time they were included in the qualified detection file 
# under a partner project (SGS).
# ---------------------------------------------------------
table_1_tagging_M <- SAB_tag_metadata_M25 %>%
  group_by(common_name) %>%
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
    SAB_tag_metadata_M25 %>%
      count(common_name, EST_TAG_LIFE) %>%
      mutate(EST_TAG_LIFE = paste0("Life_", EST_TAG_LIFE, "_days")) %>%
      pivot_wider(names_from = EST_TAG_LIFE, values_from = n, values_fill = 0),
    by = "common_name"
  ) %>%
  mutate(Body_Length = paste0(Avg_Length_M, " ± ", sd_Length_M, " (", Min_Length_M, " - ", Max_Length_M, ")"))  %>%
  select(
    Species = common_name, 
    n = Total_Individuals, 
    `Total Length (m)` = Body_Length, 
    `Tag type` = Tag_Models, 
    starts_with("Life_") # Use starts_with() to grab all the tag life columns
  )


write.csv(table_1_tagging_M, "output/Table_1_SAB_tags_Demographics_M.csv", row.names = FALSE)

# ---------------------------------------------------------
# 2. Table 2: Array Detections Summary - matched dets
# Fish counts, total detections, first/last det, mean lat/lon by array
# ---------------------------------------------------------

table_2_array_M <- SAB_core_M25_FINAL %>%
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


write.csv(table_2_array_M, "output/Table_2_array_M.csv", row.names = FALSE)

# ---------------------------------------------------------
# 3. Table 3: Qualified summary table 
# Total detections and individuals for each species by habitat type, zone, depth
# and maximum years detected.
# ---------------------------------------------------------

Table_3_dets_Q <- SAB_model_data_Q %>%
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
    SAB_model_data_Q %>%
      count(common_name, Assigned_c) %>%
      pivot_wider(names_from = Assigned_c, values_from = n, values_fill = 0, names_prefix = "Hab_"),
    by = "common_name"
  ) %>%
  # Join Zone Distribution (Detections per zone)
  left_join(
    SAB_model_data_Q %>%
      count(common_name, zone) %>%
      mutate(zone = paste0("Zone_", zone)) %>%
      pivot_wider(names_from = zone, values_from = n, values_fill = 0),
    by = "common_name"
  )


write.csv(Table_3_dets_Q,"output/Table_3_SAB_Q_summary.csv")

# ---------------------------------------------------------
# 4. Table S1: Annual Detections vs Releases - Matched detections
# Tagged individuals detected annually vs released annually
# ---------------------------------------------------------

# a. Calculate yearly summaries
yearly_releases_M <- SAB_tag_metadata_M25 %>%
  mutate(year = year(UTC_RELEASE_DATE_TIME)) %>%
  group_by(common_name, year) %>%
  summarise(Released = n_distinct(animal_id), .groups = "drop")

yearly_detections_M <- SAB_core_M25_FINAL %>%   
  mutate(year = year(detection_timestamp_utc), common_name = commonName) %>%
  group_by(common_name, year) %>%
  summarise(Detected = n_distinct(animal_id), .groups = "drop")

# b. Calculate OVERALL totals from raw data (prevents double-counting individuals across years)
total_releases_M <- SAB_tag_metadata_M25 %>%
  group_by(common_name) %>%
  summarise(Total_Released = n_distinct(animal_id), .groups = "drop")

total_detections_M <- SAB_core_M25_FINAL %>%
  mutate(common_name = commonName) %>%
  group_by(common_name) %>%
  summarise(Total_Detected = n_distinct(animal_id), .groups = "drop")

totals_combined_M <- full_join(total_detections_M, total_releases_M, by = "common_name") %>%
  replace_na(list(Total_Detected = 0, Total_Released = 0)) %>%
  # Formats as "Total_Detected (Total_Released)" to match your yearly columns
  mutate(Total = paste0(Total_Detected, " (", Total_Released, ")")) %>%
  select(common_name, Total)

# c. Assemble the final table
Table_S1_M <- yearly_detections_M %>%
  full_join(yearly_releases_M, by = c("common_name", "year")) %>%
  replace_na(list(Detected = 0, Released = 0)) %>%
  mutate(Detected_vs_Released = paste0(Detected, " (", Released, ")")) %>%
  select(common_name, year, Detected_vs_Released) %>%
  # names_sort = TRUE forces the year columns into chronological order
  pivot_wider(
    names_from = year, 
    values_from = Detected_vs_Released, 
    values_fill = "0 (0)",
    names_sort = TRUE
  ) %>%
  # Attach the overall totals column to the far right
  left_join(totals_combined_M, by = "common_name")

# d. Calculate yearly totals across ALL species
yearly_totals_R <- SAB_tag_metadata_M25 %>%
  mutate(year = year(UTC_RELEASE_DATE_TIME)) %>%
  group_by(year) %>%
  summarise(Released = n_distinct(animal_id), .groups = "drop")

yearly_totals_D <- SAB_core_M25_FINAL %>%   
  mutate(year = year(detection_timestamp_utc)) %>%
  group_by(year) %>%
  summarise(Detected = n_distinct(animal_id), .groups = "drop")

# e. Calculate the absolute grand total (All years, all species)
grand_total_R <- n_distinct(SAB_tag_metadata_M25$animal_id)
grand_total_D <- n_distinct(SAB_core_M25_FINAL$animal_id)

# f. Format the bottom row to perfectly match the main table structure
bottom_row <- full_join(yearly_totals_D, yearly_totals_R, by = "year") %>%
  replace_na(list(Detected = 0, Released = 0)) %>%
  mutate(Detected_vs_Released = paste0(Detected, " (", Released, ")")) %>%
  select(year, Detected_vs_Released) %>%
  pivot_wider(
    names_from = year, 
    values_from = Detected_vs_Released, 
    values_fill = "0 (0)",
    names_sort = TRUE
  ) %>%
  # Add the label and the grand total column
  mutate(
    common_name = "Total",
    Total = paste0(grand_total_D, " (", grand_total_R, ")")
  ) %>%
  relocate(common_name) # Ensure 'common_name' is the first column

# g. Bind the bottom row to your existing table
Table_S1_M_Final <- bind_rows(Table_S1_M, bottom_row) %>%
  # If any years were present in one table but not the other, fill the resulting NAs
  mutate(across(everything(), ~replace_na(.x, "0 (0)")))

# View and save the final output
print(Table_S1_M_Final)

write.csv(Table_S1_M, "output/Table_S1_Annual_Det_vs_Rel_M.csv", row.names = FALSE)

# ---------------------------------------------------------
# 5. Table S2: Individual species counts by project code and location - qualified
# detections
# ---------------------------------------------------------

# a. Create the detailed breakdown (Species + Project + Location)
sp_count_summary_Q <- SAB_model_data_Q %>%
  group_by(common_name, Project_full, RELEASE_LOCATION) %>%
  summarise(
    Detections = n(),
    Individuals = n_distinct(animal_id),
    .groups = "drop",
  )

# b. Create the "Overall" species summary (to act as headers)
overall_sp_summary_Q <- SAB_model_data_Q %>%
  group_by(common_name) %>%
  summarise(
    Project_full = "---", 
    RELEASE_LOCATION = "--- ALL LOCATIONS ---",
    Detections = n(),
    Individuals = n_distinct(animal_id),
    .groups = "drop"
  )

# c. Combine, Sort, and Rename
Table_S2_Combined_Q <- bind_rows(overall_sp_summary_Q, sp_count_summary_Q) %>%
  # Sort by species name, then put the "ALL LOCATIONS" row first for each
  arrange(common_name, desc(RELEASE_LOCATION == "--- ALL LOCATIONS ---")) %>%
  rename(
    "Species" = common_name,
    "Project" = Project_full,
    "Release Location" = RELEASE_LOCATION
  )

# d. Apply Visual Aids with formattable
# Bold the "Overall" rows using a custom formatter
Table_S2_final <- formattable(Table_S2_Combined_Q, list(
  # Left-align text columns, Right-align numeric
  align = c("l", "l", "l", "r", "r"),
  
  # Format the Species column: Italics for fish names
  Species = formatter("span", style = x ~ style(font.style = "plain")),
  
  # Format the Release Location: Bold the 'ALL' rows
  `Release Location` = formatter("span", style = x ~ style(
    font.weight = ifelse(x == "--- ALL LOCATIONS ---", "bold", "normal"))),
  
  # Add the color bar visual aids
  Detections = color_bar("lightblue"),
  Individuals = color_tile("transparent", "lightgray")
))

# Save Table S2
write.csv(Table_S2_final,"output/Table_S2_FINAL.csv")

# Optional: Convert your formattable object to an htmlwidget - htmlwidgets package
library(htmlwidgets)
Table_s2_widget <- as.htmlwidget(Table_S2_final, width = "100%", height = "500px")

saveWidget(Table_s2_widget, "output/Table_S2_FINAL.html", selfcontained = TRUE)

# ---------------------------------------------------------
# 6. Summary table for Q dets release locations for mapping on Figure 1
# ---------------------------------------------------------
rel_loc_summary_Q <- SAB_glatos_Q25_FINAL %>%
  mutate(
    lat_num = as.numeric(RELEASE_LATITUDE),
    lon_num = as.numeric(RELEASE_LONGITUDE)
  ) %>%
  group_by(common_name, Project_full, RELEASE_LOCATION) %>%
  summarise(
    Detections = n(),
    Individuals = n_distinct(animal_id),
    # 1. Calculate the core means
    mean_lat = mean(lat_num, na.rm = TRUE),
    mean_lon = mean(lon_num, na.rm = TRUE),
    
    # 2. Check if there is more than one unique coordinate pair
    # We use n_distinct on a combination of lat and lon
    coord_spread = n_distinct(paste(lat_num, lon_num)),
    
    # 3. Conditional Range: Only show if there is more than 1 unique location
    min_lat = if_else(coord_spread > 1, as.character(round(min(lat_num, na.rm = TRUE), 4)), "-"),
    max_lat = if_else(coord_spread > 1, as.character(round(max(lat_num, na.rm = TRUE), 4)), "-"),
    min_lon = if_else(coord_spread > 1, as.character(round(min(lon_num, na.rm = TRUE), 4)), "-"),
    max_lon = if_else(coord_spread > 1, as.character(round(max(lon_num, na.rm = TRUE), 4)), "-"),
    .groups = "drop"
  ) %>%
  # Remove the helper column before finishing
  select(-coord_spread)

# save release locations for mapping Figure S5
write.csv(rel_loc_summary_Q,"output/Fig_1_Q_releaselocs.csv")
