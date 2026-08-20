### SABMPA_decadeoftelemetry ###
# Script 02: Detection Filters
# Author: Harri Pettitt-Wade
# contact: harri.pettitt-wade@dfo-mpo.gc.ca
# Date Updated: 2026-08-20

# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# TODO: Add other detection filters
# TODO: tidy up script and test run

# ---------------------------------------------------------
# 0. Load project configuration and packages
source("code/00_setup.R")
# ---------------------------------------------------------
# --- STEP 1: False Detection Filter (MinLag) ---


# glatos uses a default threshold of 30 (detections within 30x the min_delay are valid)
SAB_filtered <- false_detections(SAB_glatos_Q25, tf = 3600)

# Count how many were flagged as false (passed_filter == 0)
num_false_detections <- sum(SAB_filtered$passed_filter == 0)

# Keep only the valid detections
SAB_step1 <- SAB_filtered %>% filter(passed_filter == 1)


# --- STEP 2: Remove Singletons ---
# Identify tags that only have 1 valid detection left in the whole dataset
SAB_glatos_Q25_filt <- SAB_glatos_Q25 %>%
  group_by(transmitter_id) %>%
  filter(n() > 1) %>%
  ungroup()

num_singletons_removed <- nrow(SAB_glatos_Q25) - nrow(SAB_glatos_Q25_filt)
num_singletons_removed

# --- SUMMARY TABLE ---
filter_summary <- data.frame(
  Step = c("Original Data", "Post-MinLag Filter", "Post-Singleton Removal"),
  Detections = c(nrow(SAB_glatos_Q25), nrow(SAB_step1), nrow(SAB_step2)),
  Removed = c(0, num_false_detections, num_singletons_removed)
)

print(filter_summary)


# ---------------------------------------------------------
# 2. predation tag 'digestion' filter

# check

SAB_predation_1 <- SAB_glatos_Q25_final %>%
  filter(sensorRaw == "1")

# 1. Create a dedicated dataset for predation detections
SAB_predation_data <- SAB_glatos_Q25_final %>%
  filter(Sensor == "D")

unique(SAB_glatos_Q25_final$Sensor)

# 2. Filter the main dataset to only include 'alive' or 'normal' detections
# This removes the 'D' tags to ensure your residency analysis isn't skewed by a predator
SAB_glatos_Q25_clean <- SAB_glatos_Q25_final %>%
  filter(Sensor != "D" | is.na(Sensor))

# filter based on the animal IDs
SAB_glatos_Q25_clean <- SAB_glatos_Q25_final %>%
  filter(Sensor != "D" | is.na(Sensor))

# Summary of the discovery
message("Predation events detected by SensorRaw: ", nrow(SAB_predation_1))
message("Predation events detected: ", nrow(SAB_predation_data))
message("Number of unique predated tags: ", n_distinct(SAB_predation_data$animal_id))

## --- removing all the predation tag data because the D and P sensors alternative after digestion starts (D)

message("Main dataset initial ", n_distinct(SAB_glatos_Q25_final$animal_id), " live individuals.")

# 1. Get the list of biological individuals that triggered the 'D' sensor
predated_animal_ids <- unique(SAB_predation_data$animal_id)

# 2. Filter the main dataset to exclude these specific individuals entirely
SAB_glatos_Q25_clean <- SAB_glatos_Q25_final %>%
  filter(!(animal_id %in% predated_animal_ids))

# 3. Save the predated individuals separately for your "Predation Case Study"
SAB_predation_case_studies <- SAB_glatos_Q25_final %>%
  filter(animal_id %in% predated_animal_ids)

# Verification
message("Removed ", length(predated_animal_ids), " predated individuals from the main dataset.")
message("Main dataset now contains ", n_distinct(SAB_glatos_Q25_clean$animal_id), " live individuals.")

write.csv(SAB_predation_case_studies,"SAB_predation_case_studies.csv")

# ---------------------------------------------------------
# . Mort filter
### ----------------- mort filter for qdets (Atlantic cod) ---------------------------

# note - this is unlikely for qualified detections unless they tagged inside the MPA

# 1. Filter for Atlantic Cod only
cod_dets <- SAB_glatos_Q25_clean %>% 
  filter(common_name == "Atlantic cod")
head(cod_dets)

head(SAB_glatos_Q25_clean)
nrow(SAB_glatos_Q25_clean)

# 2. Identify "Stationary" tags (Manual Check for REI Cleaning)
# We look for fish only seen at ONE station for a massive window of time
stationary_tags <- cod_dets %>%
  group_by(animal_id) %>%
  summarise(
    n_stations = n_distinct(station),
    first_det = min(detection_timestamp_utc),
    last_det = max(detection_timestamp_utc),
    duration_days = as.numeric(difftime(last_det, first_det, units = "days")),
    .groups = "drop"
  ) %>%
  # Threshold: If seen at only 1 station for > 10 days, it's likely a shed/mort
  filter(n_stations == 1 & duration_days > 10)

message("Found ", nrow(stationary_tags), " Cod tags that appear stationary (10+ days at 1 station).")



# ---------------------------------------------------------
# . Speed filters
SAB_speed_categorized_final <- SAB_speed_eval %>%
  mutate(final_status = case_when(
    # 1. ALWAYS KEEP the first detection of every fish
    is.na(velocity_ms) ~ "Keep: Initial",
    
    # 2. FLAG 'Ghost' pings (Physically impossible for ANY species)
    velocity_ms > 35 ~ "Flag: Ghost",
    
    # 3. FLAG 'Slow Prey' (Cod/Salmon/Eel/Crab/Sturgeon/Mackerel/Halibut > 5 m/s)
    velocity_ms > 5 & common_name %in% c("Atlantic cod", "Atlantic salmon", "Atlantic halibut",
                                         "American eel", "Atlantic mackerel",
                                         "Snow crab", "Atlantic sturgeon") ~ "Flag: Predator/Error",
    
    # 4. FLAG 'Intermediate' (Species that can reach ~10 m/s but not 35 m/s)
    # This keeps your White shark, Mako, Leatherback, and Seals safe up to 10 m/s
    velocity_ms > 10 & !common_name %in% c("White shark", "Grey seal", "Shortfin mako", 
                                           "Leatherback turtle", "Porbeagle shark",
                                           "Bluefin tuna", "Blue shark") ~ "Flag: Predator/Error",
    
    # 5. KEEP High-speed Migrants (Tuna and Blue sharks can burst > 10 m/s)
    velocity_ms > 10 & common_name %in% c("Bluefin tuna", "Blue shark") ~ "Keep: High-Speed",
    
    # 6. KEEP everything else (Standard swimming < 5 m/s)
    TRUE ~ "Keep: Valid Biological"
  ))

speed_summary_table <- SAB_speed_categorized_final %>%
  group_by(common_name) %>%
  summarise(
    Total_n = n(),
    # Count how many are flagged for removal
    Removed_n = sum(grepl("Flag", final_status)),
    # Count how many are kept
    Kept_n = sum(grepl("Keep", final_status)),
    # Calculate percentages
    Pct_Removed = round((Removed_n / Total_n) * 100, 1),
    Pct_Kept = round((Kept_n / Total_n) * 100, 1),
    .groups = "drop"
  ) %>%
  # Create a professional string for the table
  mutate(
    Detections_Removed = paste0(Removed_n, " (", Pct_Removed, "%)"),
    Detections_Kept = paste0(Kept_n, " (", Pct_Kept, "%)")
  ) %>%
  select(common_name, Total_n, Detections_Removed, Detections_Kept)

# 1. Print the table to the console
print(speed_summary_table)

# 2. Save it as a CSV for your Supplementary Materials
write.csv(speed_summary_table,"SAB_Q25_speedsum_table.csv", row.names = FALSE)

#SAB_glatos_Q25_prespeed <- SAB_glatos_Q25_clean # backup

# 1. Count detections BEFORE filtering
pre_filter_n <- nrow(SAB_speed_categorized_final)

# 2. Create the final "Clean" dataset
# We keep only the rows we labeled with 'Keep'
SAB_glatos_Q25_FINAL <- SAB_speed_categorized_final %>%
  filter(grepl("Keep", final_status))

# 3. Count detections AFTER filtering
post_filter_n <- nrow(SAB_glatos_Q25_FINAL)

# 4. Calculate the difference
removed_n <- pre_filter_n - post_filter_n
pct_lost <- round((removed_n / pre_filter_n) * 100, 2)

# --- Print the Result ---
cat("--- DATA FILTERING SUMMARY ---\n",
    "Total Raw Detections:    ", pre_filter_n, "\n",
    "Total Filtered Detections: ", post_filter_n, "\n",
    "Detections Removed:        ", removed_n, " (", pct_lost, "% of dataset)\n",
    "------------------------------", sep = "")


# Count unique individuals before and after
pre_indiv <- n_distinct(SAB_speed_categorized_final$animal_id)
post_indiv <- n_distinct(SAB_glatos_Q25_FINAL$animal_id)

cat("\n--- INDIVIDUAL IMPACT ---\n",
    "Unique Animals (Pre):  ", pre_indiv, "\n",
    "Unique Animals (Post): ", post_indiv, "\n",
    "Individuals Lost:      ", pre_indiv - post_indiv, "\n",
    "-------------------------", sep = "")



### ---------------------- OTHER CHECKS --------------------------------

### checking initial count matches individuals detected count - i.e. that the speed filter is done properly
SAB_final_speed_categorized %>%
  group_by(common_name) %>%
  summarise(
    Unique_Animals = n_distinct(animal_id),
    Initial_Detections_Count = sum(final_status == "Keep: Initial Detection")
  )

#### ----------------- filtering by speed based on behavioural shift


library(dplyr)

# 1. Identify individuals with SUSTAINED high-speed behavior
predation_suspects <- SAB_glatos_Q25_FINAL %>%
  filter(common_name %in% c("Atlantic cod", "Atlantic salmon", "American eel")) %>%
  group_by(animal_id) %>%
  summarise(
    total_dets = n(),
    # How many times did it 'speed' (excluding the supersonic ghosts)
    speeding_events = sum(velocity_ms > 2 & velocity_ms < 10, na.rm = TRUE),
    pct_speeding = (speeding_events / total_dets) * 100,
    max_bio_velocity = max(velocity_ms[velocity_ms < 10], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Flag if more than 20% of their detections imply high-speed movement
  filter(pct_speeding > 20 & total_dets > 5)

print(predation_suspects)

# removed two individuals
#### ---

# 1. Identify the IDs to be removed entirely
ids_to_remove <- predation_suspects$animal_id

# 2. Create the Final Master Dataset
SAB_glatos_Q25_FINAL <- SAB_speed_categorized %>%
  # Remove the 'Supersonic Ghosts' (>50 m/s or >10 m/s depending on your tier)
  filter(detection_type != "Ghost >50 m/s") %>%
  # Remove the specific individuals confirmed as predators
  filter(!(animal_id %in% ids_to_remove)) %>%
  # Keep only the valid biological and initial pings
  filter(detection_type %in% c("Valid Biological", "Initial/Single", "Large Migrant Burst"))

message("Final Cleanup Complete.")
message("Total Individuals: ", n_distinct(SAB_glatos_FINAL$animal_id))
