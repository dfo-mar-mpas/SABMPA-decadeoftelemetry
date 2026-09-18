# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 01_datacleaning.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-08-20
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Cleans raw acoustic telemetry detections, applies 
#              spatial/temporal filters, and merges metadata for 
#              final manuscript datasets.
# ===================================================================

# ---------------------------------------------------------
# 0. Setup & Packages
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(tidyr)
library(glatos)

# ---------------------------------------------------------
# 1. Receiver Deployments
# ---------------------------------------------------------
# Load and format receiver metadata
sab_deploys_short <- read_csv("data/processed/sab_deploys_short.csv")
SAB_deployments_to2025 <- read_csv("data/processed/sab_deployments_glatos.csv")

SAB_deploys <- SAB_deployments_to2025 %>%
  left_join(
    sab_deploys_short %>% distinct(STATION_NO2, .keep_all = TRUE), 
    by = c("STATION_NO3" = "STATION_NO2")
  ) %>%
  mutate(
    # Clean OTN datetime format by removing 'T'
    deploy_date = as.POSIXct(gsub("T", " ", deploy_date), tz = "UTC"),
    recovery_date = as.POSIXct(gsub("T", " ", recovery_date), tz = "UTC")
  )

# ---------------------------------------------------------
# 2. Matched Detections (SAB Tagged Fish)
# ---------------------------------------------------------
# Combine annual matched detection files (2016-2025)
file_years <- 2016:2025
matched_SAB_to2025 <- bind_rows(
  lapply(file_years, function(yr) {
    read.csv(paste0("data/raw/sabmpa_matched_detections_", yr, ".csv")) %>% setDT()
  })
)

# Initial temporal and spatial filtering
matched_SAB_25_f1 <- matched_SAB_to2025 %>%
  # Remove 'release' rows (handled separately)
  filter(receiver != 'release') %>%
  # Remove Scatarie Bank receivers deployed in 2024
  filter(!station %in% c('SAB_SB1', 'SAB_SB2', 'SAB_SB3', 'SAB_SB4', 'SAB_SB5'))

# Create Table S1 Dataset (All Cleaned Final)
# Define spatial bounds
lat_north <- 47.00
lat_south <- 45.58
lon_east  <- -59.00
lon_west  <- -60.40

all_cleaned_final <- matched_SAB_25_f1 %>%
  filter(
    decimalLatitude >= lat_south & decimalLatitude <= lat_north,
    decimalLongitude >= lon_west & decimalLongitude <= lon_east
  )

# ---------------------------------------------------------
# 3. Format for GLATOS & Interpolation
# ---------------------------------------------------------
# Rename columns to match GLATOS standard requirements
matched_SAB_25_glatos <- all_cleaned_final %>%
  rename(
    detection_timestamp_utc = dateCollectedUTC,
    animal_id = organismID,
    receiver_sn = receiver,
    deploy_lat = decimalLatitude,
    deploy_long = decimalLongitude,
    transmitter_codespace = codeSpace,
    transmitter_id = tagName,
    sensor_value = sensorValue,
    sensor_unit = sensorUnit
  ) %>%
  mutate(
    detection_timestamp_utc = as.POSIXct(gsub("T", " ", detection_timestamp_utc), tz = "UTC")
  ) %>%
  glatos::as_glatos_detections()

# ---------------------------------------------------------
# 4. Qualified Detections (External Tags) & Metadata Merge
# ---------------------------------------------------------
# Load raw qualified detections and cleaned metadata
SAB_qdet25 <- read_csv("data/raw/SAB_qdets_2025.csv") 
qual_fishdata_clean <- read_csv("data/processed/qual_fishdata_Feb2026.csv") %>%
  filter(!is.na(fieldnumber)) %>%
  mutate(Project = recode(Project, "PBSM_QRPT" = "PBSM", "MIGRAMAR.LBTPCR" = "LBTPCR"))

# Remove 'messy' overlapping metadata from raw detections to prevent duplication
common_cols <- setdiff(intersect(names(SAB_qdet25), names(qual_fishdata_clean)), c("tag_ID", "fieldnumber"))
SAB_clean_for_join <- SAB_qdet25 %>%
  select(-any_of(common_cols)) %>%
  # Remove Scatarie Bank receivers
  filter(!station %in% c('SAB_SB1', 'SAB_SB2', 'SAB_SB3', 'SAB_SB4', 'SAB_SB5')) %>%
  # Standardize array names
  mutate(
    glatos_array = gsub('V2LSCF', 'SABMPA', glatos_array),
    local_area = gsub('V2LSCF', 'SABMPA', local_area),
    detectedby = gsub('V2LSCF', 'SABMPA', detectedby)
  )

# Merge clean metadata into qualified detections
SAB_enriched_final <- SAB_clean_for_join %>%
  left_join(qual_fishdata_clean, by = c("tag_ID" = "fieldnumber"))

# ---------------------------------------------------------
# 5. Export Processed Data
# ---------------------------------------------------------
# Save using .rds to drastically reduce file size and preserve formatting
saveRDS(SAB_deploys, "data/processed/SAB_deploys_final.rds")
saveRDS(all_cleaned_final, "data/processed/all_cleaned_final.rds")
saveRDS(matched_SAB_25_glatos, "data/processed/matched_SAB_25_glatos.rds")
saveRDS(SAB_enriched_final, "data/processed/SAB_enriched_final.rds")