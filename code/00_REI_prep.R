library(dplyr)
library(glatos)
library(tidyr)



# 1. Update your detection file to match the glatos structure
SAB_glatos_FINAL <- SAB_glatos_FINAL %>%
  separate(transmitter_id, 
           into = c("transmitter_codespace", "tag_ID"), 
           sep = "-(?=[^-]+$)", # Splits at the last hyphen
           remove = FALSE)    # Keeps the original combined column too
    
    # Recreate the full transmitter_id from the two pieces
    #SAB_glatos_FINAL <- SAB_glatos_FINAL %>%
    #  mutate(transmitter = paste(transmitter_codespace, transmitter_id, sep = "-"))

# 2. Update your deployment/metadata sheet if it has the same format
# (Ensure 'transmitter_id' or 'fieldnumber' is split the same way if used)

# 3. Quick check of the new columns
SAB_glatos_FINAL %>% 
  select(transmitter_codespace, tag_ID) %>% 
  head()


# --- A. Force Detections to glatos standard ---
# Ensure these names and types are EXACT
SAB_det_ready <- SAB_glatos_FINAL %>%
  transmute(
    animal_id = as.character(animal_id),
    detection_timestamp_utc = as.POSIXct(detection_timestamp_utc, tz = "UTC"),
    deploy_lat = as.numeric(deploy_lat),
    deploy_long = as.numeric(deploy_long),
    station = as.character(station),
    common_name_e = as.character(common_name),
    glatos_array = "SABMPA",
    utc_release_date_time = as.Date(UTC_RELEASE_DATE_TIME, tz = "UTC"),
    release_latitude = as.numeric(RELEASE_LATITUDE),
    release_longitude = as.numeric(RELEASE_LONGITUDE),
    capture_location = as.character(RELEASE_LOCATION),
    receiver_sn = as.character(station_SN),
    transmitter_codespace = as.character(transmitter_codespace),
    transmitter_id = as.character(tag_ID)
  )

colnames(SAB_det_ready)

# --- B. Force Deployments to glatos standard ---
# Note: 'deploy_date_time' and 'recover_date_time' are mandatory
SAB_dep_ready <- SAB_deploys_final %>%
  transmute(
    station = as.character(station_renamed),
    deploy_date_time = as.POSIXct(deploy_date, tz = "UTC"),
    recover_date_time = as.POSIXct(recovery_date, tz = "UTC"),
    deploy_lat = as.numeric(stn_lat),
    deploy_long = as.numeric(stn_long),
    glatos_array = "SABMPA"
  )

SAB_dep_ready <- SAB_dep_ready %>%  # remove NAs in recover date times for those not recovered
  mutate(recover_date_time = replace(recover_date_time,
                                      is.na(recover_date_time), 
                                      Sys.time()))


# --- C. Run the REI ---
# If it still gives an error, try adding: location_col = "station"
SAB_rei_results <- REI(SAB_det_ready, SAB_dep_ready)
SAB_rei_results

write.csv(SAB_dep_ready,"SAB_dep_ready.csv")
write.csv(SAB_det_ready,"SAB_det_ready.csv")


receivers <- read_glatos_receivers(SAB_dep_ready,"SAB_dep_ready.csv")
detections <- read_glatos_detections(SAB_det_ready)


# Add the 'glatos' class tag
#class(SAB_det_ready) <- c("glatos_detections", "data.frame")

# Add the 'glatos' class tag
#class(SAB_dep_ready) <- c("glatos_receivers", "data.frame")




