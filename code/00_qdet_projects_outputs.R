## SABMPA qdet data summaries for other projects detected

# Outputs for projects detected on the SABMPA array.

# 1. Table of unique transmitter_id detected by project (Project_OTN) with summarized detections, 
# first detection, last detection, list of stations detected, 
# mean_latitude (deploy_lat), mean_longitude (deploy_long), 

# include the all unique values in the following columns for this data: common_name, animal_id (for my ref as I made this), animal_id_2, 
# contactPOC, contactPI, NOTES, Action, tagging_data

colnames(SAB_glatos_Q25_final)
head(SAB_glatos_Q25_final)

library(dplyr)
library(tidyr)

# 1. Create the detailed summary by Tag and Project
external_project_summary <- SAB_glatos_FINAL %>%
  # Group by the Project ID and Transmitter
  group_by(Project_OTN, transmitter_id) %>%
  summarise(
    Total_Detections = n(),
    First_Detection = min(detection_timestamp_utc, na.rm = TRUE),
    Last_Detection = max(detection_timestamp_utc, na.rm = TRUE),
    
    # Create a comma-separated list of unique stations
    Station_List = paste(sort(unique(station)), collapse = ", "),
    
    # Calculate the mean location of detections in the MPA
    Mean_Lat_MPA = round(mean(deploy_lat, na.rm = TRUE), 5),
    Mean_Lon_MPA = round(mean(deploy_long, na.rm = TRUE), 5),
    
    # Collapse metadata columns to unique values (in case of variations)
    Species = paste(unique(common_name), collapse = "; "),
    Internal_ID = paste(unique(animal_id), collapse = "; "),
    External_ID = paste(unique(animal_id_2), collapse = "; "),
    POC = paste(unique(contactPOC), collapse = "; "),
    PI = paste(unique(contactPI), collapse = "; "),
    Notes_Metadata = paste(unique(NOTES), collapse = "; "),
    Action_Status = paste(unique(Action), collapse = "; "),
    Tag_metadata = paste(unique(tagging_data), collapse = "; "),
    
    .groups = "drop"
  ) %>%
  # Sort by Project and then the most active tags
  arrange(Project_OTN, desc(Total_Detections))

# 2. View the table
print(external_project_summary)

# 3. Save for the Information Package
write.csv(external_project_summary, "SABMPA_External_Project_Reports_Q25.csv", row.names = FALSE)


### ---------------

### ------- with 10 year data outputs

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(purrr)
library(sf)

#SAB_glatos_Q25_final

# 1. Setup Master Contact List
# Includes count of total animals vs. those specifically missing release dates
master_contact_list <- SAB_glatos_FINAL %>%
  filter(Project_OTN %in% still_missing$Project_OTN) %>%
  group_by(Project_OTN) %>%
  summarise(
    Principal_Investigator = paste(unique(na.omit(contactPI)), collapse = "; "),
    Point_of_Contact = paste(unique(na.omit(contactPOC)), collapse = "; "),
    Total_Animals_Detected = n_distinct(animal_id),
    Animals_Missing_Release_Date = n_distinct(animal_id[is.na(UTC_RELEASE_DATE_TIME)]),
    Species_List = paste(unique(common_name), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(Animals_Missing_Release_Date))

if (!dir.exists("Project_Outreach_Packages")) dir.create("Project_Outreach_Packages")
write.csv(master_contact_list, "Project_Outreach_Packages/MASTER_Contact_List.csv", row.names = FALSE)

# 2. Automation Loop for Project Folders
# for just projects missing tag info - use the file 'still_missing' and filter by Project_OTN
walk(unique(SAB_glatos_FINAL$Project_OTN), function(proj_id) {
  
  # --- Setup ---
  proj_folder <- file.path("Project_Outreach_Packages", proj_id)
  if (!dir.exists(proj_folder)) dir.create(proj_folder)
  
  # Filter Data for this project
  proj_dets <- SAB_glatos_FINAL %>% filter(Project_OTN == proj_id)
  
  # --- Part A: Data Summary CSV ---
  proj_summary <- proj_dets %>%
    group_by(transmitter_id) %>%
    summarise(
      Species = paste(unique(common_name), collapse = "; "),
      Total_Detections = n(),
      First_Detection = min(detection_timestamp_utc, na.rm = TRUE),
      Last_Detection = max(detection_timestamp_utc, na.rm = TRUE),
      Missing_Release_Date = any(is.na(UTC_RELEASE_DATE_TIME)),
      .groups = "drop"
    )
  write.csv(proj_summary, file.path(proj_folder, paste0("Summary_", proj_id, ".csv")), row.names = FALSE)
  
  # --- Part B: Temporal Bar Plots (Gray) ---
  proj_temp_data <- proj_dets %>%
    mutate(day = as.Date(detection_timestamp_utc),
           month_bin = floor_date(detection_timestamp_utc, unit = "month")) %>%
    group_by(month_bin) %>%
    summarise(days_detected = n_distinct(day, animal_id),
              unique_indiv = n_distinct(animal_id), .groups = "drop")
  
  p_temp <- (
    ggplot(proj_temp_data, aes(x = month_bin, y = days_detected)) +
      geom_col(fill = "gray40", color = "black", width = 2500000, just = 0, linewidth = 0.1) +
      scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
      theme_classic() + labs(title = paste("Temporal Activity:", proj_id), y = "Detection Days", x = NULL)
  ) / (
    ggplot(proj_temp_data, aes(x = month_bin, y = unique_indiv)) +
      geom_col(fill = "gray60", color = "black", width = 2500000, just = 0, linewidth = 0.1) +
      scale_x_datetime(date_breaks = "3 month", date_labels = "%Y %m", expand = c(0,0)) +
      theme_classic() + labs(y = "Unique Individuals", x = "Year") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  ggsave(file.path(proj_folder, paste0("Temporal_Plot_", proj_id, ".png")), p_temp, width = 8, height = 7)
  
  # --- Part C: Spatial Bubble Map - ANIMALS ---
  proj_map_data <- proj_dets %>%
    group_by(station) %>%
    summarise(num_fish = n(),
              lat = mean(deploy_lat), lon = mean(deploy_long), .groups = "drop")
  
  p_map <- ggplot() +
    geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", alpha = 0.2) +
    # Background: All receivers
    geom_point(data = SAB_glatos_FINAL %>% distinct(station, deploy_long, deploy_lat),
               aes(x = deploy_long, y = deploy_lat), shape = 1, color = "gray80", size = 1.5) +
    # Project-specific detections
    geom_point(data = proj_map_data, aes(x = lon, y = lat, size = num_fish), 
               color = "green", alpha = 0.7) +
    coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
    theme_minimal() +
    labs(title = paste("Spatial Distribution:", proj_id), size = "Individuals", x = NULL, y = NULL)
  
  ggsave(file.path(proj_folder, paste0("Spatial_Map1_", proj_id, ".png")), p_map, width = 8, height = 6)
  
  # --- Part D: Spatial Bubble Map - DETECTIONS ---
  proj_map_data2 <- proj_dets %>%
    group_by(station,animal_id) %>%
    summarise(num_fish = n_distinct(animal_id),
              lat = mean(deploy_lat), lon = mean(deploy_long), .groups = "drop")
  
  p_map <- ggplot() +
    geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", alpha = 0.2) +
    # Background: All receivers
    geom_point(data = SAB_glatos_FINAL %>% distinct(station, deploy_long, deploy_lat),
               aes(x = deploy_long, y = deploy_lat), shape = 1, color = "gray80", size = 1.5) +
    # Project-specific detections
    geom_point(data = proj_map_data2, aes(x = lon, y = lat, size = num_fish,color = as.factor(animal_id)), 
               alpha = 0.7) +
    scale_color_viridis_d(name = "Animal", option = "viridis") +
    coord_sf(xlim = c(extent["xmin"], extent["xmax"]), ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
    theme_minimal() +
    labs(title = paste("Spatial Distribution:", proj_id), size = "Individuals", color = "Animal",x = NULL, y = NULL)
  
  ggsave(file.path(proj_folder, paste0("Spatial_Map2_", proj_id, ".png")), p_map, width = 8, height = 6)
})

message("Finished! Check 'Project_Outreach_Packages' for ", length(unique(SAB_glatos_FINAL$Project_OTN)), " folders.")


### -------------------- missing contact list that includes all projects (even not missing data) ---------


library(dplyr)

master_contact_list_all <- SAB_glatos_Q25_final %>%
  group_by(Project_OTN) %>%
  summarise(
    Principal_Investigator = paste(unique(na.omit(contactPI)), collapse = "; "),
    Point_of_Contact = paste(unique(na.omit(contactPOC)), collapse = "; "),
    Total_Tags_Detected = n_distinct(transmitter_id),
    Total_Animals_Detected = n_distinct(animal_id),
    # Count how many fish from this project are missing release dates
    Animals_Missing_Release_Date = n_distinct(animal_id[is.na(UTC_RELEASE_DATE_TIME)]),
    Species_List = paste(unique(common_name), collapse = ", "),
    .groups = "drop"
  ) %>%
  # Sort by total animals detected to see your biggest contributors
  arrange(desc(Total_Animals_Detected))

# Save the complete list
write.csv(master_contact_list_all, 
          "Project_Outreach_Packages/MASTER_Contact_List_ALL.csv", 
          row.names = FALSE)

# Print a preview
print(master_contact_list_all)

