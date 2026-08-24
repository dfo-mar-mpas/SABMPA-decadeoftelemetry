### SAB - running stats on the array split

## ------------ REI

library(dplyr)
library(glatos)

head(detections)
head(receivers)

library(dplyr)

# 1. Update the 'glatos_array' column in both detections and receivers
# This replaces "SABMPA" with the specific design name
SAB_det_ready <- SAB_det_ready %>%
  mutate(glatos_array = case_when(
    grepl("^2021_", station) ~ "Single Gate (2021)",
    grepl("^N|^S", station) ~ "Double Gate (N/S)",
    TRUE ~ "Other"
  ))

unique(SAB_det_ready$glatos_array)

SAB_dep_ready <- SAB_dep_ready %>%
  mutate(glatos_array = case_when(
    grepl("^2021_", station) ~ "Single Gate (2021)",
    grepl("^N|^S", station) ~ "Double Gate (N/S)",
    TRUE ~ "Other"
  ))

# 2. Split the data based on the new 'glatos_array' values
det_double <- SAB_det_ready %>% filter(glatos_array == "Double Gate (N/S)")
rec_double <- SAB_dep_ready %>% filter(glatos_array == "Double Gate (N/S)")

det_single <- SAB_det_ready %>% filter(glatos_array == "Single Gate (2021)")
rec_single <- SAB_dep_ready %>% filter(glatos_array == "Single Gate (2021)")

# 3. Run REI for each design independently
rei_double_results <- glatos::REI(det_double, rec_double) %>% 
  mutate(array_design = "Double Gate (N/S)")
rei_double_results

rei_single_results <- glatos::REI(det_single, rec_single) %>% 
  mutate(array_design = "Single Gate (2021)")
rei_single_results

library(dplyr)

# 1. Ensure you have the two separate result objects
# rei_double_results and rei_single_results

# 2. Combine them into one data frame
SAB_rei_final_comparison <- bind_rows(
  rei_double_results %>% mutate(array_design = "Double Gate (N/S)"),
  rei_single_results %>% mutate(array_design = "Single Gate (2021)")
)

# 3. Calculate the Mean REI per Array Design
# This must be done using group_by() to prevent global averaging
SAB_rei_summary <- SAB_rei_final_comparison %>%
  group_by(array_design) %>%
  summarise(
    Mean_REI = mean(rei, na.rm = TRUE),
    Median_REI = median(rei, na.rm = TRUE),
    Total_Stations = n(),
    .groups = "drop"
  )

print(SAB_rei_summary)

# Check the range of scores for each design
range(rei_double_results$rei)
range(rei_single_results$rei)


## ----------- with percentages seperate

library(dplyr)

# 1. Calculate REI for Double Gate (Era 1)
rei_double <- glatos::REI(det_double, rec_double) %>%
  mutate(
    array_design = "Double Gate (N/S)",
    # Percentage relative ONLY to other Double Gate stations
    rei_pct_within_era = (rei / sum(rei, na.rm = TRUE)) * 100
  )

# 2. Calculate REI for Single Gate (Era 2)
rei_single <- glatos::REI(det_single, rec_single) %>%
  mutate(
    array_design = "Single Gate (2021)",
    # Percentage relative ONLY to other Single Gate stations
    rei_pct_within_era = (rei / sum(rei, na.rm = TRUE)) * 100
  )

# 3. Combine and Summary
SAB_rei_final_comparison <- bind_rows(rei_double, rei_single)

SAB_rei_summary <- SAB_rei_final_comparison %>%
  group_by(array_design) %>%
  summarise(
    n_stations = n(),
    mean_rei = mean(rei, na.rm = TRUE),
    median_rei = median(rei, na.rm = TRUE),
    sd_rei = sd(rei, na.rm = TRUE), # Check the standard deviation
    sum_rei = sum(rei, na.rm = TRUE)
  )

print(rei_single)

print(SAB_rei_summary) # mean values are the same because rei is a ratio 0-1 and the two arrays have same total stations.
# but not by year. I really should run this by year/month and use that variation in models and outputs. 


SAB_rei_sum_singleG <- rei_single %>%
  summarise(
    n_stations = n(),
    mean_rei = mean(rei, na.rm = TRUE),
    median_rei = median(rei, na.rm = TRUE),
    sd_rei = sd(rei, na.rm = TRUE), # Check the standard deviation
    sum_rei = sum(rei, na.rm = TRUE)
  )

SAB_rei_sum_doubleG <- rei_double %>%
  summarise(
    n_stations = n(),
    mean_rei = mean(rei, na.rm = TRUE),
    median_rei = median(rei, na.rm = TRUE),
    sd_rei = sd(rei, na.rm = TRUE), # Check the standard deviation
    sum_rei = sum(rei, na.rm = TRUE)
  )

print(SAB_rei_sum_singleG)
print(SAB_rei_sum_doubleG)


library(dplyr)

#  Calculate Global Parameters per Array Design

# Calculate the 'a' variables (Total Array) separately for each design
array_globals <- SAB_det_ready %>%
  group_by(glatos_array) %>%
  summarise(
    Ta = n_distinct(animal_id),
    Sa = n_distinct(common_name_e),
    DDa = n_distinct(as.Date(detection_timestamp_utc)),
    .groups = "drop"
  ) %>%
  # Join with deployment data to get Da (Total active days for that design)
  left_join(
    SAB_dep_ready %>%
      group_by(glatos_array) %>%
      summarise(Da = as.numeric(difftime(max(recover_date_time), 
                                         min(deploy_date_time), units = "days"))),
    by = "glatos_array"
  )


# Calculate Station Metrics and the Independent REI

# 1. Station-specific stats (Tr, Sr, DDr)
rei_stats <- SAB_det_ready %>%
  group_by(glatos_array, station) %>%
  summarise(
    Tr = n_distinct(animal_id),
    Sr = n_distinct(common_name_e),
    DDr = n_distinct(as.Date(detection_timestamp_utc)),
    .groups = "drop"
  )

# 2. Join with Deployments and Global Constants
SAB_rei_final <- SAB_dep_ready %>%
  group_by(glatos_array, station) %>%
  summarise(
    Dr = sum(as.numeric(difftime(recover_date_time, deploy_date_time, units = "days"))),
    deploy_lat = mean(deploy_lat),
    deploy_long = mean(deploy_long),
    .groups = "drop"
  ) %>%
  # Bring in station stats
  left_join(rei_stats, by = c("glatos_array", "station")) %>%
  # Bring in the Era-specific Global Constants (Ta, Sa, DDa, Da)
  left_join(array_globals, by = "glatos_array") %>%
  # Fill silent stations
  mutate(across(c(Tr, Sr, DDr), ~tidyr::replace_na(., 0))) %>%
  # 3. Apply Equation using ERA-SPECIFIC values
  mutate(
    REI_Score = (Tr / Ta) * (Sr / Sa) * (DDr / DDa) * (Da / Dr),
    # Create the relative percentage within each design
    rei_pct_within_era = (REI_Score / sum(REI_Score, na.rm = TRUE)) * 100
  ) %>%
  rename(array_design = glatos_array)


### ----- plotting array separately on same map 
# different rei run for each array

# data set for rei run seperately for double and single gate array
SAB_rei_final_comparison

# Join the species count (Sr) to your mapping object if not already there
array <- array %>%
  left_join(SAB_rei_final %>% select(station, Sr), by = "station")

array_split 
  
#load the coordinates for the new array
array <- read.csv("r:/Science/CESD/HES_MPAGroup/Projects/Acoustic/SAB/OTN_redesign_coords.csv")%>%
  st_as_sf(coords=c("long","lat"),crs=latlong)

#load the coordinates for all SAB arrays
array <- read.csv("data/metadata/Receiver metadata/SABMPA_rcv_glatos_Feb2024.csv")%>%
  st_as_sf(coords=c("DEPLOY_LONG","DEPLOY_LAT"),crs=latlong)
  
#load coordinates based on the rei
#array <- rei %>%
#  st_as_sf(coords=c("longitude","latitude"),crs=latlong)

# 1. Join Zone info to your station array mapping object
# Match 'station' from the array to 'station_renamed' from the deployments
#array <- array %>%
#  left_join(SAB_deploys_final %>% 
#              distinct(station_renamed, Zone), 
#            by = c("station" = "station_renamed"))

#load coordinates based on the rei in SAB_rei_final_comparison - rei run separately by array and combined afterwards
array_split <- SAB_rei_final_comparison %>%
  st_as_sf(coords=c("longitude","latitude"),crs=latlong)


# 1. Join Zone info to your station array mapping object
# Match 'station' from the array to 'station_renamed' from the deployments
#array_split <- array_split %>%
#  left_join(SAB_deploys_final %>% 
#              distinct(station_old, Zone), 
#            by = c("station" = "station_old")) - this would be to replace station with station_old

# Join metadata while keeping the current 'station' name
array_split <- array_split %>%
  left_join(SAB_deploys_final %>% 
              dplyr::select(station, Zone, Assigned_c), 
            by = "station")

library(dplyr)

# 1. Select the relevant columns from your final independent REI table
# This ensures you bring over Sr, REI_Score, and the within-era percentages
array_split <- array_split %>%
  left_join(SAB_rei_final %>% 
              dplyr::select(station, Tr, Sr, DDr, REI_Score, rei_pct_within_era, Da, Dr, Ta, Sa, DDa), 
            by = "station")

array_split <- array_split %>%
  left_join(SAB_rei_final %>% 
              dplyr::select(station, Sr), 
            by = "station")

# 2. Verify that 'Sr' is now a numeric column in your sf object
summary(array_split$Sr)


print(array_split)

colnames(SAB_deploys_final)
unique(SAB_deploys_updated$station_name)
unique(SAB_deploys_updated$station)
colnames(SAB_deploys_updated)

p1_rei <- ggplot() +
  geom_sf(data = basemap) +
  # Set alpha to 1 for solid, accurate habitat colors
  geom_sf(data = sab_bs_clipped, aes(fill = Assigned_c), color = NA, alpha = 1) +
  # Bold MPA Border
  geom_sf(data = sab, fill = NA, color = "gray29", linewidth = 1) +
  
  # REI Bubbles (Keep these on top)
  geom_point(data = array_split, 
             aes(geometry = geometry, size = rei_pct_within_era, color = Sr), 
             stat = "sf_coordinates", alpha = 0.7) +
  
  geom_point(data = array_split, 
             aes(geometry = geometry), 
             stat = "sf_coordinates", 
             shape = 20, color = "gray1", size = 2) +
  
  theme_bw() +
  scale_fill_manual(name = "Benthoscape habitat", values = habitat_cols) +
  scale_color_viridis_c(name = "Species richness", option = "viridis", direction = -1) +
  scale_size_continuous(name = "REI %", range = c(2, 12)
                        )

p1_rei


## ------ refining


library(dplyr)

# Update the zone column to match your naming convention
sab <- sab %>%
  mutate(zone = case_when(
    Zone == "2a" ~ "2",
    Zone == "2b" ~ "3",
    Zone == "2c" ~ "4",
    TRUE ~ as.character(Zone) # Keeps Zone 1 as is
  ))



p1_rei_final <- ggplot() +
  # 1. Base Habitat Layer (Uses the 'fill' scale)
  geom_sf(data = basemap) +
  geom_sf(data = sab_bs_clipped, aes(fill = Assigned_c), color = NA, alpha = 1) +
  scale_fill_manual(name = "Benthoscape habitat", values = habitat_cols) +
  
  # 2. Station Positions (Background Dots)
  # We use 'shape = 1' (open circle) and manual colors
  geom_point(data = array_split, 
             aes(geometry = geometry, color = array_design), 
             stat = "sf_coordinates", 
             shape = 1, size = 2, stroke = 1.2) +
  scale_color_manual(name = "Array design", 
                     values = c("Single Gate (2021)" = "black", 
                                "Double Gate (N/S)" = "red")) +
  
  # 3. REI Bubbles (Sized by REI%, Colored by Species Richness)
  # New scale: 'new_scale_color' or simply using a different aesthetic
  # To avoid the 'fill' conflict, we map Sr to a NEW color scale using ggnewscale
  # If you don't have ggnewscale, we will use 'color' for Sr and 'shape' for design
  ggnewscale::new_scale_color() + 
  geom_point(data = array_split, 
             aes(geometry = geometry, size = rei_pct_within_era, color = Sr), 
             stat = "sf_coordinates", 
             alpha = 0.7) +
  scale_color_viridis_c(name = "Species richness", option = "viridis", direction = -1) +
  
  # 4. Zone Numbers
  geom_sf_text(data = sab, aes(label = Zone), 
               size = 4, fontface = "bold", color = "black") +
  
  # 5. Bold MPA Border
  geom_sf(data = sab, fill = NA, color = "darkblue", linewidth = 1) +
  
  scale_size_continuous(name = "REI %", range = c(2, 12)) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        legend.text = element_text(size = 8)) +
  labs(x = "Longitude", y = "Latitude")

p1_rei_final


## -- repositioning labels

# 1. Force the design names to match the manual scale exactly
array_split <- array_split %>%
  mutate(array_design = case_when(
    grepl("^2021", station) ~ "Single Gate (2021-2025)",
    grepl("^N|^S", station) ~ "Double Gate (2015-2021)",
    TRUE ~ as.character(array_design)
  ))

# Update the zone column to match your naming convention
sab <- sab %>%
  mutate(Zone = case_when(
    Zone == "2a" ~ "2",
    Zone == "2b" ~ "3",
    Zone == "2c" ~ "4",
    TRUE ~ as.character(Zone) # Keeps Zone 1 as is
  ))

# 1. Create a dataset for standard labels (Zones 1, 2, 3)
standard_labels <- sab %>% filter(Zone != "4")

# 2. Create a dataset for the 'problem' label (Zone 4)
Zone_4_label <- sab %>% filter(Zone == "4")

library(ggplot2)
library(sf)
library(ggnewscale)

p1_rei_final <- ggplot() +
  # 1. BASE LAYERS (Bottom)
  geom_sf(data = basemap) +
  geom_sf(data = sab_bs_clipped, aes(fill = Assigned_c), color = NA, alpha = 1) +
  scale_fill_manual(name = "Benthoscape habitat", values = habitat_cols) +
  
  # 2. BOLD MPA BORDER
  geom_sf(data = sab, fill = NA, color = "gray23", linewidth = 1) +
  
  # 3. REI BUBBLES (Middle Layer)
  # Using 'new_scale_color' here first to handle Species Richness
  new_scale_color() + 
  geom_point(data = array_split, 
             aes(geometry = geometry, size = rei_pct_within_era, color = Sr), 
             stat = "sf_coordinates", alpha = 0.8) +
  scale_color_viridis_c(name = "Species richness", option = "viridis", direction = -1) +
  scale_size_continuous(name = "REI %", range = c(2, 12)) +

  # 4. STATION POSITIONS (Top Layer - Black/Red circles)
  # We start ANOTHER new scale to prevent conflict with Sr
  new_scale_color() + 
  geom_point(data = array_split, 
             aes(geometry = geometry, color = array_design), 
             stat = "sf_coordinates", shape = 20, size = 1, stroke = 1) +
  scale_color_manual(name = "Array design", 
                     values = c("Single Gate (2021-2025)" = "black", 
                                "Double Gate (2015-2021)" = "red")) +

  # 5. Zone LABELS
  # Standard positions for 1, 2, 3
  geom_sf_text(data = sab %>% filter(Zone != "4"), aes(label = Zone), 
               size = 5, fontface = "bold", color = "black") +
  # Nudged position for Zone 4 (East and South)
  geom_sf_text(data = sab %>% filter(Zone == "4"), aes(label = Zone), 
               size = 5, fontface = "bold", color = "black",
               nudge_x = 0.08, nudge_y = -0.05) +
  
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        legend.spacing.y = unit(0, "cm")) +
  labs(x = NULL, y = NULL)

p1_rei_final



### export

library(ggplot2)

# 1. Export as a high-resolution TIFF (Standard for final journal gallery)
# LZW compression is used to keep the 300 DPI file size manageable
ggsave("Figure_REI_Map_Final.tiff", 
       plot = p1_rei_final, 
       width = 7.5,      # Full column width for portrait page
       height = 5.0,     # Half-page vertical height
       units = "in", 
       dpi = 300, 
       compression = "lzw")

# 2. Export as a high-resolution PNG (Standard for drafts and presentations)
ggsave("Figure_REI_Map_Final.png", 
       plot = p1_rei_final, 
       width = 7.5, 
       height = 5.0, 
       units = "in", 
       dpi = 300)



p1_rei_final <- p1_rei_final +
  guides(
    # Set the habitat legend to 3 columns
    fill = guide_legend(ncol = 3, byrow = TRUE, order = 1),
    # Keep the other legends organized
    size = guide_legend(ncol = 2, order = 2),
    color = guide_colorbar(order = 3)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    # Reduce text size slightly if it still feels crowded
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )

# Re-run the export
ggsave("Figure_REI_Map_3Column.png", width = 7.5, height = 5, dpi = 300)



p1_rei_export <- p1_rei_final +
  # 1. Shrink the REI Bubbles
  # Try range = c(1, 8) instead of c(2, 12)
  scale_size_continuous(name = "REI %", range = c(1, 8)) +
  
  # 2. Shrink the Station Points (Black/Red dots)
  # Update the geom_point layer directly if possible, or override here:
  # (Note: Overriding a specific layer size in a saved object is tricky, 
  # so it's best to edit the main plot code block with these smaller numbers)
  theme(axis.text = element_text(size = 8), 
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# 2. Save again with the adjusted sizes
ggsave("Figure_REI_Map_Final_V2.tiff", 
       plot = p1_rei_export, 
       width = 7.5, height = 5.0, units = "in", dpi = 300, compression = "lzw")



# 1. Update the map with specific legend column counts
p1_rei_full_page <- p1_rei_final +
  guides(
    # Force Habitat to 3 columns
    fill = guide_legend(ncol = 3, byrow = TRUE, order = 1),
    # Force REI % to 1 column
    size = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    # Ensure Species richness remains a standard colorbar
    color = guide_colorbar(order = 3)
    ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    # Standardize text sizes for full page
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    # Adjust margins to fill the page better
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# 2. Export as Full-Page (8.5 x 11 inches)
# TIFF for final publication
ggsave("Figure_REI_FullPage.tiff", 
       plot = p1_rei_full_page, 
       width = 8.5, 
       height = 9, 
       units = "in", 
       dpi = 300, 
       compression = "lzw")

# PNG for easy viewing
ggsave("Figure_REI_FullPage.png", 
       plot = p1_rei_full_page, 
       width = 8.5, 
       height = 9, 
       units = "in", 
       dpi = 300)

