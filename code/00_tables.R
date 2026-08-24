## SAB CJFAS final summary tables
# 6 March 2026

# bring in columns I need from detections file to receivers

colnames(SAB_glatos_FINAL)
colnames(SAB_deploys_final)

library(dplyr)

unique(SAB_deploys_final$station_name)
unique(SAB_deploys_final$station_renamed)

unique(SAB_deploys_final$Zone)
unique(SAB_deploys_final$Assigned_c)
unique(SAB_deploys_final$depth)
unique(SAB_deploys_final$mean_depth)

# 1. Rename columns in the deployment file as requested
SAB_deploys_final <- SAB_deploys_final %>%
  rename(station_old = station_name, 
         station = station_renamed)

# 2. Check for Station Mismatches
# Find stations in detections that are NOT in the deployment metadata
mismatched_stations <- setdiff(unique(SAB_glatos_FINAL$station), 
                               unique(SAB_deploys_final$station))

if(length(mismatched_stations) > 0) {
  message("Warning: The following stations in detections do not exist in metadata:")
  print(mismatched_stations)
} else {
  message("Success: All detection stations matched the deployment metadata.")
}

# 1. Identify rows with missing metadata in your deployment file
na_metadata_audit <- SAB_deploys_final %>%
  filter(is.na(Zone) | is.na(depth) | is.na(Assigned_c) | is.na(station)) %>%
  select(station, Zone, mean_depth, depth, Assigned_c, stn_notes)

library(dplyr)

# 2. Group by your 'renamed' station and count unique metadata values
consistency_check <- SAB_deploys_final %>%
  group_by(station) %>%
  summarise(
    unique_zones = n_distinct(Zone, na.rm = TRUE),
    unique_depths = n_distinct(depth, na.rm = TRUE),
    unique_habitats = n_distinct(Assigned_c, na.rm = TRUE),
    # Capture the actual values to see the conflict
    habitat_list = paste(unique(Assigned_c), collapse = " | "),
    depth_range = max(depth, na.rm = TRUE) - min(depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 2. Filter for any station that has more than 1 unique value
  filter(unique_zones > 1 | unique_depths > 1 | unique_habitats > 1)

# 3. Report findings
if(nrow(consistency_check) > 0) {
  message("Warning: Found ", nrow(consistency_check), " stations with inconsistent metadata:")
  print(consistency_check)
} else {
  message("Success: All metadata is 100% consistent across all deployments for each station.")
}

# Create the clean, one-row-per-station lookup table
station_lookup_clean <- SAB_deploys_final %>%
  group_by(station) %>%
  summarise(
    zone = first(Zone),
    mean_depth = mean(depth, na.rm = TRUE),
    Assigned_c = first(Assigned_c),
    .groups = "drop"
  )

station_lookup_clean

# some stations have two depths and two Assigned_c (because they were named based on clustered positions around <200m apart)

library(dplyr)
library(geosphere)

# 1. Calculate max distance between deployments within each 'Inconsistent' station
dist_check <- SAB_deploys_final %>%
  filter(station %in% consistency_check$station) %>%
  group_by(station) %>%
  summarise(
    n_deployments = n(),
    # Calculate the max distance between any two points in the group (meters)
    max_dist_m = max(distm(cbind(stn_long, stn_lat), fun = distHaversine)),
    habitats = paste(unique(Assigned_c), collapse = " & "),
    depth_diff = max(depth) - min(depth),
    .groups = "drop"
  )

print(dist_check)

library(leaflet)
library(sf)

# Prepare spatial data
# Transform to WGS84 for Leaflet
sab_wgs84 <- st_transform(sab, 4326)
bs_wgs84 <- st_transform(sab_bs_clipped, 4326)

# Define color palette for inconsistent vs consistent
SAB_deploys_final <- SAB_deploys_final %>%
  mutate(map_color = ifelse(station %in% consistency_check$station, "red", "blue"))

# Create the Map
map_html <- leaflet(SAB_deploys_final) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # 1. Add MPA Zones
  addPolygons(data = sab_wgs84, color = "black", weight = 2, fillOpacity = 0.1, group = "MPA Zones") %>%
  # 2. Add Benthoscape Habitat
  addPolygons(data = bs_wgs84, color = "gray", weight = 1, 
              fillColor = ~Assigned_c, fillOpacity = 0.4, 
              popup = ~Assigned_c, group = "Benthoscape") %>%
  # 3. Add All Stations (Blue)
  addCircleMarkers(lng = ~stn_long, lat = ~stn_lat, 
                   radius = 4, color = "blue", stroke = FALSE, fillOpacity = 0.6,
                   popup = ~paste("Station:", station, "<br>Depth:", depth),
                   group = "All Stations") %>%
  # 4. Highlight Inconsistent Stations (Red)
  addCircleMarkers(data = SAB_deploys_final %>% filter(station %in% consistency_check$station),
                   lng = ~stn_long, lat = ~stn_lat, 
                   radius = 6, color = "red", weight = 2, fillOpacity = 0.8,
                   popup = ~paste("<b>WARNING: INCONSISTENT</b><br>Station:", station, 
                                  "<br>Habitat:", Assigned_c, "<br>Depth:", depth),
                   group = "Inconsistent Only") %>%
  addLayersControl(overlayGroups = c("MPA Zones", "Benthoscape", "All Stations", "Inconsistent Only"),
                   options = layersControlOptions(collapsed = FALSE))

# Display the map
map_html


## 
library(dplyr)
library(geosphere)

# Calculate the max distance (metres) between any two points assigned to the same station
dist_check <- SAB_deploys_final %>%
  filter(station %in% consistency_check$station) %>%
  group_by(station) %>%
  summarise(
    max_dist_m = round(max(distm(cbind(stn_long, stn_lat), fun = distHaversine)), 1),
    depth_diff = max(depth) - min(depth),
    habitats = paste(unique(Assigned_c), collapse = " & "),
    .groups = "drop"
  )

print(dist_check)


library(leaflet)
library(sf)

library(leaflet)
library(sf)

# Ensure data is WGS84
sab_wgs84 <- st_transform(sab, 4326)
bs_wgs84 <- st_transform(sab_bs_clipped, 4326)

# Map with explicit leaflet:: prefixes
map_html <- leaflet(SAB_deploys_final) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # 1. MPA Zones
  addPolygons(data = sab_wgs84, color = "black", weight = 2, fillOpacity = 0.1, group = "MPA Zones") %>%
  # 2. Benthoscape Habitat
  addPolygons(data = bs_wgs84, color = "gray", weight = 0.5, 
              fillColor = ~Assigned_c, fillOpacity = 0.4, 
              popup = ~Assigned_c, group = "Benthoscape") %>%
  # 3. All Stations
  addCircleMarkers(lng = ~stn_long, lat = ~stn_lat, 
                   radius = 3, color = "blue", stroke = FALSE, fillOpacity = 0.5,
                   group = "All Stations") %>%
  # 4. Highlight Inconsistent Stations (Red)
  addCircleMarkers(data = SAB_deploys_final %>% filter(station %in% consistency_check$station),
                   lng = ~stn_long, lat = ~stn_lat, 
                   radius = 6, color = "red", weight = 2, fillOpacity = 0.8,
                   popup = ~paste("<b>STATION:</b>", station, 
                                  "<br><b>Habitat:</b>", Assigned_c, 
                                  "<br><b>Depth:</b>", depth, "m"),
                   group = "Inconsistent Only") %>%
  # Use explicit leaflet prefix for the control functions
  leaflet::addLayersControl(
    overlayGroups = c("MPA Zones", "Benthoscape", "All Stations", "Inconsistent Only"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

map_html




# install.packages("mapview")
library(mapview)
library(sf)

# 1. Prepare the spatial data
# Convert your deployment df to an sf object
deploys_sf <- SAB_deploys_final %>%
  st_as_sf(coords = c("stn_long", "stn_lat"), crs = 4326) %>%
  mutate(Inconsistent = ifelse(station %in% consistency_check$station, "Yes", "No"))

# 2. Create the map
# This will show Benthoscape, All Stations, and highlight the red ones
map_view <- mapview(sab_bs_clipped, zcol = "Assigned_c", layer.name = "Habitat", alpha.regions = 0.3) +
  mapview(deploys_sf, zcol = "Inconsistent", 
          col.regions = list("blue", "red"), 
          layer.name = "Inconsistent Stations",
          cex = 4) +
  mapview(sab, color = "black", lwd = 2, fill = FALSE, layer.name = "MPA Zones")

map_view


library(ggplot2)
library(sf)

library(ggplot2)
library(sf)

# 1. Convert everything to a simple dataframe with X/Y for the faceted zoom
inconsistent_pts_df <- SAB_deploys_final %>%
  filter(station %in% consistency_check$station) %>%
  mutate(lon = stn_long, lat = stn_lat)

# 2. Map it with free scales
ggplot() +
  # Use the clipped habitat but as a standard geom_sf (it will adapt to the facet limits)
  geom_sf(data = sab_bs_clipped, aes(fill = Assigned_c), color = "white", size = 0.1, alpha = 0.3) +
  # Plot the deployment points using standard X/Y to allow 'free' scales
  geom_point(data = inconsistent_pts_df, aes(x = lon, y = lat), color = "red", size = 2) +
  # Facet by station to see each 'cluster' up close
  facet_wrap(~station, scales = "free") +
  theme_bw() +
  scale_fill_manual(values = habitat_cols, name = "Habitat") +
  labs(title = "Audit: Inconsistent Station Clusters",
       subtitle = "Red dots = individual deployments; Faceted zoom shows habitat boundaries",
       x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 7))
