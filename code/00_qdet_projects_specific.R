## -- SABMPA - Project detections by individual project


library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)

# 1. Load high-resolution regional data
# 'scale = 50' or '10' for more detail in the Northwest Atlantic
land_reg <- ne_countries(scale = 10, country = "Canada", returnclass = "sf")

# 2. Filter detections for project 'OTN.TAG'
otn_tag_dets <- SAB_glatos_FINAL %>%
  filter(Project_OTN == "OTN.TAG") %>%
  group_by(station, animal_id) %>%
  summarise(lat = mean(deploy_lat), lon = mean(deploy_long), .groups = "drop")

# 3. Define Northwest Atlantic Extent (approximate for NS/NL/Gulf region)
nw_atlantic_extent <- list(xmin = -65, xmax = -55, ymin = 43, ymax = 49)

# 4. Generate the Map
ggplot() +
  # Layer 1: Landmass
  geom_sf(data = land_reg, fill = "gray90", color = "gray70") +
  
  # Layer 2: MPA Boundary (Bold and shaded)
  geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", linewidth = 1, alpha = 0.3) +
  
  # Layer 3: Receiver Line (All stations in gray)
  geom_point(data = SAB_glatos_FINAL %>% distinct(station, deploy_long, deploy_lat),
             aes(x = deploy_long, y = deploy_lat), shape = 3, color = "gray60", size = 1) +
  
  # Layer 4: OTN.TAG Project Detections (In Green)
  geom_point(data = otn_tag_dets, 
             aes(x = lon, y = lat), 
             color = "green", size = 2, alpha = 0.8) +
  
  # Spatial Elements
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  
  # Set the Northwest Atlantic regional view
  coord_sf(xlim = c(nw_atlantic_extent$xmin, nw_atlantic_extent$xmax), 
           ylim = c(nw_atlantic_extent$ymin, nw_atlantic_extent$ymax), 
           expand = FALSE) +
  
  theme_minimal() +
  labs(title = "Project OTN.TAG: Regional Detection Map (2026)",
       subtitle = "Green points indicate individuals detected within St. Anns Bank MPA",
       x = "Longitude", y = "Latitude")


# 1. Broaden the coordinates to capture more of the continent
# xmin: -95 (Central US/Canada) to -45 (Open Atlantic)
# ymin:  25 (Florida/Gulf) to  60 (Northern Labrador)
na_atlantic_extent <- list(xmin = -85, xmax = -45, ymin = 35, ymax = 55)

# 2. Re-run your map with these limits
ggplot() +
  geom_sf(data = land_reg, fill = "gray90", color = "gray70") +
  geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", linewidth = 1) +
  # Use the broader limits here
  coord_sf(xlim = c(na_atlantic_extent$xmin, na_atlantic_extent$xmax), 
           ylim = c(na_atlantic_extent$ymin, na_atlantic_extent$ymax), 
           expand = FALSE) +
  theme_minimal() +
  labs(title = "Broad Regional Context: OTN.TAG", x = NULL, y = NULL)



library(rnaturalearth)
library(sf)
library(dplyr)

# 1. Fetch land data for both Canada and the US
land_reg <- ne_countries(scale = 50, 
                         country = c("Canada", "United States of America"), 
                         returnclass = "sf")

# 2. Re-run your Northwest Atlantic Map
# We will use broader limits to show more of the US coastline
na_atlantic_extent <- list(xmin = -78, xmax = -50, ymin = 40, ymax = 50)

ggplot() +
  # Layer 1: Landmass (Now includes US and Canada)
  geom_sf(data = land_reg, fill = "gray90", color = "gray70") +
  
  # Layer 2: MPA Boundary (Bold and shaded)
  geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", linewidth = 1, alpha = 0.3) +
  
  # Layer 3: Receiver Line (All stations in gray)
  geom_point(data = SAB_glatos_FINAL %>% distinct(station, deploy_long, deploy_lat),
             aes(x = deploy_long, y = deploy_lat), shape = 3, color = "gray60", size = 1) +
  
  # Layer 4: OTN.TAG Project Detections (In Green)
  geom_point(data = otn_tag_dets, 
             aes(x = lon, y = lat), 
             color = "green", size = 2, alpha = 0.8) +
  
  # Set the broad regional view
  coord_sf(xlim = c(na_atlantic_extent$xmin, na_atlantic_extent$xmax), 
           ylim = c(na_atlantic_extent$ymin, na_atlantic_extent$ymax), 
           expand = FALSE) +
  
  theme_minimal() +
  labs(title = "Regional Context: OTN.TAG",
       subtitle = "Green points indicate project individuals detected in SAB MPA",
       x = "Longitude", y = "Latitude")


library(ggplot2)
library(sf)
library(ggrepel) # For better label placement
library(rnaturalearth)

# 1. Fetch land data for Canada and the US
land_reg <- ne_countries(scale = 50, 
                         country = c("Canada", "United States of America"), 
                         returnclass = "sf")

# 2. Create a dataframe for the Country Labels
# We pick central coordinates for the visible portion of the map
country_labels <- data.frame(
  name = c("CANADA", "USA"),
  lat = c(50, 40),
  lon = c(-55, -78)
)

#Create a "dissolved" version of the MPA just for the label
sab_combined_label <- st_union(sab_mpa) %>% st_as_sf()

sab_label_fix <- st_union(sab_mpa) %>% 
  st_as_sf() %>% 
  st_set_geometry("geometry") # Forces the name to 'geometry'


# 3. Create the Map
ggplot() +
  # Layer 1: Landmass
  geom_sf(data = land_reg, fill = "gray92", color = "gray75") +
  
  # Layer 4: MPA Boundary (Bold and shaded)
  geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", linewidth = 1.2, alpha = 0.4) +
  
  # Layer 5: MPA Label (With a line pointing to the bank)
  # We use the centroid of the MPA polygon for the anchor point
  geom_label_repel(data = sab_label_fix, 
                   aes(label = "St. Anns Bank MPA", geometry = geometry),
                   stat = "sf_coordinates",
                   nudge_x = 3, nudge_y = 2, # Push label into the Atlantic
                   size = 3.5, fontface = "bold", fill = "white", alpha = 0.8,
                   arrow = arrow(length = unit(0.02, "npc")),
                   segment.color = "darkblue") +
  
  # Layer 6: OTN.TAG Project Detections (Green)
  geom_point(data = otn_tag_dets, aes(x = lon, y = lat), 
             color = "green", size = 2, alpha = 0.8) +
  
  # View Settings (2026 Broad Northwest Atlantic View)
  coord_sf(xlim = c(-75, -55), ylim = c(40, 50), expand = FALSE) +
  
  # Layer 2: Country Labels (Subtle gray text)
  geom_text(data = country_labels, aes(x = lon, y = lat, label = name), 
            color = "gray50", fontface = "bold", size = 3) +
  
  theme_minimal() +
  labs(title = "Regional View: Project OTN.TAG",
       subtitle = "St. Anns Bank MPA project detections",
       x = NULL, y = NULL)



# 2. Create a dataframe for the Country Labels
# These coordinates are chosen to fit within the -75 to -55 view
country_labels <- data.frame(
  name = c("CANADA", "USA"),
  lat = c(48.5, 43.5), # Moved Canada south slightly, USA north to be visible
  lon = c(-68, -72)    # Moved Canada west, USA east to be inside the -75/-55 box
)

# 3. Create the Map (keeping your other settings)
ggplot() +
  # Layer 1: Landmass
  geom_sf(data = land_reg, fill = "gray92", color = "gray75") +
  
  # Layer 4: MPA Boundary
  geom_sf(data = sab_mpa, fill = "lightblue", color = "darkblue", linewidth = 1.2, alpha = 0.4) +
  
  # Layer 5: MPA Label
  geom_label_repel(data = sab_label_fix, 
                   aes(label = "St. Anns Bank MPA", geometry = geometry),
                   stat = "sf_coordinates",
                   nudge_x = 4, nudge_y = 2, # Pushed a bit further East
                   size = 3.5, fontface = "bold", fill = "white", alpha = 0.8,
                   arrow = arrow(length = unit(0.02, "npc")),
                   segment.color = "darkblue") +
  
  # Layer 6: OTN.TAG Project Detections
  geom_point(data = otn_tag_dets, aes(x = lon, y = lat), 
             color = "green", size = 2, alpha = 0.8) +
  
  # View Settings (YOUR CURRENT VIEW)
  coord_sf(xlim = c(-75, -55), ylim = c(40, 50), expand = FALSE) +
  
  # Layer 2: Country Labels (NOW INSIDE THE VIEW)
  geom_text(data = country_labels, aes(x = lon, y = lat, label = name), 
            color = "gray50", fontface = "bold", size = 4) +
  
  theme_minimal() +
  labs(title = "Regional View: Project OTN.TAG",
       subtitle = "St. Anns Bank MPA project detections",
       x = "Longitude", y = "Latitude")

### ---------------------------------------------------



