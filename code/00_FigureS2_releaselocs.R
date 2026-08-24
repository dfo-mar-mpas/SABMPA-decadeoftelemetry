## SABMPA - release location maps

library(dplyr)
library(ggplot2)
library(sf)

unique(tagging_map_all$Common.Name)

# 1. Filter for all valid tagging locations inside the MPA
# We use the species name for coloring
tagging_map_all <- fish_SABtagged_clean_updated %>%
  filter(!is.na(RELEASE_LATITUDE) & !is.na(RELEASE_LONGITUDE))

# 2. Filter specifically for the 2025 cohort
tagging_map_2025 <- tagging_map_all %>%
  filter(YEAR == 2025)

colnames(tagging_map_2025)

min(tagging_map_2025$UTC_RELEASE_DATE_TIME)
max(tagging_map_2025$UTC_RELEASE_DATE_TIME)

tagging_map_corefish <- fish_SABtagged_clean_updated %>%
  filter(!Common.Name == "Thorny skate") %>%
  filter(!Common.Name =="Atlantic tomcod") %>%
  filter(!Common.Name =="Shorthorn sculpin")


receiver_background <- SAB_glatos_FINAL %>%
  distinct(station, deploy_long, deploy_lat) # can get zone, yearcollected

# 2. Filter specifically for the 2025 cohort
SAB_deploys_final_NEWonly <- SAB_deploys_final %>%
  filter(OTN_ARRAY_2 == "SAB_NEW")

receiver_background <- SAB_deploys_final_NEWonly %>%
  distinct(station, stn_long, stn_lat)

colnames(receiver_background)
colnames(SAB_glatos_FINAL)
colnames(SAB_deploys_)
colnames(SAB_deploys_final)
unique(SAB_deploys_final$OTN_ARRAY_2)

receiver_background <- SAB_glatos_FINAL %>%
  distinct(station, deploy_long, deploy_lat)

library(patchwork)

# --- MAP A: All Historical Tagging ---
p_all <- ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = z)) +
  #scale_fill_etopo(name = "Depth (m)", guide = "none") + # Hide depth legend for clarity
  #geom_sf(data = land_hi, fill = "gray80", color = "gray40") +
  geom_sf(data = sab, fill = NA, color = "darkblue", linewidth = 1.5) +
  
  # receivers
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat),
             shape = 20, color = "black", size = 3) +
  
  # Plot release points colored by species
  geom_point(data = tagging_map_all, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2, alpha = 0.7) +
  
  #scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  theme_bw() +
  labs(title = NULL, x = NULL, y = NULL, color = "Species")

p_all

print(sabmatch_release)


# --- MAP B: 2025 Tagging Only ---
p_2025 <- ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_etopo(name = "Depth (m)", guide = "none") +
  #geom_sf(data = land_hi, fill = NA, color = "gray40") +
  geom_sf(data = sab, fill = NA, color = "darkblue", linewidth = 1.5) +
  
  # receivers
  geom_point(data = receiver_background,
             aes(x = stn_long, y = stn_lat),
             shape = 1, color = "darkgray", size = 1.2, alpha = 0.6) +
  
  # Plot 2025 points specifically in Green (or by species)
  geom_point(data = tagging_map_2025, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2.5, alpha = 0.9) +
  
  #scale_color_viridis_d(name = "Species", option = "viridis") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  theme_bw() +
  labs(title = "St. Anns Bank MPA Tagging Releases - April 2025", x = NULL, y = NULL)

p_2025
# Combine into a single figure
tagging_figure <- p_all + p_2025 + plot_layout(guides = "collect")
tagging_figure



library(ggplot2)
library(dplyr)
library(sf)
library(marmap)

# 1. Background Receiver Array (All stations, light gray + markers)
# We use the 'SAB_glatos_FINAL' dataset to pull all known coordinates
receiver_background <- SAB_glatos_FINAL %>%
  distinct(station, Zone, deploy_long, deploy_lat)


p_historical_all <- ggplot() +
  # Detailed Bathymetry & Landmass
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_etopo(name = "Depth (m)") +
  geom_sf(data = land_hi, fill = "gray80", color = "gray40") +
  
  # MPA Boundary & Zone Labels
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1.2) +
  geom_sf_label(data = sab_label_data, aes(label = Zone_Num), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  
  # THE RECEIVER LINE (Light gray '+' markers)
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat), 
             shape = 3, color = "gray75", size = 1.2, alpha = 0.6) +
  
  # ALL TAGGING RELEASES
  geom_point(data = fish_SABtagged_clean_updated, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2, alpha = 0.7) +
  
  scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  theme_bw() +
  labs(title = "All Historical Tagging Releases (2014-2024)", 
       subtitle = "Receiver array positions shown in light gray (+)",
       x = "Longitude", y = "Latitude")

print(p_historical_all)



p_2015_tagging <- ggplot() +
  # MPA Border & Zone Labels
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1.2) +
  geom_sf_label(data = sab_label_data, aes(label = Zone_Num), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  
  # THE RECEIVER LINE (Light gray)
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat), 
             #shape = 3, 
             color = "gray75", size = 1.2, alpha = 0.6) +
  
  # 2015 RELEASES ONLY
  geom_point(data = fish_SABtagged_clean_updated %>% filter(YEAR == 2015), 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2.5, alpha = 0.9) +
  
  scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  theme_bw() +
  labs(title = "2015 Tagging Releases", 
       subtitle = "St. Anns Bank MPA",
       x = "Longitude", y = "Latitude")

print(p_2015_tagging)


### --------------------- FINAL matched Release locs map ------------- ##



p_match_rel <- ggplot() +
  # Detailed Bathymetry & Landmass
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_etopo(name = "Depth (m)") +
  geom_sf(data = land_hi, fill = "gray80", color = "gray40") +
  
  # MPA Boundary & Zone Labels
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1.2) +
  geom_sf_label(data = sab_label_data, aes(label = Zone_Num), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  
  # THE RECEIVER LINE (Light gray '+' markers)
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat), 
             shape = 3, color = "gray75", size = 1.2, alpha = 0.6) +
  
  # ALL TAGGING RELEASES
  geom_point(data = fish_SABtagged_clean_updated, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2, alpha = 0.7) +
  
  scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  theme_bw() +
  labs(title = "All Historical Tagging Releases (2014-2024)", 
       subtitle = "Receiver array positions shown in light gray (+)",
       x = "Longitude", y = "Latitude")

print(p_match_rel)


p_match_rel <- ggplot() +
  geom_sf(data = basemap) +
  
  # THE RECEIVER LINE (Light gray '+' markers)
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat), 
             shape = 20,color = "red", size = 2, alpha = 0.6) +
  
  # ALL TAGGING RELEASES
  geom_point(data = fish_SABtagged_clean_updated, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2, alpha = 0.7) +
  
  
  scale_color_viridis_d(name = "Species", option = "turbo") +
  coord_sf(xlim = c(extent["xmin"], extent["xmax"]), 
           ylim = c(extent["ymin"], extent["ymax"]), expand = FALSE) +
  
  # Bold MPA Border (Plot this last to sit on top)
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = sab_wgs84, fill = NA, color = "black", linewidth = 1) +
  geom_sf_label(data = sab_label_data, aes(label = Id), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  geom_sf(data=shelfbreak,fill=NA,lwd=0.2)+
  coord_sf(xlim = c(-59.75, -58.25), ylim = c(45.7, 46.55), expand = FALSE) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9,0.22),
    legend.background = element_rect(color="black")
  )+
  labs(x = "Longitude ",
       y = "Latitude ")

p_match_rel


### ----- FINAL release locs map for CJFAS paper supplemental -------
library(ggplot2)
library(sf)
library(smoothr) 

# 1. Safely smooth the line using the 'chaikin' algorithm
shelfbreak_smooth <- smoothr::smooth(shelfbreak, method = "chaikin")

# 2. Crop the smoothed line to your exact plot boundaries
plot_box <- st_bbox(c(xmin = -59.75, xmax = -58.25, ymin = 45.7, ymax = 46.55), 
                    crs = st_crs(shelfbreak))
shelfbreak_cropped <- st_crop(shelfbreak_smooth, plot_box)

# 3. Generate the Plot
p_match_rel <- ggplot() +
  geom_sf(data = basemap) +
  
  # THE RECEIVER LINE 
  geom_point(data = receiver_background,
             aes(x = deploy_long, y = deploy_lat), 
             shape = 20, color = "red", size = 3.5, alpha = 0.6) +
  
  # ALL TAGGING RELEASES
  geom_point(data = fish_SABtagged_clean_updated, 
             aes(x = RELEASE_LONGITUDE, y = RELEASE_LATITUDE, color = Common.Name), 
             size = 2, alpha = 0.7) +
  
  # Bold MPA Borders
  geom_sf(data = sab, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = sab_wgs84, fill = NA, color = "black", linewidth = 1) +
  
  # The Smoothed & Cropped Depth Contour
  # Removed aes() mapping so it draws the line but ignores the legend entirely
  geom_sf(data = shelfbreak_cropped, fill = NA, color = "gray40", 
          linewidth = 0.5, linetype = "solid") +
  
  # Zone Labels
  geom_sf_label(data = sab_label_data, aes(label = Id), 
                size = 3.5, fontface = "bold", alpha = 0.8) +
  
  # Species Color Scale
  scale_color_viridis_d(name = "Species", option = "turbo") +
  
  # Coordinate Limits
  coord_sf(xlim = c(-59.75, -58.25), ylim = c(45.7, 46.55), expand = FALSE) +
  
  theme_bw(base_size = 12) +
  theme(
    # Moved legend to the right and removed the bounding box
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank(), # Removes default gray background from legend keys
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  labs(x = "Longitude", y = "Latitude")

print(p_match_rel)

# 4. Export
ggsave("Figure_Release_Locs_HighRes.tiff", 
       plot = p_match_rel, 
       width = 6.5,             
       height = 4.5,             
       units = "in", 
       dpi = 300,              
       compression = "none")


## -----------alternative simple map from activity app code ----- not used
p_match_rel_simple <- ggplot()+
  geom_sf(data=shelfbreak,fill=NA,lwd=0.2)+
  geom_sf(data=basemap)+
  geom_sf(data=sab_zones,fill="cornflowerblue",alpha=0.30,col="grey10")+
  #geom_sf(data=rbind(surveysets%>%filter(type=="2022 tag locations"),reciever_locs),aes(fill=type),col="black",pch=21,size=2)+
  geom_sf(data=locs_matched,aes(fill=type),col="black",pch=21,size=1.2)+
  geom_sf(data=surveysets%>%filter(type=="2022 tag locations"),aes(fill=type),col="black",pch=21,size=2)+
  scale_fill_manual(values=c("grey80","red"))+
  coord_sf(xlim=plot_boundaries[c(1,2)],ylim=plot_boundaries[c(3,4)],expand=0)+
  labs(fill="")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="bottom")

print(p_match_rel_simple)
# ------------------------