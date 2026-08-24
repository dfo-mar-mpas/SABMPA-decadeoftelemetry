### SAB Matched Residence Index



library(ggplot2)

# Assuming your plot object is named 'ri_plot'
# If you need to re-run the plot code:
p_ri_final <- ggplot(residency_data, aes(x = station, y = residency_index, fill = common_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~common_name, scales = "free_y") + # standard RI facetting
  theme_bw(base_size = 12) + # larger base font for legibility
  theme(
    text = element_text(family = "Arial"), # standard sans-serif font
    strip.text = element_text(face = "bold", size = 10), # bold facet labels
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines") # increase space between facets for clarity
  ) +
  labs(x = "Station", y = "Residency Index", fill = "Species")

# View the plot in RStudio
print(p_ri_final)

plot(Restimeplot_lat)

library(patchwork)
library(ggplot2)

# 1. Combine the plots: Top (Lat) over Bottom (Long)
# 'guides = "collect"' moves the legend to the side to avoid duplication
combined_res_plot <- (Restimeplot_lat / Restimeplot_lon) + 
  plot_layout(guides = "collect") & 
  theme_bw(base_size = 12) & # Standardize font size for both
  theme(text = element_text(family = "Arial")) # Clean sans-serif for publication

# 2. View the combined result in R
print(combined_res_plot)


# Save as a high-res TIFF (600 DPI)
ggsave("Figure_Residency_Time_LatLong_Stack.tiff", 
       plot = combined_res_plot, 
       width = 8.5,      # Standard page width (inches)
       height = 11,      # Standard page height (inches)
       units = "in", 
       dpi = 600,        # High resolution for print
       compression = "lzw")

# Also save as a PDF for infinite scalability (best for reviewers)
ggsave("Figure_Residency_Time_LatLong_Stack.pdf", 
       plot = combined_res_plot, 
       width = 210,      # A4 Width (mm)
       height = 297,     # A4 Height (mm)
       units = "mm")




# ----------------



library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Load Data
# (Ensure your working directory is set to where the CSV is located)
residency_data <- read.csv("all_ri_data_SAB.csv")

# 2. Top Plot: Latitude
Restimeplot_lat <- ggplot(residency_data, aes(x = mean_latitude, y = residency_index)) +
  # Draw thin vertical lines connecting points at the exact same location
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  # Add points over the lines, colored dynamically by individual animal
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  # Free X scales allow the axes to fit each species' specific range
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Latitude", y = "Residency Index (RI)")

# 3. Bottom Plot: Longitude
Restimeplot_lon <- ggplot(residency_data, aes(x = mean_longitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Longitude", y = "Residency Index (RI)")

# 4. Combine and Apply Shared Theme
# The '&' operator from patchwork applies these theme settings to BOTH plots simultaneously
combined_res_plot <- (Restimeplot_lat / Restimeplot_lon) & 
  theme_bw(base_size = 12) &
  theme(
    text = element_text(family = "sans"), # Standard sans-serif font
    strip.text = element_text(face = "italic", size = 10), # Matches the italics in the image
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position = "none", # Hides the massive animal_id legend
    panel.grid.minor = element_blank() # Cleans up the background slightly
  )

# Display the final combined plot
print(combined_res_plot)

## prepping for publication

library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Load Data
residency_data <- read.csv("all_ri_data_SAB.csv")

# 2. Define Shared Theme for Publication
# Defining this as an object keeps the code clean and ensures consistency
pub_theme <- theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    strip.text = element_text(face = "italic", size = 10),
    # Added linewidth = 0.5 to ensure the facet border is crisp
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# 3. Top Plot: Latitude (Keeps the facet strips)
Restimeplot_lat <- ggplot(residency_data, aes(x = mean_latitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Latitude", y = "Residency Index (RI)") +
  pub_theme

# 4. Bottom Plot: Longitude (Removes the facet strips)
Restimeplot_lon <- ggplot(residency_data, aes(x = mean_longitude, y = residency_index)) +
  geom_line(aes(group = location), color = "black", linewidth = 0.3) +
  geom_point(aes(fill = animal_id), shape = 21, color = "black", size = 2, alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Longitude", y = "Residency Index (RI)") +
  pub_theme +
  # Add overrides specifically for the bottom plot
  theme(
    strip.background = element_blank(), # Removes the white box
    strip.text.x = element_blank()      # Removes the text
  )

# 5. Combine Plots
combined_res_plot <- Restimeplot_lat / Restimeplot_lon

# Display in R
print(combined_res_plot)

# 6. High-Quality Export
# TIFF is the standard requirement for most scientific journals. 
# 600 DPI ensures extremely crisp lines and text.
ggsave("Figure_Residency_Index_HighRes_lzw.tiff", 
       plot = combined_res_plot, 
       width = 10,             # Adjust width based on journal column requirements
       height = 7,             # Adjust height as needed
       units = "in", 
       dpi = 600,              # Minimum 300 required by journals, 600 is safer for line art/points
       compression = "lzw")    # Compresses the TIFF so the file size isn't massive


### ---------------------

# Option 1: 2D Spatial Map
map_plot <- ggplot(residency_data, aes(x = mean_longitude, y = mean_latitude)) +
  # Use a slight jitter so individuals at the same station don't perfectly overlap
  geom_point(aes(fill = residency_index, size = residency_index), 
             shape = 21, color = "black", alpha = 0.7,
             position = position_jitter(width = 0.02, height = 0.02)) +
  # Use a colorblind-friendly gradient (viridis) to show RI intensity
  scale_fill_viridis_c(name = "Residency\nIndex", option = "plasma") +
  scale_size_continuous(name = "Residency\nIndex", range = c(1, 5)) +
  facet_wrap(~species, scales = "free") +
  # coord_quickmap() ensures the aspect ratio of Lat/Lon is geographically accurate
  coord_quickmap() + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    # Bring the legend back to show the RI scale
    legend.position = "right" 
  )

print(map_plot)



# Option 2: Latitude Boxplots
# Note: Requires converting 'location' to a factor so it plots as discrete groups along the continuous X-axis
lat_boxplot <- ggplot(residency_data, aes(x = mean_latitude, y = residency_index, group = location)) +
  geom_boxplot(fill = "gray80", outlier.shape = 21, outlier.fill = "gray50", alpha = 0.7) +
  facet_wrap(~species, scales = "free_x") +
  labs(x = "Mean Latitude", y = "Residency Index (RI)") +
  pub_theme # (Using the pub_theme we defined earlier)

print(lat_boxplot)
