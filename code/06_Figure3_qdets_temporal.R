# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 06_Figure3_qdets_temporal.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-10-17
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description:  Generates summaries for qualified detections monthly days detected
#               and unique individuals detected by species and Figure3 temporal
#               combined plots.
# ===================================================================
# ---------------------------------------------------------
# Required packages (run 00_setup.R). 
# ---------------------------------------------------------
# ---------------------------------------------------------
# 0. Load project configuration and packages
source("code/00_setup.R")
# ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)
library(cowplot)

# ---------------------------------------------------------
## Load the cleaned, filtered and processed data
# ---------------------------------------------------------
# Qualified detections with species metadata
SAB_glatos_Q25_FINAL <- readRDS("data/processed/SAB_glatos_Q25_FINAL.rds")
# make sure detection data is the correct format for this temporal plot
SAB_glatos_Q25_FINAL$detection_timestamp_utc <- as.POSIXct(SAB_glatos_Q25_FINAL$detection_timestamp_utc, tz = "UTC") 

## --- load palette (14 species, vibrant and diverse colours)
my_palette <- readRDS("data/processed/my_palette.rds")

# ---------------------------------------------------------
# 1. Prepare monthly days detected and unique individuals using animal_id
# ---------------------------------------------------------
F3_plot_data_all_Q <- SAB_glatos_Q25_FINAL %>%
  mutate(
    day = as.Date(detection_timestamp_utc),
    month_bin = floor_date(detection_timestamp_utc, unit = "month"),
    # Month as a factor for correct clock placement
    month_name = lubridate::month(detection_timestamp_utc, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(common_name, month_bin, month_name) %>%
  summarise(
    # 'animal_id' accounts for multiple sensors on one individual
    monthly_days_detected = n_distinct(day, animal_id),
    unique_individuals = n_distinct(animal_id),
    .groups = "drop"
  )

# Set axis limits
F3_start_date <- min(F3_plot_data_all_Q$month_bin) %m-% months(1)
F3_end_date   <- max(F3_plot_data_all_Q$month_bin) %m+% months(1)
F3_years      <- year(F3_start_date):year(F3_end_date)
F3_shading_winter <- data.frame(xmin = as.POSIXct(paste0(F3_years - 1, "-12-01")),
                             xmax = as.POSIXct(paste0(F3_years, "-03-31")))
# ---------------------------------------------------------
# 2. Define Plots: p1: Top Timeline (Days Detected) 
# ---------------------------------------------------------
# We enable the legend ONLY on p1 so patchwork() has a source to collect
F3_p1 <- ggplot() +
  geom_rect(data = F3_shading_winter, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.5) +
  geom_vline(xintercept = as.POSIXct(paste0(F3_years, "-01-01")), 
             linetype = "dotted", color = "gray40", linewidth = 0.5) +
  geom_col(data = F3_plot_data_all_Q, aes(x = month_bin, y = monthly_days_detected, fill = common_name), 
           position = "stack", width = 2500000, just = 0, color = "black", linewidth = 0.05) +
  scale_fill_manual(values = my_palette) +
  scale_x_datetime(limits = c(F3_start_date, F3_end_date), expand = c(0,0)) +
  labs(x = NULL, y = "Detection Days") +
  theme_classic() +
  theme(legend.position = "none", # Needed for collection #bottom
        axis.text.x = element_blank(), # Hide dates on top plot
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 5)))
# ---------------------------------------------------------
# 3. Define Plots: p2: Middle Timeline (Unique Individuals)
# ---------------------------------------------------------
F3_p2 <- ggplot() +
  geom_rect(data = F3_shading_winter, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.5) +
  geom_vline(xintercept = as.POSIXct(paste0(F3_years, "-01-01")), 
             linetype = "dotted", color = "gray40", linewidth = 0.5) +
  geom_col(data = F3_plot_data_all_Q, aes(x = month_bin, y = unique_individuals, fill = common_name), 
           position = "stack", width = 2500000, just = 0, color = "black", linewidth = 0.05) +
  scale_fill_manual(values = my_palette) +
  scale_x_datetime(limits = c(F3_start_date, F3_end_date), date_breaks = "6 months", 
                   date_labels = "%b %Y", expand = c(0,0)) +
  labs(x = NULL, y = "Unique Individuals") +
  theme_classic() +
  theme(legend.position = "none", # Keep individual legends off
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 5)))
# ---------------------------------------------------------
# 4. Polar Clocks: summarize monthly data
# ---------------------------------------------------------
F3_polar_sum <- F3_plot_data_all_Q %>%
  group_by(common_name, month_name) %>%
  summarise(total_days = sum(monthly_days_detected),
            total_indiv = sum(unique_individuals), .groups = "drop")
# ---------------------------------------------------------
# 5. Polar Clocks: p3: Individuals Clock
# ---------------------------------------------------------
F3_p3 <- ggplot(F3_polar_sum, aes(x = as.numeric(month_name), y = total_indiv, fill = common_name)) +
  geom_col(position = "stack", color = "black", linewidth = 0.1, width = 1) + 
  coord_polar(start = 0) + 
  scale_fill_manual(values = my_palette) +
  # Convert to continuous axis: starts at 0.5 (12 o'clock), labels at 1:12
  scale_x_continuous(limits = c(0.5, 12.5), 
                     breaks = 1:12, 
                     labels = levels(F3_polar_sum$month_name),
                     minor_breaks = seq(0.5, 12.5, by = 1)) +
  labs(title = "Monthly Individuals", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"), 
        # Turn OFF grid lines in the middle of the bars
        panel.grid.major.x = element_blank(), 
        # Turn ON grid lines at the edges of the bars
        panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.5), 
        legend.position = "none")
# ---------------------------------------------------------
# 6. Polar Clocks: p4: Days Detected Clock
# ---------------------------------------------------------
F3_p4 <- ggplot(F3_polar_sum, aes(x = as.numeric(month_name), y = total_days, fill = common_name)) +
  geom_col(position = "stack", color = "black", linewidth = 0.1, width = 1) + 
  coord_polar(start = 0) + 
  scale_fill_manual(values = my_palette) +
  scale_x_continuous(limits = c(0.5, 12.5), 
                     breaks = 1:12, 
                     labels = levels(F3_polar_sum$month_name),
                     minor_breaks = seq(0.5, 12.5, by = 1)) +
  labs(title = "Monthly Detection Days", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.5), 
        legend.position = "none")

# ---------------------------------------------------------
# 7. Final Layout: Extract the legend as a standalone object (Keep existing code)
# ---------------------------------------------------------
F3_shared_legend <- get_legend(
  ggplot(F3_polar_sum, aes(x = month_name, y = total_days, fill = common_name)) +
    geom_col() +
    scale_fill_manual(values = my_palette) +
    theme_classic() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size = 12)) + 
    guides(fill = guide_legend(ncol = 2)) # Keep 2 columns
)
# ---------------------------------------------------------
# 8. Final Layout: Define a perfectly rectangular 9-character grid
# ---------------------------------------------------------
# C = Left Clock (3/9), L = Legend (3/9), E = Right Clock (3/9)
design <- "
  AAAAAAAAA
  BBBBBBBBB
  CCCLLLEEE
"
# ---------------------------------------------------------
# 9. Final Layout: 3. Assemble the grid
# ---------------------------------------------------------
F3_final_layout <- wrap_plots(
  A = F3_p1 + theme(legend.position = "none"), 
  B = F3_p2 + theme(legend.position = "none"), 
  C = F3_p3 + theme(legend.position = "none"), 
  L = F3_shared_legend, 
  E = F3_p4 + theme(legend.position = "none"), 
  design = design
) + 
  plot_layout(heights = c(1, 1, 1.5))

plot(F3_final_layout)

# ---------------------------------------------------------
# save the plot
# ---------------------------------------------------------
ggsave("output/Figure3_qdet_temporal.png", # save as .tiff 300 dip for high quality
       plot = F3_final_layout, 
       width = 23.9,
       height = 18.2,
       units = "cm", 
       dpi = 150)
