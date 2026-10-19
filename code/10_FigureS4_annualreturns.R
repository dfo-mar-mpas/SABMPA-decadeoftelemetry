# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 08_FigureS4_annualreturns.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-09-23
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Calculate annually returning individuals from qualified 
#              detections and visualize in an abacus plot (Figure S4).
# ===================================================================
# ---------------------------------------------------------
# 0. Setup & Packages
# ---------------------------------------------------------
source("code/00_setup.R")

library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(glatos)

## --- Load the data
SAB_glatos_Q25_FINAL <- readRDS("data/processed/SAB_glatos_Q25_FINAL.rds")

# ---------------------------------------------------------
# 1. Generate Detection Events and Retain Metadata
# ---------------------------------------------------------
# Note: Detection events are also generated in 03_glatos_summaries.R
# Split by species to run events if not already generated, 
# explicitly using purrr::map_df to avoid map.poly errors
events_all_Q <- SAB_glatos_Q25_FINAL %>%
  split(.$common_name) %>%
  purrr::map_df(~{
    species <- unique(.x$common_name)
    # Run the glatos event classification (12-hour gap = new event)
    ev <- detection_events(.x, time_sep = 43200)
    ev$common_name <- species
    return(ev)
  })

# ---------------------------------------------------------
# 2. Identify Multi-Year Returners
# ---------------------------------------------------------
# Filter for individuals detected in > 1 distinct calendar year
events_returning_Q <- events_all_Q %>%
  mutate(year = year(first_detection)) %>%
  group_by(animal_id) %>%
  filter(n_distinct(year) > 1) %>%
  ungroup()

# ---------------------------------------------------------
# 3. Generate Summary Tables
# ---------------------------------------------------------
# A. Individual Return Table
individual_return_table_Q <- events_returning_Q %>%
  group_by(common_name, animal_id, year = year(first_detection)) %>%
  summarise(
    Arrival = min(first_detection),
    Departure = max(last_detection),
    Days_Duration = as.numeric(difftime(Departure, Arrival, units = "days")),
    .groups = "drop"
  )

# B. Annual Return Summary Table (By Species)
# Helper function: finds maximum sequential span (e.g., 2015 and 2025 = 10 sequential years)
get_max_sequential_span <- function(years) {
  if(length(years) == 0) return(0)
  max(years) - min(years) + 1
}

species_return_summary_Q <- events_returning_Q %>%
  mutate(year = year(first_detection)) %>%
  group_by(common_name) %>%
  summarise(
    Total_Individuals = n_distinct(animal_id),
    Total_Years_Detected = n_distinct(year),
    Max_Sequential_Years_Detected = max(tapply(year, animal_id, get_max_sequential_span)),
    Years_Detected_List = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 4. Publication Abacus Plot (Figure S4)
# ---------------------------------------------------------
# Define custom palette for annual returns figure
my_palette_returns <- c("gray","#3C899E","#E4779C","#800000",
                        "#49A75A","#CFA42D","blue","#DCBEFF")

p_abacus_final_Q <- ggplot(events_returning_Q, 
                         aes(x = first_detection, 
                             y = as.factor(animal_id))) +
  # Dotted lines connecting detection periods for each individual
  geom_line(aes(group = animal_id), linetype = "dotted", linewidth = 1, color = "gray29") +
  # Detection events colored by species
  geom_point(aes(color = common_name), size = 2, alpha = 0.8) +
  # Formatting X-axis for Month and Year
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_color_manual(values = my_palette_returns, name = "Species") +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_text(angle = 90, size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(
    x = "Detection period date",
    y = "Animal ID"
  )

print(p_abacus_final_Q)

# Save outputs

ggsave("output/FigureS4_Abacus_Returns_Q.png", # save as tiff at 300 dpi for high quality
       plot = p_abacus_final_Q, 
       width = 7.1, .
       height = 5, 
       units = "in", 
       dpi = 150)
