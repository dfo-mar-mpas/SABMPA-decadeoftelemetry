# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 08_FigureS4_annualreturns.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-09-23
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
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

# Load the data (Note: Adjust to .rds if you saved it as RDS in 01_datacleaning)
SAB_glatos_FINAL <- read.csv("data/processed/SAB_glatos_FINAL.csv")

# ---------------------------------------------------------
# 1. Generate Detection Events and Retain Metadata
# ---------------------------------------------------------
# Split by species to run events, explicitly using purrr::map_df to avoid map.poly errors
events_all <- SAB_glatos_FINAL %>%
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
events_returning <- events_all %>%
  mutate(year = year(first_detection)) %>%
  group_by(animal_id) %>%
  filter(n_distinct(year) > 1) %>%
  ungroup()

# ---------------------------------------------------------
# 3. Generate Summary Tables
# ---------------------------------------------------------
# A. Individual Return Table
individual_return_table <- events_returning %>%
  group_by(common_name, animal_id, year = year(first_detection)) %>%
  summarise(
    Arrival = min(first_detection),
    Departure = max(last_detection),
    Days_Duration = as.numeric(difftime(Departure, Arrival, units = "days")),
    .groups = "drop"
  )

write.csv(individual_return_table, "output/individual_return_table.csv", row.names = FALSE)

# B. Annual Return Summary Table (By Species)
# Helper function: finds maximum sequential span (e.g., 2014 and 2024 = 10 sequential years)
get_max_sequential_span <- function(years) {
  if(length(years) == 0) return(0)
  max(years) - min(years) + 1
}

species_return_summary <- events_returning %>%
  mutate(year = year(first_detection)) %>%
  group_by(common_name) %>%
  summarise(
    Total_Individuals = n_distinct(animal_id),
    Total_Years_Detected = n_distinct(year),
    Max_Sequential_Years_Detected = max(tapply(year, animal_id, get_max_sequential_span)),
    Years_Detected_List = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  )

write.csv(species_return_summary, "output/species_return_summary.csv", row.names = FALSE)

# ---------------------------------------------------------
# 4. Publication Abacus Plot (Figure S4)
# ---------------------------------------------------------
# Define custom palette for annual returns figure
my_palette_returns <- c("gray","#3C899E","#E4779C","#800000",
                        "#49A75A","#CFA42D","blue","#DCBEFF")

p_abacus_final <- ggplot(events_returning, 
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

print(p_abacus_final)

# Save figure output
ggsave("output/FigureS4_Abacus_Returns.png", plot = p_abacus_final, 
       width = 7.1, height = 5, units = "in", dpi = 300)
