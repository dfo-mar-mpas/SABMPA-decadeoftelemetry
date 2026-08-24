### SABMPA_decadeoftelemetry ###
# Script 0#: Individual Annual Returns
# Author: Harri Pettitt-Wade
# contact: harri.pettitt-wade@dfo-mpo.gc.ca
# Date Updated: 2026-08-20

# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# TODO: tidy the code and remove repeats (i.e., use code closer to the bottom)
# TODO: test run the code

# ---------------------------------------------------------
# 0. Load project configuration and packages
source("code/00_setup.R")
# ---------------------------------------------------------
# ---------------------------------------------------------
# Load the data
# ---------------------------------------------------------
SAB_glatos_FINAL <- read.csv("data/processed/SAB_glatos_FINAL.csv")
# ---------------------------------------------------------
# 1. Identify individuals with detections in > 1 year 
# ---------------------------------------------------------
returning_ids <- SAB_glatos_FINAL %>%
  group_by(animal_id) %>%
  summarise(n_years = n_distinct(yearcollected)) %>%
  filter(n_years > 1) %>%
  pull(animal_id)
# ---------------------------------------------------------
# 2. Filter the main dataset for only these returning individuals
# ---------------------------------------------------------
returning_fish_data <- SAB_glatos_FINAL %>%
  filter(animal_id %in% returning_ids)


detection_periods <- returning_fish_data %>%
  group_by(common_name, animal_id, yearcollected) %>%
  summarise(
    First_Detection = min(detection_timestamp_utc),
    Last_Detection = max(detection_timestamp_utc),
    Days_Present = as.numeric(difftime(Last_Detection, First_Detection, units = "days")),
    .groups = "drop"
  )

print(detection_periods)
# ---------------------------------------------------------
# 3. Plotting the results in an abacus style plot
# ---------------------------------------------------------
ggplot(returning_fish_data, aes(x = detection_timestamp_utc, y = as.factor(animal_id), color = common_name)) +
  geom_point(size = 1.5, alpha = 0.7) +
  theme_bw() +
  scale_color_viridis_d(name = "Species") +
  labs(
    title = "Annual Returns: Multi-Year Detection Patterns",
    x = "Date",
    y = "Animal ID"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

# ---------------------------------------------------------
## --- on detection events
# ---------------------------------------------------------
# 1. Classify discrete detection events
# ---------------------------------------------------------
# 'time_sep' defines the gap (in seconds) to start a new event
events <- detection_events(SAB_glatos_FINAL, time_sep = 43200)
# ---------------------------------------------------------
# 2. Extract years to identify annual returners
# ---------------------------------------------------------
events <- events %>%
  mutate(year_first = year(first_detection))
# ---------------------------------------------------------
# 3. Filter for individuals detected in > 1 calendar year
# ---------------------------------------------------------
returning_ids <- events %>%
  group_by(animal_id) %>%
  summarise(n_years = n_distinct(year_first)) %>%
  filter(n_years > 1) %>%
  pull(animal_id)

events_returning <- events %>%
  filter(animal_id %in% returning_ids)


annual_summary <- events_returning %>%
  group_by(common_name, animal_id, year_first) %>%
  summarise(
    Arrival = min(first_detection),
    Departure = max(last_detection),
    Duration_Days = as.numeric(difftime(Departure, Arrival, units = "days")),
    .groups = "drop"
  )

print(annual_summary)
# ---------------------------------------------------------
# 4. Create the publication-quality abacus plot
# ---------------------------------------------------------
ggplot(events_returning) +
  # Dotted lines connecting the events for each individual
  geom_line(aes(x = first_detection, y = as.factor(animal_id), group = animal_id), 
            linetype = "dotted", color = "gray60", linewidth = 0.5) +
  # Individual detection events
  geom_point(aes(x = first_detection, y = as.factor(animal_id), color = common_name), 
             size = 2, alpha = 0.8) +
  theme_bw() +
  scale_color_viridis_d(name = "Species") +
  labs(title = "Annual Returners: Detection Events and Gaps",
       subtitle = "Dotted lines indicate periods between confirmed detection events",
       x = "Year", y = "Animal ID") +
  theme(axis.text.y = element_text(size = 7),
        legend.position = "bottom")


# ---------------------------------------------------------
## ------ separate by species
# ---------------------------------------------------------
# Run Detection Events by Species

# ---------------------------------------------------------
# 1. Split the data by species, run events, and add common_name back
# ---------------------------------------------------------
events_list <- SAB_glatos_FINAL %>%
  split(.$common_name) %>%
  map(~{
    species_name <- unique(.x$common_name)
    # Run the glatos event classification
    ev <- detection_events(.x, time_sep = 43200)
    # Re-attach the common_name
    ev$common_name <- species_name
    return(ev)
  })
# ---------------------------------------------------------
# 2. Combine back into one master events file
# ---------------------------------------------------------
events_all <- bind_rows(events_list)
# ---------------------------------------------------------
# 3. Identify multi-year returners (detected in > 1 calendar year)
# ---------------------------------------------------------
returning_ids <- events_all %>%
  mutate(year_val = lubridate::year(first_detection)) %>%
  group_by(animal_id) %>%
  summarise(n_years = n_distinct(year_val)) %>%
  filter(n_years > 1) %>%
  pull(animal_id)
# ---------------------------------------------------------
# 4. Filter for only the returners
# ---------------------------------------------------------
events_returning <- events_all %>%
  filter(animal_id %in% returning_ids)
# ---------------------------------------------------------
#  5. Abacus Plot with Dotted Connections
# ---------------------------------------------------------
ggplot(events_returning, aes(x = first_detection, y = as.factor(animal_id))) +
  # Draw dotted lines between the first and last recorded events for each fish
  geom_line(aes(group = animal_id), linetype = "dotted", color = "gray70") +
  # Plot the actual detection events
  geom_point(aes(color = common_name), size = 2) +
  scale_color_viridis_d(name = "Species") +
  theme_bw() +
  labs(
    title = "Multi-Year Returners: Detection Timeline",
    subtitle = "Dotted lines indicate gaps between detection events",
    x = "Year",
    y = "Animal ID"
  ) +
  theme(axis.text.y = element_text(size = 6), legend.position = "bottom")

# ---------------------------------------------------------
## ---- refining and tables
# Generate Detection Events and Identify Returners
# ---------------------------------------------------------
# 1. Run detection events by species to retain common_name
# ---------------------------------------------------------
events_all <- SAB_glatos_FINAL %>%
  split(.$common_name) %>%
  map_df(~{
    species <- unique(.x$common_name)
    ev <- detection_events(.x, time_sep = 43200)
    ev$common_name <- species
    return(ev)
  })
# ---------------------------------------------------------
# 2. Identify and filter for annual returners (>1 year detected)
# ---------------------------------------------------------
events_returning <- events_all %>%
  mutate(year = year(first_detection)) %>%
  group_by(animal_id) %>%
  filter(n_distinct(year) > 1) %>%
  ungroup()
# ---------------------------------------------------------
# Abacus Plot (Publication Format)

p_abacus_final <- ggplot(events_returning, aes(x = first_detection, y = as.factor(animal_id))) +
  # Dotted lines connecting detection periods for each individual
  geom_line(aes(group = animal_id), linetype = "dotted", color = "gray70") +
  # Detection events colored by species
  geom_point(aes(color = common_name), size = 2, alpha = 0.8) +
  # Formatting X-axis for Month and Year
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_color_viridis_d(name = "Species") +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

print(p_abacus_final)
# ---------------------------------------------------------
# Detection Period Table (Individual Returns)

individual_return_table <- events_returning %>%
  group_by(common_name, animal_id, year = year(first_detection)) %>%
  summarise(
    Arrival = min(first_detection),
    Departure = max(last_detection),
    Days_Duration = as.numeric(difftime(Departure, Arrival, units = "days")),
    .groups = "drop"
  )

print(individual_return_table)

write.csv(individual_return_table,"individual_return_table.csv")
# ---------------------------------------------------------
# Annual Return Summary Table (By Species)

# Helper function to find max consecutive years
get_max_consecutive <- function(years) {
  if(length(years) == 0) return(0)
  years <- sort(unique(years))
  runs <- rle(diff(years))
  if(!any(runs$values == 1)) return(1)
  max(runs$lengths[runs$values == 1]) + 1
}

species_return_summary <- events_returning %>%
  mutate(year = year(first_detection)) %>%
  group_by(common_name) %>%
  summarise(
    Total_Years_Detected = n_distinct(year),
    Max_Consecutive_Years = get_max_consecutive(year),
    Years_Detected = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  )

print(species_return_summary)

# ---------------------------------------------------------
### ---- Annual returns table by species with total individuals

# Helper function to find the maximum sequential span for an individual
# e.g., detected in 2014 and 2024 = 10 sequential years
get_max_sequential_span <- function(years) {
  if(length(years) == 0) return(0)
  # Max year - Min year + 1 (the total number of years they were part of the study)
  max(years) - min(years) + 1
}

species_return_summary <- events_returning %>%
  mutate(year = year(first_detection)) %>%
  group_by(common_name) %>%
  summarise(
    Total_Individuals = n_distinct(animal_id),
    Total_Years_Detected = n_distinct(year),
    # This finds the single animal with the longest 'career' in the MPA
    Max_Sequential_Years_Detected = max(tapply(year, animal_id, get_max_sequential_span)),
    Years_Detected_List = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  )

print(species_return_summary)

write.csv(species_return_summary,"species_return_summary.csv")

# ---------------------------------------------------------
## abacus plot with my colours
# ---------------------------------------------------------
# my palette colours for final figure

#plot(my_palette) # view the current palette
#show_col(my_palette) # view the current palette

# define my palette for annual returns figure
my_palette_returns <- c("gray","#3C899E","#E4779C","#800000",
                        "#49A75A","#CFA42D","blue","#DCBEFF")
# ---------------------------------------------------------
# plot the abacus plot with my colours
p_abacus_final <- ggplot(events_returning, 
                         aes(x = first_detection, 
                            y = as.factor(animal_id))) +
  # Dotted lines connecting detection periods for each individual
  geom_line(aes(group = animal_id), linetype = "dotted", linewidth = 1,
            color = "gray29") +
  # Detection events colored by species
  geom_point(aes(color = common_name), size = 2, alpha = 0.8) +
  # Formatting X-axis for Month and Year
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_color_manual(values = my_palette_returns, name = "Species") +
  #scale_color_viridis_d(name = "Species") +
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
    legend.text = element_text(size = 14)) +
  labs(
  x = "Detection period date",
  y = "Animal ID")

print(p_abacus_final)
# ---------------------------------------------------------
# A. Save as TIFF (High-res, no compression)
ggsave("Figure_Abacus_Returns.tiff", 
       plot = p_abacus_final, 
       width = 7.1,
       height = 5,
       units = "in", 
       dpi = 300, 
       compression = "LZW") # to keep file size down. Change to 'none' for full size.
      
# B. or save as PNG (High-res, transparent background optional)
ggsave("Figure_Abacus_Returns.png", 
       plot = p_abacus_final, 
       width = 7.1, 
       height = 5, 
       units = "in", 
       dpi = 300)

