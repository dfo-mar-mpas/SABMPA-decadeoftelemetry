# ===================================================================
### SABMPA_decadeoftelemetry ###
# Script: 05_Figure5_LMMs.R
# Author: Harri Pettitt-Wade
# Date Updated: 2026-08-24
# Accompanying: Pettitt-Wade et al (2026) - CJFAS
# Description: Fits Linear Mixed Effects Models for environmental trends with 
#             individual species occurrences from SABMPA acoustic telemetry data 
#             and generates Figure 5 and data for Table S3 for the CJFAS manuscript.
# ===================================================================

# ---------------------------------------------------------
# 0. Load project configuration and packages
# ---------------------------------------------------------
source("code/00_setup.R")
# ---------------------------------------------------------
library(dplyr)
library(lubridate)
library(purrr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(performance)
library(ggplot2)
library(readr)

# Load the data
SAB_model_data <- readRDS("data/processed/SAB_model_data.rds")

# ---------------------------------------------------------
# 1. Data Preparation & Filtering
# ---------------------------------------------------------
# Add numeric year and array design eras
model_ready_data <- SAB_model_data %>%
  mutate(
    year_numeric = as.numeric(format(date_daily, "%Y")) + (as.numeric(format(date_daily, "%j")) / 365),
    array_design = ifelse(grepl("^OTN_", station), "Single (2021-2025)", "Double (2015-2021)")
  )

# Aggregate to Daily Mean per Individual to prevent over-weighting by highly detected tags
daily_indiv_data <- model_ready_data %>%
  group_by(common_name, animal_id, array_design, year_numeric, date_daily) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    mean_depth = mean(mean_depth, na.rm = TRUE),
    .groups = "drop"
  )

# Filter for species with sufficient statistical power (> 5 individuals, >= 2 years)
power_species <- model_ready_data %>%
  group_by(common_name) %>%
  summarise(
    n_fish = n_distinct(animal_id), 
    n_years = n_distinct(year(date_daily))
  ) %>%
  filter(n_fish > 5 & n_years >= 2) %>%
  pull(common_name)

# ---------------------------------------------------------
# 2. Linear Mixed-Effects Models (LMM)
# ---------------------------------------------------------
# Calculate population-level annual change, correcting for array shifts
rigorous_results_detailed <- daily_indiv_data %>%
  filter(common_name %in% power_species) %>%
  split(.$common_name) %>%
  map_df(~{
    
    # Check if the species exists in both array designs to avoid contrast errors
    if(n_distinct(.x$array_design) > 1) {
      form_temp  <- temperature ~ year_numeric + array_design + (1|animal_id)
      form_depth <- mean_depth ~ year_numeric + array_design + (1|animal_id)
      era_corrected <- "Yes"
    } else {
      form_temp  <- temperature ~ year_numeric + (1|animal_id)
      form_depth <- mean_depth ~ year_numeric + (1|animal_id)
      era_corrected <- "No"
    }
    
    m_temp  <- lmer(form_temp, data = .x)
    m_depth <- lmer(form_depth, data = .x)
    
    # Calculate Marginal R-squared (Fixed effects)
    r2_t <- performance::r2(m_temp)
    r2_d <- performance::r2(m_depth)
    
    bind_rows(
      tidy(m_temp, effects = "fixed") %>% filter(term == "year_numeric") %>% 
        mutate(variable = "Temp_Trend", R2_m = r2_t$R2_marginal, is_singular = isSingular(m_temp)),
      tidy(m_depth, effects = "fixed") %>% filter(term == "year_numeric") %>% 
        mutate(variable = "Depth_Trend", R2_m = r2_d$R2_marginal, is_singular = isSingular(m_depth))
    ) %>%
      mutate(
        common_name = unique(.x$common_name),
        n_fish = n_distinct(.x$animal_id),
        n_obs = nrow(.x),
        era_correction = era_corrected
      )
  })

# ---------------------------------------------------------
# 3. Format Results & Prepare Plot Metadata
# ---------------------------------------------------------
# Generate sidebar metadata for the y-axis (n fish | years | total days)
species_metadata <- daily_indiv_data %>%
  filter(!common_name %in% c("Atlantic halibut", "American eel")) %>% # check to remove species with insuficient data
  group_by(common_name) %>%
  summarise(
    n_fish = n_distinct(animal_id),
    n_yrs = n_distinct(year(date_daily)),
    total_days = n(),
    label_text = paste0("n=", n_fish, " | ", n_yrs, "yrs | ", total_days, "d total"),
    .groups = "drop"
  )

# Join metadata to model results and format for plotting
p_effects_refined <- rigorous_results_detailed %>%
  filter(!common_name %in% c("Atlantic halibut", "American eel")) %>% # check to remove species with insuficient data
  left_join(species_metadata, by = "common_name") %>%
  group_by(common_name) %>%
  mutate(
    significant = p.value < 0.05,
    variable = ifelse(variable == "Temp_Trend", "Temperature (°C / yr)", "Depth (m / yr)"),
    # Combine species name and metadata onto two lines
    combined_name = paste0(common_name, "\n", first(label_text))
  ) %>%
  ungroup() %>%
  mutate(
    # Alphabetize based on the combined name string so 'A' plots at the top
    combined_name = factor(combined_name, levels = rev(sort(unique(combined_name))))
  )

# ---------------------------------------------------------
# 4. Final Publication Plot
# ---------------------------------------------------------
# Ensure palette matches the 5 final plotted species
my_palette_model <- c("darkgray", "#E4779C", "#800000", "#49A75A", "#DCBEFF")

p_effects_final <- ggplot(p_effects_refined, aes(x = estimate, y = combined_name)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error, color = common_name), 
                 height = 0.2, linewidth = 1.2) +
  
  geom_point(aes(color = common_name, fill = significant), shape = 21, size = 5, stroke = 1.5) +
  
  scale_color_manual(values = my_palette_model, guide = "none") +
  scale_fill_manual(name = "Significance (p < 0.05)",
                    values = c("TRUE" = "black", "FALSE" = "white"),
                    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")) +
  
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_line(color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 12), size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 10, lineheight = 1.2, color = "gray20"),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Annual rate of change derived from\nlinear mixed-effects models of individuals", y = NULL)

print(p_effects_final)

# ---------------------------------------------------------
# 5. Export Final Assets
# ---------------------------------------------------------
# Format manuscript summary table
manuscript_table <- p_effects_refined %>%
  mutate(
    Significant = ifelse(significant == TRUE, "Yes", "No"),
    Annual_Rate = round(estimate, 4),
    Std_Error = round(std.error, 4),
    P_Value = round(p.value, 4),
    R2_Marginal = round(R2_m, 3)
  ) %>%
  # We use n_fish = n_fish.x to grab the renamed column from the join
  dplyr::select(common_name, variable, Annual_Rate, Std_Error, P_Value, 
                Significant, n_fish = n_fish.x, n_years = n_yrs, total_days, R2_Marginal) %>%
  arrange(variable, P_Value)

write_csv(manuscript_table, "output/TableS3_SAB_LMM_Results.csv")

# Save TIFF for journal submission
ggsave("output/Figure5_Model_Effects_HighRes_2.tiff", 
       plot = p_effects_final, 
       width = 6.5,             
       height = 4.5,             
       units = "in", 
       dpi = 450,              
       compression = "lzw")

# Save lightweight PNG for GitHub README
ggsave("output/Figure5_Model_Effects_README.png", 
       plot = p_effects_final, 
       width = 6.5, 
       height = 4.5, 
       units = "in", 
       dpi = 150)