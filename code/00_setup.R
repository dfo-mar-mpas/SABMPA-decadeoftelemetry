### SABMPA_decadeoftelemetry ###
# Script 00: Project Setup and Configuration
# Author: Harri Pettitt-Wade
# Date Updated: 2026-08-20

# Accompanying the manuscript:
# Harri Pettitt-Wade, Nicholas W. Jeffery, Ben Zisserson, Cassandra Hartery, Ryan R.E. Stanley. 2026. 
# A decade of acoustic telemetry in a Marine Protected Area reveals multispecies connectivity throughout the Northwest Atlantic.
# Accepted 4 July 2026
# Canadian Journal of Fisheries and Aquatic Sciences. 
# DOI: [Insert DOI here once assigned by the publisher]

# ---------------------------------------------------------
# 1. INSTALLATION NOTES (Run manually once if needed)
# ---------------------------------------------------------
# Install high-res earth data from the r-universe instead of GitHub to bypass PAT issues
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")

# ---------------------------------------------------------
# 2. LOAD REQUIRED PACKAGES
# ---------------------------------------------------------

# Data Wrangling & Manipulation
library(plyr) # TODO: remove once scripts are running smooth
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(magrittr)
library(stats)
library(stringr)
library(purrr)
library(scales)

# Spatial & Telemetry Analysis
library(sf)
library(sp) # TODO: remove once scripts are running smooth
library(raster) # TODO: switch for terra() once scripts are running smooth
library(glatos)
library(oceanmap) # TODO: remove / check needed once scripts are running smooth
library(arcpullr) # TODO: remove / check needed once scripts are running smooth
library(geosphere)
library(ggspatial)

# Visualization & Mapping
library(ggplot2)
library(treemapify)
library(gganimate) # TODO: remove / check needed once scripts are running smooth
library(ggmap)
library(leaflet)
library(plotly)
library(ggridges) # TODO: remove / check needed once scripts are running smooth
library(scales)
library(viridis)
library(RColorBrewer)
library(brew) # TODO: remove / check needed once scripts are running smooth
library(patchwork)
library(cowplot)

# ---------------------------------------------------------
# 3. GLOBAL OPTIONS & CONFIGURATION
# ---------------------------------------------------------

# Prevent scientific notation
options(scipen = 999)

# Set standard width for console outputs
options(width = 85)

# Set 'str' options for cleaner environment readouts
str_opts <- getOption("str") 
str_opts$strict.width = "wrap"
str_opts$vec.len = 1
options(str = str_opts)

# Set global seed for reproducible stochastic processes
set.seed(1234)

# Knitr options for RMarkdown generation (if applicable)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# ---------------------------------------------------------
# 4. API KEYS & CREDENTIALS
# ---------------------------------------------------------
# Load Stadia Maps API key securely from local .Renviron file
ggmap::register_stadiamaps(Sys.getenv("STADIA_MAPS_KEY"))

# ---------------------------------------------------------
# 5. CUSTOM PLOTTING THEMES & PALETTES (Placeholders)
# ---------------------------------------------------------
# TODO: Define standard ggplot theme
# theme_sabmpa <- theme_bw(...)
# theme_set(theme_sabmpa)

# TODO: Define standard species color palette
# species_palette <- c("White shark" = "#...", "Atlantic cod" = "#...")