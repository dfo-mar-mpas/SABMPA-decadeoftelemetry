# A decade of acoustic telemetry in a Marine Protected Area reveals multispecies connectivity throughout the Northwest Atlantic
### Harri Pettitt-Wade 1**, Nicholas W. Jeffery 1, Ben Zisserson 1, Cassandra Hartery 2, Ryan R.E. Stanley 1

1. Coastal Ecosystem Science Division, Fisheries and Oceans Canada, Bedford Institute of Oceanography, Challenger Drive, PO Box 1006, Dartmouth, Nova Scotia, Canada B2Y 4A2
2. Ocean Tracking Network, 1355 Oxford St., Halifax, Nova Scotia, Canada B3H 3Z1

** Corresponding author: harri.pettitt-wade@dfo-mpo.gc.ca

## Overview

This repository contains the R code and open data access for the manuscript accepted for publication in the Canadian Journal of Fisheries and Aquatic Sciences (CJFAS) Special Collection 'Fish Telemetry to Address Management and Conservation" (https://cdnsciencepub.com/topic/cjfas-icftconference2025; Pettitt-Wade et al. 2026). Raw acoustic telemetry data is available on the Ocean Tracking Network (OTN) data portal (Stanley et al. 2016; https://members.oceantrack.org/project?ccode=SABMPA). 

Please reach out for questions or further information including appropriate use of the data and code.
_

## Repository Structure

- data/raw/: Raw telemetry detections and metadata (Not tracked on GitHub due to file size). 

- data/processed/: Cleaned dataset used for final analyses and model fitting.

- code/: R scripts for data processing, spatial plotting, and statistical modeling.

- outputs/: summarized data, figures, and tables including those published in the manuscript (Figures 1-5, Figures S1-5, Tables 1-3, Tables S1-3).

- renv/ & renv.lock: R environment files for exact package reproducibility.

## Data Availability

The processed data required to run the analysis is included in the data/processed/ folder. Raw data was too large for github and can be downloaded on the Ocean Tracking Network (OTN) data portal and geoserver (https://members.oceantrack.org/project?ccode=SABMPA). See the script (...) for steps to offload, clean, and process raw data to get to the processed data stage (...).

## How to Run the Code

To reproduce the analysis and figures from the manuscript:

1. Clone the repository: Download this repository and open the R Project file (.Rproj).

2. Restore the environment: Run renv::restore() in the console. This will automatically install the exact versions of packages used in this study (e.g., glatos, treemapify, and ggplot2).

3. Run the setup script: Execute scripts/00_setup.R to load required libraries and set parameters.

4. Generate results: Scripts are numbered sequentially. For example, run scripts/03_mixed_models.R to reproduce the statistical outputs and scripts/04_figures_tables.R to generate the maps and charts once the previous scripts have been run to generate the necessary summary data.

## Citation

If you use this code or data, please cite the original paper:

Pettitt-Wade, H.,  Jeffery, N., Zisserson, B., Hartery, C., Stanley, R.R.E. 2016. A decade of acoustic telemetry in a Marine Protected Area reveals multispecies connectivity throughout the Northwest Atlantic. CJFAS. Accepted. DOI:
