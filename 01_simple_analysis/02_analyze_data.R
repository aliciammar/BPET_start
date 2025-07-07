#
# ------------ Mastering R: Best Practices and Essential Tools ------------ #
#
# This script:
# - Loads the data created in 01_get_data.R
# - Calculates the NDVI from the satellite image
# - Visualizes the NDVI using ggplot2
# - Saves the plot
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

library(sf)
library(terra)
library(tidyterra)
library(tidyverse)

# 2. Load data ------------------------------------------------------------

## Study area
selected_muni_sf <- read_sf("data/municipality.geojson")

## Satellite image
sentinel_sr <- rast("data/sentinel/ES6538026.tif")

# 3. Data analysis --------------------------------------------------------

## Calculate NDVI
## Formula: NDVI = (N - R) / (N + R)


## Rename band


# 4. Results --------------------------------------------------------------

## Visualize


## Save plot


