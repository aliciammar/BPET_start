#
# ------------ Mastering R: Best Practices and Essential Tools ------------ #
#
# This script downloads and save:
# - The boundaries of the municipality of La Orotava, in Tenerife
# - A satellite image of the bounding box of the municipality
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

library(giscoR)
library(mapview)
library(sf)
library(rsi)
library(terra)
library(tictoc)
library(tidyverse)

# 2. Get study area -------------------------------------------------------

## Set constants
sel_crs  <- "EPSG:25828"
sel_muni <- "Orotava, La"

## 2.1. Get Spain municipalities --------------

## Get Spain municipalities
spanish_muni_sf <- gisco_get_communes(
    country = "Spain"
) |>    # pipe operator is shift + cmd + m
    st_transform(sel_crs) # Change CRS to show geometry in meters

## Visualize
mapview(spanish_muni_sf)

## 2.2. Get Tenerife Island -------------------

## Get Tenerife Island
tenerife_sf <- gisco_get_nuts(
    country    = "Spain",
    resolution = "01",
    nuts_level = 3
) |> 
    filter(
        NAME_LATN == "Tenerife"
    ) |> 
    st_transform(sel_crs)

## Visualize
mapview(tenerife_sf) 

## 2.3. Get Tenerife municipalities ------------

## Filter municipalities intersecting Tenerife Island
tenerife_muni_sf <- st_filter(         # arrow operator is option -
    x = spanish_muni_sf,
    y = tenerife_sf
)

## Visualize
mapview(tenerife_muni_sf)

# 3. Download satellite image ---------------------------------------------

## 3.1. Select a municipality -----------------

## Filter municipality
selected_muni_sf <- tenerife_muni_sf |> 
    filter(
        NAME_LATN == sel_muni
    )

## Visualize
mapview(selected_muni_sf)

## 3.2. Get satellite image -------------------

## Download Sentinel-2 image
tic() # Outputs the computation time from tic() to toc()
sentinel_path <- get_sentinel2_imagery(
    aoi             = selected_muni_sf, # area of interest (AOI)
    start_date      = "2024-05-04", # time window to pull satellite photo from
    end_date        = "2024-05-05",
    output_filename = str_glue("data/sentinel/{selected_muni_sf$id}.tif")
)
toc()

## Read
sentinel_sr <- rast(sentinel_path)

## Visualize
(sentinel_sr / 10000) |> # Apparently sentinel data are stored multiplied by 10,000 so there are no                         floats (numbers with decimals)--maybe it uses less storage/is a smaller file                         this way?
    plotRGB(4, 3, 2, stretch = "lin") 

plot(
    st_geometry(selected_muni_sf),
    add    = TRUE,
    border = "red",
    lwd    = 2
    )

# 4. Export ---------------------------------------------------------------

## Export municipality
write_sf(selected_muni_sf, "data/municipality.geojson")



