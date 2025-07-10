## FUNCTION: Fix the municipality names
fix_muni_names <- function(name) {
    
    muni_split <- name |> 
        str_split(",") |> 
        pluck(1)
    
    paste(muni_split[2], muni_split[1]) |>
        str_trim()
    
}

## FUNCTION: Vectorized fix municipality names 
fix_muni_names_vec <- Vectorize(fix_muni_names)

## FUNCTION: Download Tenerife municipalities
get_tenerife_muni <- function(sel_crs = "EPSG:25828") {
    
    ## Get Spain municipalities
    spanish_muni_sf <- gisco_get_communes(
        country = "Spain"
    ) |>    
        st_transform(sel_crs) 
    
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
    
    ## Filter municipalities intersecting Tenerife Island
    filtered_municipalities <- st_filter(         
        x = spanish_muni_sf,
        y = tenerife_sf
    )
    
    ## Fix the municipality names
    filtered_municipalities |> 
        mutate(
            change = if_else(
                str_detect(COMM_NAME, ","), TRUE, FALSE
            )
        ) |> 
        mutate(
            fixed_names = if_else(
                change,
                fix_muni_names_vec(COMM_NAME),
                COMM_NAME
            )
        )
    
}

## FUNCTION: Download satellite image for each municipality
get_sentinel2_muni <- function(data) {
    
    ## Select bands (we only want 2 of the 12 available)
    bands <- rsi::sentinel2_band_mapping$planetary_computer_v1[c("B04", "B08")]
    
    ## Download Sentinel-2 image
    sentinel_path <- get_sentinel2_imagery(
        aoi             = data, 
        start_date      = "2024-05-04", 
        end_date        = "2024-05-05",
        asset_names     = bands,
        output_filename = str_glue("data/sentinel/{data$id}.tif")
    )
    
    ## Scale
    rast(sentinel_path) / 10000
    
}

## FUNCTION: Calculate NDVI
calculate_ndvi <- function(data) {
    
    ## Calculate NDVI
    ## Formula: NDVI = (N - R) / (N + R)
    ndvi_sr <- (data$N - data$R) / (data$N + data$R)
     
    ## Rename band
    names(ndvi_sr) <- "NDVI"
    
    ## Return NDVI
    ndvi_sr
    
}
