source("R/define_spanish_landscape.R")

# Top parameters
emf_dataset_path <- "~/datasets/"
test_plots <- TRUE
res <- 500
buffer_dist <- 50000

# Provinces to process
provinces <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
               as.character(11:50))
provinces <- sample(provinces)

# Common raster for provinces (res = 500 m)
raster_platon_specs <- readRDS("data-raw/penbal_platon_specs.rds")
raster_platon <- terra::rast(
  extent = raster_platon_specs$extent,
  resolution = raster_platon_specs$resolution,
  crs = raster_platon_specs$crs
)
res_raster <- sqrt(prod(terra::res(raster_platon)))

if((res %% res_raster)==0) { # If res is a multiple of res_raster, then aggregate
  raster_platon <- terra::aggregate(raster_platon, fact = res/res_raster)
} else { # Otherwise rasterize in the function
  raster_platon <- NULL
}

# Province loop
for(province_code in provinces) {
  out_sf <- paste0("data/spain_forests_provinces_",res,"m/medfateland_", province_code, "_sf_", res,"m.rds")
  output_tif <- paste0("data/spain_forests_provinces_", res, "m/medfateland_", province_code, "_raster_",res,"m.tif")
  if(!file.exists(out_sf)) {
    cli::cli_h1(paste0("Processing province: ", province_code))
    
    # Decide imputation source depending on IFN4 availability
    ifn_imputation_source <- "IFN4"
    ifn_file <- paste0(emf_dataset_path, "ForestInventories/IFN_medfateland/medfateland_",
                       tolower(ifn_imputation_source), "_",province_code,"_soilmod_WGS84.rds")
    if(!file.exists(ifn_file)) ifn_imputation_source = "IFN3"
    
    # Options changing depending on Peninsula vs. Canary Islands
    if(province_code %in% c("35", "38")) {
      crs_out <- "EPSG:32628" # UTM 28N
      target_raster <- NULL # Do not use target raster because it does not include Canary Islands
      biomass_correction <- FALSE # Biomass map does not include Canary Islands
    } else {
      crs_out <- "EPSG:25830" # UTM 30N
      target_raster <- raster_platon
      biomass_correction <- TRUE
    }
    
    # Call initialisation routine
    l <- define_spanish_landscape(province_code = province_code,
                                  emf_dataset_path = emf_dataset_path,
                                  res = res, 
                                  target_raster = target_raster,
                                  buffer_dist = buffer_dist,
                                  crs_out = crs_out, 
                                  biomass_correction = biomass_correction, 
                                  ifn_imputation_source = ifn_imputation_source)

    # Store sf object and raster    
    saveRDS(dplyr::as_tibble(l$sf), file = out_sf)
    terra::writeRaster(l$r, filename=output_tif, overwrite = TRUE)
    
    # Generate test plots
    if(test_plots) {
      ggplot2::ggsave(paste0("plots/provinces/elevation_", province_code, "_", res, "m.png"),
                      medfateland::plot_variable(l$sf, "elevation", r = l$r))
      ggplot2::ggsave(paste0("plots/provinces/mean_tree_height_", province_code, "_", res, "m.png"),
                      medfateland::plot_variable(l$sf, "mean_tree_height", r = l$r))
      ggplot2::ggsave(paste0("plots/provinces/basal_area_", province_code, "_", res, "m.png"),
                      medfateland::plot_variable(l$sf, "basal_area", r = l$r))
      
    }
  }
}
