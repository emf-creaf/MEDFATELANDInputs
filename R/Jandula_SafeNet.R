# Input generation for Jándula river watershed
#
# Project SafeNet
# Albrich Katharina (LUKE)
# Honkaniemi Juha (LUKE)
# AITOR GASTON GONZALEZ (UPM) 


source("R/define_spanish_landscape.R")

# Top parameters
emf_dataset_path <- "~/datasets/"
buffer_dist <- 50000
crs_out <- "EPSG:25830" # UTM 30N

jandula <- sf::read_sf("data-raw/boundary/jandula_wgs84.shp")
ifn_imputation_source <- "IFN4"

for(res in c(200,500)) {
  l <- define_spanish_landscape(target_polygon = jandula,
                                emf_dataset_path = emf_dataset_path,
                                res = res, 
                                buffer_dist = buffer_dist,
                                crs_out = crs_out, 
                                ifn_imputation_source = ifn_imputation_source)
  
  # Store sf object and raster    
  out_sf <- paste0("data/jandula/medfateland_jandula_sf_", res,"m.rds")
  output_tif <- paste0("data/jandula/medfateland_jandula_raster_",res,"m.tif")
  saveRDS(dplyr::as_tibble(l$sf), file = out_sf)
  terra::writeRaster(l$r, filename=output_tif, overwrite = TRUE)
  
  # Generate test plots
  medfateland::plot_variable(l$sf, "elevation", r = l$r)
  medfateland::plot_variable(l$sf, "mean_tree_height", r = l$r)
  medfateland::plot_variable(l$sf, "basal_area", r = l$r)
}
