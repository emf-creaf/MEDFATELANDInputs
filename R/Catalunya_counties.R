source("R/define_spanish_landscape.R")


res <- 100
buffer_dist <- 5000
emf_dataset_path <- "~/datasets/"
test_plots <- TRUE

comarques <- sf::read_sf(paste0(emf_dataset_path, "PoliticalBoundaries/Catalunya/Comarques/comarques.shp"))
comarques<- comarques |>
  dplyr::filter(NOM_COMAR != "Pla d'Urgell")


# Landscapes --------------------------------------------------------------
for(i in sample(1:nrow(comarques))) {
  county_polygon <- comarques[i,]
  county_name <- comarques$NOM_COMAR[i]
  county_code <- comarques$COMARCA[i]
  out_sf <- paste0("data/cat_landscapes_counties_",res,"m/cat_landscape_", county_code, "_sf_", res,"m.rds")
  output_tif <- paste0("data/cat_landscapes_counties_", res, "m/cat_landscape_", county_code, "_raster_",res,"m.tif")

  if(!file.exists(out_sf))  {
    cli::cli_h1(paste0("Processing county: ", county_name, "(", county_code, ")"))

    l <- define_spanish_landscape(emf_dataset_path = emf_dataset_path,
                                  target_polygon = county_polygon,
                                  res = res,
                                  buffer_dist = buffer_dist,
                                  ifn_imputation_source = "IFN4",
                                  forests_only = FALSE,
                                  river_network = TRUE)

    # Store sf object and raster
    saveRDS(dplyr::as_tibble(l$sf), file = out_sf)
    terra::writeRaster(l$r, filename=output_tif, overwrite = TRUE)

    ggplot2::ggsave(filename = paste0("plots/counties_land/land_cover_type_",county_code,"_",res,"m.png"),
                    medfateland::plot_variable(l$sf, "land_cover_type", r = l$r),width = 5)
    ggplot2::ggsave(filename = paste0("plots/counties_land/elevation_",county_code,"_",res,"m.png"),
                    medfateland::plot_variable(l$sf, "elevation", r = l$r),width = 5)
    ggplot2::ggsave(filename = paste0("plots/counties_land/mean_tree_height_",county_code,"_",res,"m.png"),
                    medfateland::plot_variable(l$sf, "mean_tree_height", r = l$r),width = 5)
    ggplot2::ggsave(filename = paste0("plots/counties_land/basal_area_",county_code,"_",res,"m.png"),
                    medfateland::plot_variable(l$sf, "basal_area", r = l$r),width = 5)
    ggplot2::ggsave(filename = paste0("plots/counties_land/channel_",county_code,"_",res,"m.png"),
                    medfateland::plot_variable(l$sf, "channel", r = l$r), width = 5)
    
  }
}


# Forests -----------------------------------------------------------------
# for(i in sample(1:nrow(comarques))) {
#   county_polygon <- comarques[i,] 
#   county_name <- comarques$NOM_COMAR[i]
#   county_code <- comarques$COMARCA[i]
#   out_sf <- paste0("data/cat_forests_counties_",res,"m/medfateland_", county_code, "_sf_", res,"m.rds")
#   output_tif <- paste0("data/cat_forests_counties_", res, "m/medfateland_", county_code, "_raster_",res,"m.tif")
# 
#   if(!file.exists(out_sf))  {
#     cli::cli_h1(paste0("Processing county: ", county_name, "(", county_code, ")"))
#     
#     l <- define_spanish_landscape(emf_dataset_path = emf_dataset_path,
#                                              target_polygon = county_polygon,
#                                              res = res,
#                                              buffer_dist = buffer_dist,
#                                              ifn_imputation_source = "IFN4")
#     
#     # Store sf object and raster    
#     saveRDS(dplyr::as_tibble(l$sf), file = out_sf)
#     terra::writeRaster(l$r, filename=output_tif, overwrite = TRUE)
#     
#     ggplot2::ggsave(filename = paste0("plots/counties/elevation_",county_code,"_",res,"m.png"),
#                     medfateland::plot_variable(l$sf, "elevation", r = l$r),width = 5)
#     ggplot2::ggsave(filename = paste0("plots/counties/mean_tree_height_",county_code,"_",res,"m.png"),
#                     medfateland::plot_variable(l$sf, "mean_tree_height", r = l$r),width = 5)
#     ggplot2::ggsave(filename = paste0("plots/counties/basal_area_",county_code,"_",res,"m.png"),
#                     medfateland::plot_variable(l$sf, "basal_area", r = l$r),width = 5)
#     
#   }
#   
# }
