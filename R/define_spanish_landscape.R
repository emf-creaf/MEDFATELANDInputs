#' Initialize spanish forested landscape
#' 
#' Initializes inputs for a target Spanish forested area for simulations with medfateland 
#' The target should be contained (or include the whole of) a Spanish province
#'
#' @param emf_dataset_path Path to the dataset folder
#' @param province_code String with the code of the province containing the target polygon
#' @param target_polygon Target polygon for the study area. If missing the whole province is taken
#' @param target_raster 
#' @param buffer_dist Distance (in m) used for defining a buffer from which information can be drawn
#' @param ifn_imputation_source String indicating the forest inventory version to use in forest stand imputation ("IFN2", "IFN3" or "IFN4")
#' @param res Spatial resolution (in m) of the raster definition.
#' @param crs_out String of the CRS 
#' @param forests_only 
#' @param fit_raster_extent 
#' @param spatial_aggregation Performs aggregation (median) of DEM, height map and biomass map to the output raster resolution before using them.
#' @param height_correction Logical flag to try tree height correction
#' @param biomass_correction Logical flag to try tree biomass_correction
#' @param soil_correction Logical flag to try soil depth and rock fragment content correction
#' @param river_network Logical flag to add river network
#' @param verbose Logical flag for console output
#' 
#' @author Rodrigo Balaguer Romano
#' @author Miquel De Cáceres
#' 
#' @returns A list composed of a sf object suitable for medfateland and a terra raster definition
#' @export
#'
#' @examples
define_spanish_landscape <- function(emf_dataset_path,
                                     province_code = NULL,
                                     target_polygon  = NULL,
                                     target_raster = NULL,
                                     buffer_dist = 50000,
                                     ifn_imputation_source = "IFN4",
                                     res = 500, 
                                     crs_out = "EPSG:25830", 
                                     forests_only = TRUE,
                                     fit_raster_extent = TRUE,
                                     spatial_aggregation = TRUE,
                                     height_correction = TRUE,
                                     biomass_correction = TRUE,
                                     soil_correction = TRUE,
                                     river_network = FALSE,
                                     verbose = TRUE) {
  provinces <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                 as.character(11:50))
  province_utm_fuses <- rep("30", 50) # Peninsular spain
  province_utm_fuses[c(35, 38)] <- "28" #Canarias
  

  # Check function inputs 
  if(!is.null(province_code)) {
    province_code <- as.character(province_code)
    province_code <- match.arg(province_code, provinces)
  } else if(is.null(target_polygon)) {
    cli::cli_abort("You must specify either a province or a target polygon")
  }
  if(!is.numeric(buffer_dist)) cli::cli_abort("`buffer_dist` should be numeric")
  if(!is.null(target_raster)) {
    res <- sqrt(prod(terra::res(target_raster)))
    if(verbose) cli::cli_li(paste0("Target resolution from target raster: ", res," m"))
  } else {
    if(!is.numeric(res)) cli::cli_abort("`res` should be numeric")
    if(verbose) cli::cli_li(paste0("Target resolution: ", res," m"))
  }
  if(!is.character(crs_out)) cli::cli_abort("`crs_out` should be a string")
  ifn_imputation_source <- toupper(ifn_imputation_source)
  ifn_imputation_source <- match.arg(ifn_imputation_source, c("IFN2", "IFN3", "IFN4"))

  sf_all_provinces <- sf::read_sf(paste0(emf_dataset_path, "PoliticalBoundaries/Spain/Provincias_ETRS89_30N/Provincias_ETRS89_30N.gpkg"))
  sf_all_provinces <- sf::st_make_valid(sf_all_provinces)
  sf_all_provinces <- sf_all_provinces[1:50, ] # Exclude ceuta y melilla

  # If target polygon not supplied, use the whole province
  if(is.null(target_polygon)) {
    if(verbose) cli::cli_li(paste0("Set province ", province_code," as target polygon"))
    target_polygon <- sf_all_provinces |>
      dplyr::filter(Codigo == province_code) |>
      sf::st_as_sfc()
    if(sf::st_crs(target_polygon) != sf::st_crs(crs_out)) {
      target_polygon <- target_polygon |>
        sf::st_transform(crs = sf::st_crs(crs_out)) 
    }
  } else {
    target_polygon <- target_polygon |>
        sf::st_union()
    if(sf::st_crs(target_polygon) != sf::st_crs(crs_out)) {
      target_polygon <- target_polygon |>
        sf::st_transform(crs = sf::st_crs(crs_out)) 
    }
  }
  if(verbose) cli::cli_li(paste0("Target polygon area: ", round(sf::st_area(target_polygon)/10000)," ha"))
  target_buffer <- sf::st_buffer(target_polygon, dist = buffer_dist)
  if(verbose) cli::cli_li(paste0("Buffer zone area: ", round((sf::st_area(target_buffer) - sf::st_area(target_polygon))/10000)," ha"))
  
  if(verbose) cli::cli_progress_step(paste0("Defining touched provinces"))
  if(sf::st_crs(sf_all_provinces)!=sf::st_crs(crs_out)) {
    sf_all_provinces <- sf_all_provinces |>
      sf::st_transform(crs = sf::st_crs(crs_out)) 
  }
  touched_provinces <- sf::st_intersection(sf_all_provinces, target_buffer)$Codigo

  sf_mfe_buffer_list <- vector("list", length(touched_provinces))
  sf_mfe_target_list <-  vector("list", length(touched_provinces))
  for(i in 1:length(touched_provinces)) {
    prov <- touched_provinces[i]
    if(verbose) cli::cli_progress_step(paste0("Reading MFE polygons from province ", prov))
    sf_mfe_prov <- sf::read_sf(paste0(emf_dataset_path, "ForestMaps/Spain/MFE25/MFE_PROVINCES/MFE_", prov, "_class.gpkg")) |>
      sf::st_make_valid()
    if(sf::st_crs(sf_mfe_prov)!= sf::st_crs(crs_out)) {
      sf_mfe_prov <- sf_mfe_prov |>
        sf::st_transform(crs = sf::st_crs(crs_out)) 
    }
    int_buffer <- sf::st_intersection(sf_mfe_prov, target_buffer) |>
      na.omit()
    sf_mfe_buffer_list[[i]] <- int_buffer[as.character(sf::st_geometry_type(int_buffer)) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
    int_target <- sf::st_intersection(sf_mfe_prov, target_polygon) |>
      na.omit()
    sf_mfe_target_list[[i]] <- int_target[as.character(sf::st_geometry_type(int_target)) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
    if(verbose) cli::cli_progress_step(paste0("Added ", nrow(int_target)," MFE polygons and ", nrow(int_buffer)," buffer MFE polygons from province ", prov))
    
  }
  rm(sf_mfe_prov)
  sf_mfe_target <- dplyr::bind_rows(sf_mfe_target_list)
  sf_mfe_buffer <- dplyr::bind_rows(sf_mfe_buffer_list)
  rm(sf_mfe_buffer_list)
  rm(sf_mfe_target_list)
  gc()
  
  target_polygon_vect <- terra::vect(target_polygon)
  sf_mfe_target_vect <- terra::vect(sf_mfe_target)
  if(forests_only) {
    if(is.null(target_raster)) {
      if(verbose) cli::cli_progress_step(paste0("Rasterize forest areas at ", res ,"m resolution"))
      target_raster <-terra::rast(terra::ext(sf_mfe_target_vect), resolution = c(res,res), crs = crs_out)
    } else {
      target_raster <- target_raster
      if(fit_raster_extent) target_raster <- terra::crop(target_raster, terra::ext(sf_mfe_target_vect))
    }
    if(verbose) cli::cli_progress_step(paste0("Create sf object with forest locations at pixel locations"))
    sf_out <- target_raster |> 
      terra::as.points() |>
      terra::intersect(sf_mfe_target_vect) |>
      sf::st_as_sf()
    sf_out <- sf_out[,"geometry", drop = FALSE]
    rm(sf_mfe_target_vect)
  } else {
    if(is.null(target_raster)) {
      if(verbose) cli::cli_progress_step(paste0("Rasterize target area at ", res ,"m resolution"))
      target_raster <-terra::rast(terra::ext(target_polygon_vect), resolution = c(res,res), crs = crs_out)
    } else {
      target_raster <- target_raster
      if(fit_raster_extent) target_raster <- terra::crop(target_raster, terra::ext(target_polygon_vect))
    }
    if(verbose) cli::cli_progress_step(paste0("Create sf object with locations at pixel locations"))
    sf_out <- target_raster |> 
      terra::as.points() |>
      terra::intersect(target_polygon_vect) |>
      sf::st_as_sf()
    sf_out <- sf_out[,"geometry", drop = FALSE]
  }
  
  if(verbose) cli::cli_progress_step(paste0("Add topography to sf (and filter locations with missing topography)"))
  dem <- NULL
  for(prov in touched_provinces) {
    dem_prov <- terra::rast(paste0(emf_dataset_path, "Topography/Spain/PNOA_MDT25_PROVINCES_ETRS89/PNOA_MDT25_P", 
                              prov ,"_ETRS89_H", 
                              province_utm_fuses[as.numeric(prov)], ".tif")) # Same number as province
    # Merge DEM
    if(is.null(dem)) {
      dem <- dem_prov
    } else {
      dem <- terra::merge(dem, dem_prov)
    }
  }
  rm(dem_prov)
  
  if(spatial_aggregation) { # Aggregate from 25 to the output resolution (finer DEM is used for imputation)
    dem_fact <- ceiling(res/25)
    dem_agg <- terra::aggregate(dem, fact = dem_fact, fun = "mean", na.rm = TRUE)
    sf_out <- medfateland::add_topography(sf_out, dem = dem_agg, progress = FALSE) |>
      medfateland::check_topography(missing_action = "filter", verbose = FALSE)
  } else {
    sf_out <- medfateland::add_topography(sf_out, dem = dem, progress = FALSE) |>
      medfateland::check_topography(missing_action = "filter", verbose = FALSE)
  }

  if(forests_only) {
    sf_out$land_cover_type <- "wildland"  
    if(verbose) cli::cli_progress_step(paste0("Define forest land cover for ", nrow(sf_out) , " locations"))
  } else {
    sf_out$land_cover_type <- NA
    lc_thesaurus <- readxl::read_xlsx("data-raw/LandCover_CODIIGE.xlsx")
    for(prov in touched_provinces) {
      file_lcm <- paste0(emf_dataset_path, "LandCover/Spain/SIOSE_2014/SIOSE_2014_PROVINCES_H30_GPKG/", 
                         "SIOSE_", prov ,"_H30_2014.gpkg")
      if(file.exists(file_lcm)) {
        if(verbose) cli::cli_progress_step(paste0("Reading land cover for ", prov))
        lcm_prov <- terra::vect(file_lcm)
        lcm_prov_map <- lcm_prov |>
          dplyr::select(CODIIGE) |>
          dplyr::left_join(lc_thesaurus, by = "CODIIGE") |>
          dplyr::select(MEDFATELAND) |>
          terra::rasterize(target_raster, field = "MEDFATELAND")
        
        lc<-terra::extract(lcm_prov_map, terra::vect(sf_out))$MEDFATELAND
        sf_out$land_cover_type[!is.na(lc)] <- as.character(lc[!is.na(lc)])
      }
    }
    
    # Set to agriculture NA to later replace wildland from MFE
    sf_out$land_cover_type[is.na(sf_out$land_cover_type)] <- "agriculture"

    ## REPLACE land cover type from land cover map with forests for MFE polygons
    for_rast <- !is.na(terra::rasterize(sf_mfe_target_vect, target_raster, field = "Class"))
    sf_out$land_cover_type[terra::extract(for_rast, sf_out)$Class] <- "wildland"
    if(verbose) cli::cli_progress_step(paste0("Define forest land cover for ", sum(terra::extract(for_rast, sf_out)$Class) , " locations"))
  }
  
  if(verbose) cli::cli_progress_step(paste0("Load ", ifn_imputation_source, " imputation source(s)"))
  sf_nfi_list <- vector("list", length(touched_provinces))
  for(i in 1:length(touched_provinces)) {
    prov <- touched_provinces[i]
    ifn_file <- paste0(emf_dataset_path, "ForestInventories/IFN_medfateland/medfateland_",
                       tolower(ifn_imputation_source), "_",prov,"_soilmod_WGS84.rds")
    if(file.exists(ifn_file))  {
      sf_nfi_prov <- readRDS(ifn_file)|>
        sf::st_as_sf() |>
        sf::st_transform(crs_out)
      sf_nfi_prov <- sf::st_intersection(sf_nfi_prov, target_buffer)
      sf_nfi_list[[i]] <- sf_nfi_prov
    }
  }
  sf_nfi <- dplyr::bind_rows(sf_nfi_list) |>
    sf::st_as_sf() |>
    medfateland::check_topography(missing_action = "filter", verbose = FALSE)|>
    medfateland::check_forests(missing_action = "filter", SpParams = traits4models::SpParamsES, verbose = FALSE)
  
  if(verbose) cli::cli_progress_step(paste0("Forest imputation from ", nrow(sf_nfi), " forest plots"))
  forest_map <- terra::vect(sf_mfe_buffer)
  sf_out <- medfateland::impute_forests(sf_out, sf_fi = sf_nfi, dem = dem, forest_map = forest_map, progress = FALSE)
  rm(dem)

  # Fill missing (missing tree or shrub codes should be dealt with before launching simulations)
  if(verbose) cli::cli_progress_step(paste0("Check missing forests"))
  sf_out <- medfateland::check_forests(sf_out, default_forest = medfate::emptyforest(), verbose = FALSE) 
  
  if(height_correction) {
    if(verbose) cli::cli_progress_step(paste0("Load vegetation height map"))
    height_map <- NULL
    height_fact  <- ceiling(res/25)
    for(i in 1:length(touched_provinces)) {
      height_map_prov <- terra::rast(paste0(emf_dataset_path,"RemoteSensing/Spain/CanopyHeight/PNOA_NDSMV_1Cob_PROVINCES_ETRS89/PNOA_NDSMV_cm_P",
                                       touched_provinces[i],"_ETRS89H30_25m.tif")) 
      if(spatial_aggregation) height_map_prov <- terra::aggregate(height_map_prov, fact = height_fact, fun = "median", na.rm = TRUE)
      if(is.null(height_map)) {
        height_map <- height_map_prov
      } else {
        height_map <- terra::merge(height_map, height_map_prov)
      }
    }
    # Modify forest height
    if(verbose) cli::cli_progress_step(paste0("Correct tree height"))
    sf_out <- medfateland::modify_forest_structure(x = sf_out, structure_map =  height_map,
                                                   variable = "mean_tree_height", map_var = names(height_map)[1], 
                                                   progress = FALSE)
    rm(height_map)
    rm(height_map_prov)
  }
  
  if(biomass_correction) {
    if(verbose) cli::cli_progress_step(paste0("Correct forest aboveground tree biomass"))
    biomass_map <- terra::rast(paste0(emf_dataset_path,"RemoteSensing/Spain/CanopyBiomass/CanopyBiomass_Su2025/CanopyBiomass_2021.tif"))
    biomass_fact <- ceiling(res/50)
    if(spatial_aggregation) biomass_map <- terra::aggregate(biomass_map, fact = biomass_fact, fun = "median", na.rm = TRUE)
    r_biomass_map <- terra::resample(biomass_map, target_raster) # Change CRS
    sf_out <- medfateland::modify_forest_structure(x = sf_out, structure_map = r_biomass_map,
                                                   var = "aboveground_tree_biomass", map_var = "CanopyBiomass_2021",
                                                   biomass_function = IFNallometry::IFNbiomass_medfate,
                                                   biomass_arguments = list(fraction = "aboveground",level = "stand"),
                                                   SpParams = traits4models::SpParamsES,
                                                   progress = FALSE)
    rm(biomass_map)
    rm(r_biomass_map)
  }
  
  if(verbose) cli::cli_progress_step(paste0("Read soil data from SoilGrids2.0 for ", nrow(sf_out), " locations."))
  soilgrids_path = paste0(emf_dataset_path, "Soils/Global/SoilGrids/Spain/")
  sf_out <- medfateland::add_soilgrids(sf_out, soilgrids_path = soilgrids_path, progress = FALSE)
  if(verbose) cli::cli_progress_step(paste0("Fill missing soil data with defaults"))
  sf_out <- medfateland::check_soils(sf_out,  missing_action = "default", 
                        default_values = c(clay = 25, sand = 25, bd = 1.5, rfc = 25), verbose = FALSE)
  if(verbose) cli::cli_progress_done()
  
  if(soil_correction) {
    if(verbose) cli::cli_progress_step(paste0("Modify soil depth using data from Shangguan et al. 2017"))
    # Censored soil depth (cm)
    bdricm<- terra::rast(paste0(emf_dataset_path, "Soils/Global/SoilDepth_Shangguan2017/BDRICM_M_250m_ll.tif"))
    # Probability of bedrock within first 2m [0-100]
    bdrlog <- terra::rast(paste0(emf_dataset_path, "Soils/Global/SoilDepth_Shangguan2017/BDRLOG_M_250m_ll.tif"))
    # Absolute depth to bedrock (cm)
    bdticm <- terra::rast(paste0(emf_dataset_path, "Soils/Global/SoilDepth_Shangguan2017/BDTICM_M_250m_ll.tif"))
    x_vect <- terra::vect(sf::st_transform(sf::st_geometry(sf_out), terra::crs(bdricm)))
    x_ext <- terra::ext(x_vect)
    bdricm <- terra::crop(bdricm, x_ext, snap = "out")
    bdrlog <- terra::crop(bdrlog, x_ext, snap = "out")
    bdticm <- terra::crop(bdticm, x_ext, snap = "out")
    # Soil depth in MEDFATE units
    soil_depth_mm <- (bdricm$BDRICM_M_250m_ll*10)*(1 - (bdrlog$BDRLOG_M_250m_ll/100))
    # Bed rock in MEDFATE units
    depth_to_bedrock_mm <- bdticm*10
    # Modify soils
    sf_out <- medfateland::modify_soils(sf_out, soil_depth_map = soil_depth_mm, depth_to_bedrock_map = depth_to_bedrock_mm,
                                        progress = FALSE)
  }
  if((!forests_only) && river_network) {
    rivers_a <-terra::vect(paste0(emf_dataset_path, "Hydrography/Spain/RedHidrografica/Rios_Pfafs/A_RiosCompletosv2.shp"))
    rivers_m <-terra::vect(paste0(emf_dataset_path, "Hydrography/Spain/RedHidrografica/Rios_Pfafs/M_RiosCompletosv2.shp"))
    rivers <- c(rivers_a, rivers_m)
    rivers_rast <- !is.na(terra::rasterize(terra::vect(rivers), target_raster, field = "OBJECTID"))
    sf_out$channel <- terra::extract(rivers_rast, sf_out, ID = FALSE)$OBJECTID
  }
  target_raster$value <- TRUE
  return(list(sf = sf_out, r = target_raster))
}
