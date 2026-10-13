# Summarizes forest and soil for each location over initialized landscapes in Spanish provinces
library(medfateland)

summarize_spanish_landscape <- function(province_code = "01", res = 500, SpParams = traits4models::SpParamsES, progress = FALSE, test = FALSE) {
  medfateland_sf <- readRDS(paste0("data/spain_forests_provinces_", res, "m/medfateland_", province_code, "_sf_", res,"m.rds"))
  medfateland_sf <- sf::st_as_sf(medfateland_sf)
  if(test) medfateland_sf <- medfateland_sf[1:10,]
  summary_forest_df <- function(object, SpParams, ...) {
    df <- as.data.frame(summary.forest(object, SpParams, ...))
    df[["DominantTreeSpecies"]] <- medfate::stand_dominantTreeSpecies(object,SpParams)
    return(df)
  }
  out_forest_sf <- medfateland::landscape_summary(medfateland_sf, "forest", summary_forest_df, SpParams, progress = progress)|>
    tidyr::unnest("summary") 
  summary_soil_df <-function(object, ...) {
    df <- data.frame(FieldCapacity_mm = sum(soil_waterSAT(soil(object),model = "VG")),
                     Saturation_mm = sum(soil_waterSAT(soil(object),model = "VG")),
                     Extractable_mm = sum(soil_waterExtractable(soil(object),model = "VG")))
    return(df)
  }
  out_soil_sf <- medfateland::landscape_summary(medfateland_sf, "soil", summary_soil_df, progress = progress)|>
    tidyr::unnest("summary") 
  out_sf <- out_forest_sf |>
    sf::st_join(out_soil_sf) |>
    dplyr::mutate(province = province_code,
                  id = medfateland_sf$id) |>
    dplyr::relocate(province , .after = "geometry")|>
    dplyr::relocate(id , .after = "province")
  return(out_sf)
}

res <- 500
provinces <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
               as.character(11:50))

overwrite <- TRUE
for(province_code in provinces) {
  out_file <- paste0("data/spain_forests_provinces_",res,"m_summaries/medfateland_", province_code, "_sf_", res,"m_summary.rds")
  if(!file.exists(out_file) || overwrite) {
    cli::cli_h1(paste0("Processing province: ", province_code))
    out_sf <- summarize_spanish_landscape(province_code, res, test = FALSE)
    saveRDS(out_sf, file = out_file)
  }
}