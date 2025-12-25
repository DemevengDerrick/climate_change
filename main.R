#################################################################################################
#                                                                                               #
#                                CLIMATE CHANGE PROJECT                                         #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  dplyr,
  stringr,
  tidyr,
  sf,
  ggplot2,
  geodata,
  terra,
  ggspatial,
  openxlsx
)

# INPUT VARIABLES ---------------------------------------------------------
ctry_code <- "NGA"
flood_dir <- "input/flood_layers_RP100/"
flood_tiles_dir <- "input/flood_tiles/tile_extents.geojson"
admin0_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"
admin1_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp"
admin2_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM2/geoBoundariesCGAZ_ADM2.shp"
pop_dir <- "input/pop/"

# CREATE DATAFRAME FOR STORING INDICATORS PER YEAR
indicators <- tibble::tibble(
  id = character(),
  indicator.name = character(),
  indicator.code = character(),
  country.code = character(),
  admin.level = integer(),
  admin.code = character(),
  year = integer(),
  total.pop = numeric(),
  pop.exposed = numeric(),
  perc.pop.exposed = numeric()
)

# LOAD DATA ---------------------------------------------------------------
pop_files <- list.files(path = pop_dir, full.names = T)

for(pop_file in pop_files){

  bname <- stringr::str_remove(basename(pop_file), "\\.tif$") # extract file basename
  year <- str_extract(bname, "\\d{4}")

  # vectors
  flood_tiles <- sf::read_sf(flood_tiles_dir) # flood tiles
  admin0 <- sf::read_sf(admin0_dir)
  admin1 <- sf::read_sf(admin1_dir)
  admin2 <- sf::read_sf(admin2_dir)

  # rasters
  pop_15_49 <- terra::rast(pop_file)

  # DATA TRANSFORMATION -----------------------------------------------------

  # i) filter admin0 to the country of interest
  ctry_admin0 <- admin0 |>
    dplyr::filter(shapeGroup == ctry_code)

  # ii) intersect the country with the flood tiles
  ctry_admin0  <- sf::st_make_valid(ctry_admin0)
  flood_tiles  <- sf::st_make_valid(flood_tiles)

  ctry_admin0 <- sf::st_transform(ctry_admin0, sf::st_crs(flood_tiles))
  ctry_tiles <- sf::st_intersects(ctry_admin0, flood_tiles)
  idx <- unlist(ctry_tiles)

  # iii) query the flood tiles
  # a) Pull tile codes
  ctry_tile_codes <- flood_tiles[idx, ] |>
    dplyr::mutate(
      tile_name = paste0("ID", id, "_", name)
    ) |>
    pull(tile_name)

  # b) Select the right tiles
  all_files <- list.files(flood_dir, full.names = TRUE)

  selected_files <- all_files[
    stringr::str_detect(
      basename(all_files),
      paste(ctry_tile_codes, collapse = "|")
    )
  ]

  selected_files <- selected_files[
    stringr::str_detect(selected_files, "_RP100_depth\\.tif$")
  ]

  # iv) Mosaic the queried tiles
  vrt_path <- file.path("output/vrt_flood/", paste0(ctry_code, "_RP100_depth.vrt"))
  dir.create(dirname(vrt_path), recursive = TRUE, showWarnings = FALSE)

  terra::vrt(selected_files, vrt_path, overwrite = TRUE)

  rp100 <- terra::rast(vrt_path)

  if (!terra::same.crs(rp100, pop_15_49)) {
    # keep flood as binary-nearest when projecting
    rp100 <- terra::project(rp100, pop_15_49, method = "near")
  }

  # v) Reclasify flood zones into 0 : No flood, and 1: flood depth >= 0.1
  rp100_binary <- terra::ifel(rp100 >= 0.1, 1, 0)

  # vi) Clip the pop and flood raster to the extend of country
  admin0_v <- terra::vect(ctry_admin0) # covert the sf polygon to a spatvector polygon

  rp100_binary_crop <- terra::crop(rp100_binary, admin0_v) # crop the raster
  rp100_binary_clip <- terra::mask(rp100_binary_crop, admin0_v) # mask the raster

  pop_15_49_crop <- terra::crop(pop_15_49, admin0_v) # crop the raster
  pop_15_49_clip <- terra::mask(pop_15_49_crop, admin0_v) # mask the raster

  # vii) Aggregate and Align to 1km the flood raster based on the pop raster
  if (!terra::same.crs(rp100_binary_clip, pop_15_49_clip)) { # Ensure rasters are in same CRS BEFORE factor math
    rp100_binary_clip <- terra::project(rp100_binary_clip, pop_15_49_clip, method = "near")
  }

  rp100_binary_full <- terra::ifel(
    is.na(rp100_binary_clip),
    0,
    rp100_binary_clip
  )

  fact <- floor(res(pop_15_49_clip) / res(rp100_binary_full)) # 2D aggregation factor (x,y)
  if (any(fact < 2)) stop("fact < 2: flood is not finer than pop (or CRS mismatch).")

  rp100_frac_1km <- terra::aggregate( # Aggregate 100m binary -> 1km fraction
    rp100_binary_full,
    fact = fact,
    fun = mean,
    na.rm = TRUE
  )

  rp100_frac_1km <- terra::resample(rp100_frac_1km, pop_15_49_clip, method = "near") # Align to pop grid (no averaging needed here)

  terra::global(rp100_frac_1km, range, na.rm = TRUE) # QC: should be within [0,1] and not just 0/1

  # viii) Compute population exposed
  pop_15_49_exposed <- rp100_frac_1km*pop_15_49_clip
  names(pop_15_49_exposed) <- "female_pop_15_49_exposed"
  names(pop_15_49_clip) <- "female_pop_15_49"

  # ix) Export Raster of population exposed
  out_pop_15_49_flood_exposed <- sprintf("output/flood_exposure/%s_pop_15_49_flood_exposed.tif", ctry_code)
  out_pop_15_49_clip <- sprintf("output/flood_exposure/%s_pop_15_49_clip.tif", ctry_code)
  out_rp100_frac_1km <- sprintf("output/flood_exposure/%s_rp100_frac_1km.tif", ctry_code)
  out_rp100_binary_clip <- sprintf("output/flood_exposure/%s_rp100_binary_clip.tif", ctry_code)

  terra::writeRaster(
    pop_15_49_exposed, 
    out_pop_15_49_flood_exposed,
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  )

  terra::writeRaster(
    pop_15_49_clip, 
    out_pop_15_49_clip,
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  )

  terra::writeRaster(
    rp100_frac_1km, 
    out_rp100_frac_1km,
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  )

  terra::writeRaster(
    rp100_binary_clip, 
    out_rp100_binary_clip,
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  )

  # x) Compute Zonal statistics at admin0, admin1 and admin2
  admin1_v <- admin1 |> # filter to ctry code and convert to spatvector compatible with terra
    dplyr::filter(shapeGroup == ctry_code) |>
    terra::vect()

  admin2_v <- admin2 |> # filter to ctry code and convert to spatvector compatible with terra
    dplyr::filter(shapeGroup == ctry_code) |>
    terra::vect()

  terra::zonal( # stats admin0
    pop_15_49_exposed, 
    admin0_v, 
    fun=sum,
    na.rm=T,
  )

  # admin1
  terra::zonal( # stats admin1
    pop_15_49_exposed, 
    admin1_v, 
    fun=sum,
    na.rm=T,
  )

  # admin2
  admin2_v_stat <- terra::zonal( # stats admin2
      pop_15_49_exposed, 
      admin2_v, 
      fun=sum,
      na.rm=T,
      as.polygons=T
    )

  admin2_v_stat <- terra::zonal( # stats admin2
      pop_15_49_clip, 
      admin2_v_stat, 
      fun=sum,
      na.rm=T,
      as.polygons=T
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      perc_pop_15_49_exposed = (female_pop_15_49_exposed/female_pop_15_49)*100
    )
  
  # intermidiary data frame to store intermidiary data
  year_df <- tibble::tibble(
    id               = NA,          # or any ID
    indicator.name   = "Women 15–49 exposed to RP100 floods",
    indicator.code   = "wraf100",
    country.code     = admin2_v_stat$shapeGroup,
    admin.code       = admin2_v_stat$shapeID,       # or ADM2 code
    admin.level       = as.integer(2),
    year             = as.integer(year),
    total.pop        = admin2_v_stat$female_pop_15_49,
    pop.exposed      = admin2_v_stat$female_pop_15_49_exposed,
    perc.pop.exposed = admin2_v_stat$perc_pop_15_49_exposed
  )

  indicators <- dplyr::bind_rows(indicators, year_df)
}

indicators$id = 1:nrow(indicators) # update id
openxlsx::write.xlsx(indicators, paste0("output/zonal_stats/",ctry_code,"_indicators.xlsx")) # export to excel

# join indicators to shapefiles for mapping
admin2_join <- admin2 |>
  dplyr::filter(shapeGroup == ctry_code) |>
  dplyr::left_join(
    indicators,
    by = c("shapeID" = "admin.code")
  )

# DATA VISUALIZATION ------------------------------------------------------
ggplot2::ggplot(data = admin2_join) +
  ggplot2::geom_sf(aes(fill=perc.pop.exposed)) +
  ggplot2::scale_fill_continuous(palette = "OrRd") +
  ggplot2::labs(
    fill = "percentage (%)",
    title = paste("Women of Reproductive Age Exposed to River Floods in", ctry_code),
    caption = paste(
      "Disclaimer: The boundaries and names shown and the designations used on \n",
      "this map do not imply official endorsement or acceptance by the United Nations.\n\n",
      "Data Source: WorldPop, Copernicus, and GeoBoundaries\n\n",
      "Date:", today()
    )
  ) +
  ggplot2::facet_wrap(~year) +
  # ---- SCALE BAR ----
  ggspatial::annotation_scale(
    location = "bl",
    height = unit(0.1, "cm"),
    width_hint = 0.3,
    text_cex = 0.6,
    line_width = 0.5,
    unit_category = "metric"
  ) +
  # ---- NORTH ARROW ----
  ggspatial::annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = unit(0.7, "cm"),
    width  = unit(0.7, "cm"),
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = 8,
      line_width = 0.5
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 12),
    legend.position = "bottom"
  )

# save the plot
ggplot2::ggsave(
  filename = paste0("output/flood_maps/", ctry_code, "_river_flood_exposure.jpg"),
  scale = 2
)







