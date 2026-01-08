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
  tibble,
  ggspatial,
  openxlsx,
  scales,
  lwgeom,
  mapview,
  cli,
  patchwork
)

# VARIABLES -------------------------------------------------------------
pop_folder_dir <- "input/pop/gambia/"
ctry_code <- "GMB"
flood_raster_folder_dir <- "input/flood_layers_RP100/"
flood_tiles_dir <- "input/flood_tiles/tile_extents.geojson"
admin0_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"

# HELPER FUNCTIONS ------------------------------------------------------

extract_pop_meta <- function(file_path) {
  f <- basename(file_path)

  m <- str_match(
    f,
    # country_sex(_age)?_year_CN_resolution_....
    "^[a-z]{3}_((?:[fmt])|(?:T_[FM]))(?:_([0-9]{2}))?_([0-9]{4})_[A-Z]{2}_([0-9]+m)_"
  )

  tab <- tibble(
    file       = file_path,
    sex_code   = m[,2],
    age_group  = m[,3],          # NA for T_F / T_M (no age in filename)
    year       = as.integer(m[,4]),
    resolution = m[,5]
  ) |>
    mutate(
      sex = case_when(
        sex_code == "f"   ~ "female",
        sex_code == "m"   ~ "male",
        sex_code == "t"   ~ "total",
        sex_code == "T_F" ~ "female_total",
        sex_code == "T_M" ~ "male_total",
        T              ~ NA_character_
      )
    )
  
  return(tab)
}

extract_pop_meta_multiple <- function(file_paths){
  pop_rasters_tab <- tibble::tibble()

  for(pop_raster in file_paths){
    tab <- extract_pop_meta(pop_raster)
    pop_rasters_tab <- dplyr::bind_rows(pop_rasters_tab, tab)
  }

  return(pop_rasters_tab)
}

flood_tiles_mosaic <- function(shp, flood_tiles_shp, flood_ras_dir){
  # ii) intersect the country with the flood tiles
  shp  <- sf::st_make_valid(shp)
  flood_tiles_shp  <- sf::st_make_valid(flood_tiles_shp)

  shp <- sf::st_transform(shp, sf::st_crs(flood_tiles_shp))
  ctry_tiles <- sf::st_intersects(shp, flood_tiles_shp)
  idx <- unlist(ctry_tiles)
  print(idx)

  # iii) query the flood tiles
  # a) Pull tile codes
  ctry_tile_codes <- flood_tiles_shp[idx, ] |>
    dplyr::mutate(
      tile_name = paste0("ID", id, "_", name)
    ) |>
    dplyr::pull(tile_name)

  print(ctry_tile_codes)

  # b) Select the right tiles
  all_files <- list.files(flood_ras_dir, full.names = T)

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
  vrt_path <- file.path("output/vrt_flood/flood_RP100_depth.vrt")

  dir.create(dirname(vrt_path), recursive = T, showWarnings = F)

  terra::vrt(selected_files, vrt_path, overwrite = T)

  rp100 <- terra::rast(vrt_path)

  return(rp100)
}

flood_exposure <- function(shp, pop_ras, flood_ras){
  # ii) compute Exposure
  # a) align CRS
  if(class(shp)[1] == "sf"){
    shp <- terra::vect(shp) # Convert sf to spatVector used by terra
  }
  shp <- terra::project(shp, terra::crs(pop_ras))

  # b) Crop and Mask pop and flood to admin boundary
  pop_c   <- terra::crop(pop_ras, shp) |> terra::mask(shp)
  flood_c <- terra::crop(flood_ras, shp) |> terra::mask(shp)

  # c) Align flood to pop raster
  flood_a <- terra::resample(flood_c, pop_c, method = "near")

  # d) Reclassifiy flood raster to binary
  flood_bin <- terra::ifel(flood_a >= 0.1, 1, 0)

  # e) Exposure
  exposure <- pop_c * flood_bin

  names(exposure) <- "pop_exposed"
  names(pop_c) <- "pop_tot"

  return(list(pop_c, exposure))
}

# LOAD DATA -------------------------------------------------------------
pop_rasters <- dir(pop_folder_dir, recursive = T, full.names = T) # all pop raster files
flood_tiles <- sf::read_sf(flood_tiles_dir) # flood tiles
admin0 <- sf::read_sf(admin0_dir)

# DATA TRANSFORMATION ---------------------------------------------------

# 0) pop raster meta dataframe
pop_raster_meta <- extract_pop_meta_multiple(pop_rasters) # pop meta data dataframe

# i) filter admin0 to the country of interest
ctry_admin0 <- admin0 |>
  dplyr::filter(shapeGroup == ctry_code)

# ii) Mosaic flood rasters
rp100 <- flood_tiles_mosaic(
  shp = ctry_admin0, 
  flood_tiles_shp = flood_tiles, 
  flood_ras_dir = flood_raster_folder_dir
)

# iii) compute Exposure
exposure_stat <- pop_raster_meta
exposure_stat$pop_exposed <- 0.0
exposure_stat$pop_tot <- 0.0

i <- 1
pb <- cli::cli_progress_bar("Processing", total = nrow(exposure_stat))

for(pop_raster in exposure_stat$file){
  pop_raster <- terra::rast(pop_raster)
  pops <- flood_exposure(
    shp = ctry_admin0, 
    pop_ras = pop_raster, 
    flood_ras = rp100
  )

  pop_exposed <- pops[[2]]
  pop_tot <- pops[[1]]

  zonal_stat_exposed <- terra::zonal( # stats admin2
      pop_exposed, 
      ctry_admin0 |> terra::vect(), 
      fun=sum,
      na.rm=T,
      as.polygons=F
    )
  
  zonal_stat_pop_tot <- terra::zonal( # stats admin2
      pop_tot, 
      ctry_admin0 |> terra::vect(), 
      fun=sum,
      na.rm=T,
      as.polygons=F
    )
  
  exposure_stat$pop_exposed[i] <- zonal_stat_exposed$pop_exposed[1]
  exposure_stat$pop_tot[i] <- zonal_stat_pop_tot$pop_tot[1]
  i <- i+1

  cli::cli_progress_update(pb, i)
}

cli::cli_progress_done(pb)

# iv) prepare data for pyramid
exposure_stat_m_f <- 
  exposure_stat |>
    dplyr::filter(sex %in% c("male","female")) |>
    dplyr::mutate(
      perc_exposed = round((pop_exposed/pop_tot)*100, 1),
      pop_exposed = case_when(
        sex == "female" ~ -pop_exposed,
        T ~ pop_exposed
      ),
      pop_tot = case_when(
        sex == "female" ~ -pop_tot,
        T ~ pop_tot
      ),
      perc_exposed = case_when(
        sex == "female" ~ -perc_exposed,
        T ~ perc_exposed
      )
  )

# VISUALIZATION ---------------------------------------------------------

# pop pyramid of exposure
pyramid_count <- 
  ggplot2::ggplot(data = exposure_stat_m_f) +
  #ggplot2::geom_bar(aes(x = pop_tot, y = age_group), stat = "identity", color = "white", fill = "grey") +
  ggplot2::geom_bar(aes(x = pop_exposed, y = age_group, fill = sex), stat = "identity", width = 0.9, color = "white") +
  ggplot2::labs(
    title = "Flood Exposure by Age Group (Count)",
    caption = "Data Source: WorldPop, Copernicus",
    y = "Age Group",
    x = "Population Count (in thousands)"
  ) +
  ggplot2::theme_minimal(base_size = 18) +
  ggplot2::theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank()
  ) +
  ggplot2::geom_text(
        aes(
          x = pop_exposed,
          y = age_group,
          label = abs(round(pop_exposed/1000,1)),
          hjust = ifelse(pop_exposed < 0, 1.1, -0.1)
        ),
        size = 2.8,
        color = "black"
  ) +
  ggplot2::facet_wrap(~year, ncol = 4, nrow = 1)

pyramid_perc <- 
  ggplot2::ggplot(data = exposure_stat_m_f) +
  #ggplot2::geom_bar(aes(x = perc_exposed, y = age_group, fill = sex), stat = "identity", width = 0.9, color = "white") +
  ggplot2::geom_bar(aes(x = perc_exposed, y = age_group, fill = sex), stat = "identity", width = 0.9, color = "white") +
  ggplot2::labs(
    title = "Flood Exposure by Age Group (Percentage)",
    caption = "Data Source: WorldPop, Copernicus",
    y = "Age Group",
    x = "% Population"
  ) +
  ggplot2::theme_minimal(base_size = 16) +
  ggplot2::theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank()
  ) +
  ggplot2::geom_text(
        aes(
          x = perc_exposed,
          y = age_group,
          label = abs(perc_exposed),
          hjust = ifelse(perc_exposed < 0, 1.1, -0.1)
        ),
        size = 2.8,
        color = "black"
  ) +
  ggplot2::facet_wrap(~year, ncol = 4, nrow = 1)

pyramid_count / pyramid_perc

ggplot2::ggsave(filename = "output/flood_maps/exposure_pyramid.jpg", scale = 1.2, width = 15, height = 10)

# interactive map
mapview::mapviewOptions(fgb = FALSE)  # helps if you have many layers

m1 <- mapview::mapview(pop_c,  layer.name = "Population", maxpixels = 2e6)
m2 <- mapview::mapview(flood_a, layer.name = "Flood mask", maxpixels = 2e6)
m3 <- mapview::mapview(flood_bin, layer.name = "Flood binary", maxpixels = 2e6)
m4 <- mapview::mapview(exposure, layer.name = "Exposure", maxpixels = 2e6)

m1 + m2 + m3 + m4
