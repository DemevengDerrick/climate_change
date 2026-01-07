#################################################################################################
#                                                                                               #
#                             CONFLICT EXPOSURE ANALYSIS                                        #
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
  openxlsx,
  scales,
  lwgeom,
  readr,
  ggrepel
)

# INPUT VARIABLES ---------------------------------------------------------
conflict_data_dir <- "input/acled/ACLED Data_2026-01-01.csv"
admin0_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"

# LOAD DATA ---------------------------------------------------------------
conflict_data <- readr::read_csv(file = conflict_data_dir, col_types = list(iso = 'c'))
admin0 <- sf::read_sf(admin0_dir)

# DATA TRANSFORMATION -----------------------------------------------------
conflict_count_by_country <- conflict_data |>
  dplyr::group_by(country) |>
  dplyr::summarise(
    num.of.disorder = n(),
    num.fatalities = sum(fatalities),
    exposed.pop.best = sum(population_best)
  )

# DATA VISUALIZATION ------------------------------------------------------
ggplot2::ggplot(data = conflict_count_by_country |> filter(exposed.pop.best > 0)) +
  ggplot2::geom_point(
    aes(size = num.fatalities, 
        y = num.of.disorder, 
        x = exposed.pop.best), 
    shape = 21,
    fill = NA
  ) +
  ggrepel::geom_text_repel(
    aes(y = num.of.disorder, 
        x = exposed.pop.best, 
        label = country,
        color = num.fatalities >= 40
      ), 
    point.padding = 0.3,
    box.padding = 0.5,
    min.segment.length = 0
  ) +
  ggplot2::scale_size(
    range = c(4, 15)   # increase visual size
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::theme_minimal(base_size = 18)


ggplot2::ggplot(data = conflict_data) +
  ggplot2::geom_point(aes(y = latitude, x = longitude), shape = 23, fill = "red") +
  ggplot2::geom_sf(data = admin0, fill = NA) +
  #ggplot2::facet_wrap(~sub_event_type) +
  ggplot2::theme_void()

ggplot2::ggsave()