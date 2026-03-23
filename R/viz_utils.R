#################################################################################################
#                                                                                               #
#                           VISUALIZATION UTILITY FUNCTIONS                                     #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

# ---------------------------------------------------------------------------
# SHARED CONSTANTS
# ---------------------------------------------------------------------------

.SEX_COLOURS <- c(female = "#c94a7e", both = "#3d7fbf")

.WP_CAPTION <- paste0(
  "Disclaimer: The boundaries and names shown do not imply official endorsement ",
  "or acceptance by the United Nations.\n",
  "Sources: WorldPop (population), Copernicus GLOFAS (flood hazard), GeoBoundaries (admin). ",
  "Date: ", Sys.Date()
)

# Light grey helper for non-data areas
.LAND_FILL  <- "#f2f2f0"
.BORDER_COL <- "grey55"


# ---------------------------------------------------------------------------
# INTERNAL: raster to data frame
# ---------------------------------------------------------------------------

.rast_to_df <- function(ras, name = "value") {
  df        <- terra::as.data.frame(ras, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x", "y", name)
  df
}


# ---------------------------------------------------------------------------
# DIAGNOSTIC MAP — hazard layer
# ---------------------------------------------------------------------------

#' Plot the hazard fraction raster (flood extent)
#'
#' Saves or returns a ggplot map showing the fraction of each 1 km cell that
#' is classified as flooded.  Used in methodology reports.
#'
#' @param flood_frac_ras SpatRaster. Flood fraction per cell (0–1).
#' @param ctry_sf        sf. Country boundary.
#' @param admin1_sf      sf or `NULL`. Admin1 boundaries for context.
#' @param ctry_code      Character. ISO3 code for map title.
#' @param hazard_name    Character. Hazard label (default `"River Flood"`).
#' @param return_period  Integer. Return period (default `100`).
#' @param out_path       Character or `NULL`.
#' @param width          Numeric. Inches (default `7`).
#' @param height         Numeric. Inches (default `6`).
#' @param dpi            Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_plot_hazard <- function(flood_frac_ras, ctry_sf, admin1_sf = NULL,
                             ctry_code, hazard_name = "River Flood",
                             return_period = 100,
                             out_path = NULL, width = 7, height = 6,
                             dpi = 200) {

  df <- .rast_to_df(flood_frac_ras, "flood_frac")
  df <- df[df$flood_frac > 0.01, ]

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL,
                     color = "grey70", linewidth = 0.3) +
    ggplot2::geom_raster(data = df,
                          ggplot2::aes(x = x, y = y, fill = flood_frac * 100)) +
    ggplot2::scale_fill_distiller(
      palette   = "Blues", direction = 1,
      name      = "% cell flooded",
      limits    = c(0, 100),
      labels    = function(x) paste0(x, "%"),
      guide     = ggplot2::guide_colorbar(barwidth = 8, barheight = 0.5)
    ) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA, color = "grey55",
                         linewidth = 0.25)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA, color = "#333333",
                     linewidth = 0.5) +
    ggspatial::annotation_scale(
      location = "bl", height = ggplot2::unit(0.1, "cm"),
      width_hint = 0.3, text_cex = 0.55, line_width = 0.4,
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tl", which_north = "true",
      height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"),
      style  = ggspatial::north_arrow_fancy_orienteering(
        text_size = 7, line_width = 0.4)
    ) +
    ggplot2::labs(
      title    = paste0(hazard_name, " Extent — ", ctry_code),
      subtitle = paste0("1-in-", return_period, "-year flood scenario"),
      caption  = .WP_CAPTION
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption    = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin     = ggplot2::margin(6, 6, 6, 6)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# DIAGNOSTIC MAP — population layer
# ---------------------------------------------------------------------------

#' Plot the total population distribution raster
#'
#' @param pop_ras    SpatRaster. Population counts per cell.
#' @param ctry_sf   sf. Country boundary.
#' @param admin1_sf sf or `NULL`. Admin1 for context.
#' @param ctry_code Character. ISO3 code.
#' @param year      Integer. Population year for subtitle.
#' @param out_path  Character or `NULL`.
#' @param width     Numeric. Inches (default `7`).
#' @param height    Numeric. Inches (default `6`).
#' @param dpi       Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_plot_population <- function(pop_ras, ctry_sf, admin1_sf = NULL,
                                 ctry_code, year = 2020,
                                 out_path = NULL, width = 7, height = 6,
                                 dpi = 200) {

  df <- .rast_to_df(pop_ras, "pop")
  df <- df[df$pop > 0, ]
  df$pop_log <- log10(df$pop + 1)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL,
                     color = "grey70", linewidth = 0.3) +
    ggplot2::geom_raster(data = df,
                          ggplot2::aes(x = x, y = y, fill = pop_log)) +
    ggplot2::scale_fill_distiller(
      palette  = "YlOrRd", direction = 1,
      name     = "Population\n(log\u2081\u2080 scale)",
      breaks   = c(0, 1, 2, 3, 4),
      labels   = c("1", "10", "100", "1,000", "10,000"),
      guide    = ggplot2::guide_colorbar(
        barwidth  = 8, barheight = 0.5,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA, color = "grey55",
                         linewidth = 0.25)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA, color = "#333333",
                     linewidth = 0.5) +
    ggspatial::annotation_scale(
      location = "bl", height = ggplot2::unit(0.1, "cm"),
      width_hint = 0.3, text_cex = 0.55, line_width = 0.4,
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tl", which_north = "true",
      height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"),
      style  = ggspatial::north_arrow_fancy_orienteering(
        text_size = 7, line_width = 0.4)
    ) +
    ggplot2::labs(
      title    = paste0("Population Distribution — ", ctry_code),
      subtitle = paste0("WorldPop ", year, " | 1 km resolution"),
      caption  = .WP_CAPTION
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption    = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin     = ggplot2::margin(6, 6, 6, 6)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# DIAGNOSTIC MAP — hazard × population overlay
# ---------------------------------------------------------------------------

#' Overlay map: population density with flood extent highlighted
#'
#' Shows population as a greyscale background and flood-affected cells in
#' orange/red, illustrating the spatial overlap that drives exposure estimates.
#'
#' @param pop_ras        SpatRaster. Population counts per cell.
#' @param flood_frac_ras SpatRaster. Flood fraction per cell.
#' @param ctry_sf        sf. Country boundary.
#' @param admin1_sf      sf or `NULL`. Admin1 for context.
#' @param ctry_code      Character. ISO3 code.
#' @param hazard_name    Character. Hazard label.
#' @param return_period  Integer. Return period.
#' @param year           Integer. Population year.
#' @param out_path       Character or `NULL`.
#' @param width          Numeric. Inches (default `7`).
#' @param height         Numeric. Inches (default `6`).
#' @param dpi            Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_plot_overlay <- function(pop_ras, flood_frac_ras, ctry_sf,
                              admin1_sf = NULL, ctry_code,
                              hazard_name = "River Flood", return_period = 100,
                              year = 2020,
                              out_path = NULL, width = 7, height = 6,
                              dpi = 200) {

  df_pop   <- .rast_to_df(pop_ras, "pop")
  df_pop   <- df_pop[df_pop$pop > 0, ]
  df_pop$pop_log <- log10(df_pop$pop + 1)

  df_flood <- .rast_to_df(flood_frac_ras, "flood_frac")
  df_flood <- df_flood[df_flood$flood_frac > 0.1, ]

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL,
                     color = NA) +
    ggplot2::geom_raster(data = df_pop,
                          ggplot2::aes(x = x, y = y, fill = pop_log),
                          alpha = 0.85) +
    ggplot2::scale_fill_distiller(
      palette = "Greys", direction = 1,
      name    = "Population\n(log\u2081\u2080 scale)",
      breaks  = c(0, 1, 2, 3, 4),
      labels  = c("1", "10", "100", "1,000", "10,000"),
      guide   = ggplot2::guide_colorbar(
        barwidth  = 5, barheight = 0.4, order = 2,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_raster(data = df_flood,
                          ggplot2::aes(x = x, y = y, fill = flood_frac),
                          alpha = 0.70) +
    ggplot2::scale_fill_distiller(
      palette = "Oranges", direction = 1,
      name    = "Flood fraction",
      breaks  = c(0, 0.25, 0.50, 0.75, 1.00),
      labels  = c("0%", "25%", "50%", "75%", "100%"),
      guide   = ggplot2::guide_colorbar(
        barwidth  = 5, barheight = 0.4, order = 1,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA,
                         color = "grey45", linewidth = 0.2)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA, color = "#333333",
                     linewidth = 0.5) +
    ggspatial::annotation_scale(
      location = "bl", height = ggplot2::unit(0.1, "cm"),
      width_hint = 0.3, text_cex = 0.55, line_width = 0.4,
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tl", which_north = "true",
      height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.7, "cm"),
      style  = ggspatial::north_arrow_fancy_orienteering(
        text_size = 7, line_width = 0.4)
    ) +
    ggplot2::labs(
      title    = paste0("Population in Flood-Prone Areas — ", ctry_code),
      subtitle = paste0(hazard_name, " (RP", return_period, ") overlaid on ",
                        year, " population"),
      caption  = .WP_CAPTION
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption    = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin     = ggplot2::margin(6, 6, 6, 6)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# CHOROPLETH MAP — per hazard, faceted by demographic group
# ---------------------------------------------------------------------------

#' Faceted choropleth map of exposure by demographic group (one file per hazard)
#'
#' Produces a 3-column grid of maps — one panel per demographic group — showing
#' the percentage of each admin zone's population exposed. One output file is
#' generated per hazard, making it easy to add future hazards (coastal flood,
#' heat, aridity) without changing the function.
#'
#' @param admin_sf      sf. Admin2 boundaries joined to indicators. Must
#'   contain `perc.pop.exposed` and `group.label`.
#' @param ctry_sf       sf. Country boundary (admin0) for the outer outline.
#' @param admin1_sf     sf or `NULL`. Admin1 boundaries drawn as thin context
#'   lines inside each panel.
#' @param ctry_code     Character. ISO3 code for the map title.
#' @param hazard_name   Character. Full hazard name for the title / subtitle.
#' @param return_period Integer. Return period for the subtitle.
#' @param year          Integer. Population year for the subtitle.
#' @param out_path      Character or `NULL`. If provided, the plot is saved.
#' @param ncol          Integer. Facet columns (default `3`).
#' @param width         Numeric. Inches (default `14`).
#' @param height        Numeric. Inches (default `16`).
#' @param dpi           Numeric (default `250`).
#'
#' @return ggplot object (invisibly if `out_path` is not NULL).
#' @export
viz_choropleth <- function(admin_sf, ctry_sf, ctry_code,
                            admin1_sf     = NULL,
                            hazard_name   = "River Flood",
                            return_period = 100,
                            year          = 2020,
                            footnote      = NULL,
                            out_path      = NULL,
                            ncol          = 3,
                            width         = 14,
                            height        = 16,
                            dpi           = 250) {

  if ("group.label" %in% names(admin_sf)) {
    present_levels <- WP_GROUP_LEVELS[WP_GROUP_LEVELS %in% admin_sf$group.label]
    admin_sf <- admin_sf |>
      dplyr::mutate(group.label = factor(group.label, levels = present_levels))
    facet_var <- "group.label"
  } else {
    facet_var <- "year"
  }

  p <- ggplot2::ggplot(data = admin_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = perc.pop.exposed),
                     color = NA) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA,
                         color = "white", linewidth = 0.35)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA,
                     color = "#111111", linewidth = 0.55) +
    ggplot2::scale_fill_distiller(
      palette  = "YlOrRd",
      direction = 1,
      name     = "% of group\nexposed",
      limits   = c(0, NA),
      labels   = function(x) paste0(x, "%"),
      na.value = "grey88",
      guide    = ggplot2::guide_colorbar(
        barwidth = 10, barheight = 0.55,
        title.position = "top", title.hjust = 0.5
      )
    ) +
    ggplot2::facet_wrap(
      stats::as.formula(paste("~", facet_var)),
      ncol = ncol
    ) +
    ggspatial::annotation_scale(
      location = "bl", height = ggplot2::unit(0.1, "cm"),
      width_hint = 0.25, text_cex = 0.45, line_width = 0.35,
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tl", which_north = "true",
      height = ggplot2::unit(0.55, "cm"), width = ggplot2::unit(0.55, "cm"),
      pad_x  = ggplot2::unit(0.12, "cm"), pad_y = ggplot2::unit(0.12, "cm"),
      style  = ggspatial::north_arrow_fancy_orienteering(
        text_size = 6, line_width = 0.35)
    ) +
    ggplot2::labs(
      title    = paste0(hazard_name,
                        " Exposure by Demographic Group \u2014 ", ctry_code),
      subtitle = paste0("1-in-", return_period,
                        "-year event  |  Population year: ", year),
      caption  = if (!is.null(footnote))
                   paste0(footnote, "\n", .WP_CAPTION)
                 else .WP_CAPTION
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5,
                                               size = 14, margin = ggplot2::margin(b = 4)),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey35",
                                               size = 10, margin = ggplot2::margin(b = 8)),
      plot.caption    = ggplot2::element_text(hjust = 0, size = 9,
                                               color = "grey45",
                                               margin = ggplot2::margin(t = 6)),
      strip.text      = ggplot2::element_text(face = "bold", size = 9,
                                               margin = ggplot2::margin(b = 3, t = 3)),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      panel.spacing   = ggplot2::unit(0.4, "lines"),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(size = 8),
      legend.text     = ggplot2::element_text(size = 8),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin     = ggplot2::margin(8, 8, 8, 8)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# GROUP COMPARISON BAR CHART
# ---------------------------------------------------------------------------

#' Horizontal bar chart comparing exposure across demographic groups
#'
#' @param ctry_indicators Tibble. Country-level summary.
#' @param ctry_code       Character. ISO3 code.
#' @param hazard_name     Character. Hazard label.
#' @param return_period   Integer. Return period (default `100`).
#' @param out_path        Character or `NULL`.
#' @param width           Numeric. Inches (default `11`).
#' @param height          Numeric. Inches (default `7`).
#' @param dpi             Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_group_bars <- function(ctry_indicators, ctry_code,
                            hazard_name   = "River Flood",
                            return_period = 100,
                            out_path = NULL, width = 11, height = 7,
                            dpi = 200) {

  df <- ctry_indicators |>
    dplyr::mutate(
      group.label = factor(
        group.label,
        levels = dplyr::arrange(ctry_indicators, pop.exposed)$group.label
      )
    )

  p <- ggplot2::ggplot(df,
         ggplot2::aes(y = group.label, x = pop.exposed, fill = sex)) +
    ggplot2::geom_col(width = 0.62, alpha = 0.92) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(perc.pop.exposed, 1), "%")),
      hjust = -0.12, size = 3.4, fontface = "bold", color = "#222222"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.20))
    ) +
    ggplot2::scale_fill_manual(
      values = .SEX_COLOURS,
      labels = c(female = "Female", both = "Both sexes"),
      name   = "Sex"
    ) +
    ggplot2::labs(
      title    = paste0("Population Exposed to ", hazard_name,
                        " (RP", return_period, ") \u2014 ", ctry_code),
      subtitle = paste("Year:", unique(ctry_indicators$year),
                       " | Numbers at bar tips show % of group exposed"),
      x        = "Number of people exposed",
      y        = NULL,
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle      = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption       = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position    = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 11, face = "bold"),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# LOLLIPOP CHART — % exposed by group
# ---------------------------------------------------------------------------

#' Lollipop chart ranking demographic groups by exposure rate
#'
#' @param ctry_indicators Tibble. Country-level summary.
#' @param ctry_code       Character. ISO3 code.
#' @param hazard_name     Character. Hazard label.
#' @param return_period   Integer. Return period (default `100`).
#' @param out_path        Character or `NULL`.
#' @param width           Numeric. Inches (default `10`).
#' @param height          Numeric. Inches (default `6`).
#' @param dpi             Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_exposure_dotplot <- function(ctry_indicators, ctry_code,
                                  hazard_name   = "River Flood",
                                  return_period = 100,
                                  out_path = NULL, width = 10, height = 6,
                                  dpi = 200) {

  df <- ctry_indicators |>
    dplyr::mutate(
      group.label = factor(
        group.label,
        levels = ctry_indicators |>
          dplyr::arrange(perc.pop.exposed) |>
          dplyr::pull(group.label)
      )
    )

  p <- ggplot2::ggplot(df,
         ggplot2::aes(y = group.label, x = perc.pop.exposed, color = sex)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = perc.pop.exposed,
                   y = group.label, yend = group.label),
      color = "grey78", linewidth = 1.0
    ) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(perc.pop.exposed, 1), "%")),
      hjust = -0.50, size = 3.3, color = "#222222", fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    ) +
    ggplot2::scale_color_manual(
      values = .SEX_COLOURS,
      labels = c(female = "Female", both = "Both sexes"),
      name   = "Sex"
    ) +
    ggplot2::labs(
      title    = paste0("Share of Group Exposed to ", hazard_name,
                        " (RP", return_period, ") \u2014 ", ctry_code),
      subtitle = paste("Year:", unique(ctry_indicators$year)),
      x        = "Percentage of group's population in flood-prone areas",
      y        = NULL,
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle      = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption       = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position    = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 11, face = "bold"),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# BUBBLE CHART — group size vs exposure rate
# ---------------------------------------------------------------------------

#' Bubble chart: group total population vs exposure rate
#'
#' @param ctry_indicators Tibble. Country-level summary.
#' @param ctry_code       Character. ISO3 code.
#' @param hazard_name     Character. Hazard label.
#' @param return_period   Integer. Return period (default `100`).
#' @param out_path        Character or `NULL`.
#' @param width           Numeric. Inches (default `10`).
#' @param height          Numeric. Inches (default `7`).
#' @param dpi             Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_exposure_bubble <- function(ctry_indicators, ctry_code,
                                 hazard_name   = "River Flood",
                                 return_period = 100,
                                 out_path = NULL, width = 10, height = 7,
                                 dpi = 200) {

  p <- ggplot2::ggplot(ctry_indicators,
         ggplot2::aes(x = total.pop, y = perc.pop.exposed,
                      size = pop.exposed, color = sex,
                      label = group.label)) +
    ggplot2::geom_point(alpha = 0.78) +
    ggrepel::geom_text_repel(
      size = 3.3, color = "#222222",
      box.padding = 0.45, max.overlaps = 15,
      segment.color = "grey60", segment.size = 0.35
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::scale_size_continuous(
      name = "People exposed", labels = scales::comma, range = c(3, 16)
    ) +
    ggplot2::scale_color_manual(
      values = .SEX_COLOURS,
      labels = c(female = "Female", both = "Both sexes"),
      name   = "Sex"
    ) +
    ggplot2::labs(
      title    = paste0("Group Size vs Exposure Rate \u2014 ", ctry_code),
      subtitle = paste0(hazard_name, " RP", return_period,
                        "  |  Year: ", unique(ctry_indicators$year)),
      x        = "Total group population",
      y        = "% of group exposed to flooding",
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption    = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position = "right",
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# TREND HISTOGRAM — multi-year single-group
# ---------------------------------------------------------------------------

#' Dual-axis trend histogram (multi-year, single group)
#'
#' @param ctry_indicators Tibble. Country-level summary, one row per year.
#' @param indicator_label Character. Group label for titles.
#' @param hazard_name     Character. Hazard label.
#' @param return_period   Integer. Return period.
#' @param out_path        Character or `NULL`.
#' @param width Numeric (default `10`). @param height Numeric (default `7`).
#' @param dpi   Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_trend_histogram <- function(ctry_indicators,
                                 indicator_label = "Exposed population",
                                 hazard_name     = "River Flood",
                                 return_period   = 100,
                                 out_path = NULL, width = 10, height = 7,
                                 dpi = 200) {

  max_pop <- max(ctry_indicators$pop.exposed, na.rm = TRUE)

  p <- ggplot2::ggplot(ctry_indicators, ggplot2::aes(x = year)) +
    ggplot2::geom_col(ggplot2::aes(y = pop.exposed), width = 0.65, alpha = 0.85) +
    ggplot2::geom_line(
      ggplot2::aes(y = (perc.pop.exposed / 100) * max_pop, group = 1),
      linewidth = 1.1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = (perc.pop.exposed / 100) * max_pop), size = 2.8
    ) +
    ggplot2::scale_x_continuous(breaks = ctry_indicators$year) +
    ggplot2::scale_y_continuous(
      name     = paste(indicator_label, "(count)"),
      labels   = scales::comma,
      sec.axis = ggplot2::sec_axis(~ (. / max_pop) * 100,
                                    name = paste(indicator_label, "(%)"))
    ) +
    ggplot2::labs(
      title   = paste0(indicator_label, " \u2014 ", hazard_name,
                       " RP", return_period, " (",
                       unique(ctry_indicators$country.code), ")"),
      subtitle = "Trend over time",
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.caption     = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# POPULATION PYRAMID
# ---------------------------------------------------------------------------

#' Prepare exposure data for a population pyramid
#' @export
viz_prep_pyramid_data <- function(exposure_stat) {
  exposure_stat |>
    dplyr::filter(sex %in% c("male", "female")) |>
    dplyr::mutate(
      perc_exposed = round((pop_exposed / pop_tot) * 100, 1),
      pop_exposed  = dplyr::case_when(sex == "female" ~ -pop_exposed, TRUE ~ pop_exposed),
      pop_tot      = dplyr::case_when(sex == "female" ~ -pop_tot,     TRUE ~ pop_tot),
      perc_exposed = dplyr::case_when(sex == "female" ~ -perc_exposed, TRUE ~ perc_exposed)
    )
}

#' Population pyramid of flood exposure
#' @export
viz_exposure_pyramid <- function(pyramid_data, ctry_code, out_path = NULL,
                                  width = 15, height = 10, dpi = 300) {

  pyramid_count <- ggplot2::ggplot(data = pyramid_data) +
    ggplot2::geom_bar(ggplot2::aes(x = pop_exposed, y = age_group, fill = sex),
                      stat = "identity", width = 0.9, color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(x = pop_exposed, y = age_group,
                   label = abs(round(pop_exposed / 1000, 1)),
                   hjust = ifelse(pop_exposed < 0, 1.1, -0.1)),
      size = 2.8, color = "black"
    ) +
    ggplot2::labs(title = paste("Flood Exposure by Age Group (Count):", ctry_code),
                  y = "Age Group", x = "Population Count (thousands)") +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~year, ncol = 4, nrow = 1)

  pyramid_perc <- ggplot2::ggplot(data = pyramid_data) +
    ggplot2::geom_bar(ggplot2::aes(x = perc_exposed, y = age_group, fill = sex),
                      stat = "identity", width = 0.9, color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(x = perc_exposed, y = age_group, label = abs(perc_exposed),
                   hjust = ifelse(perc_exposed < 0, 1.1, -0.1)),
      size = 2.8, color = "black"
    ) +
    ggplot2::labs(title = paste("Flood Exposure by Age Group (%):", ctry_code),
                  y = "Age Group", x = "% Population exposed") +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~year, ncol = 4, nrow = 1)

  combined <- pyramid_count / pyramid_perc

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = combined, scale = 1.2,
                    width = width, height = height, dpi = dpi)
    return(invisible(combined))
  }
  combined
}


# ---------------------------------------------------------------------------
# FACETED POPULATION MAP — one panel per demographic group
# ---------------------------------------------------------------------------

#' Faceted map of population rasters by demographic group
#'
#' Reads all processed group rasters from `pop_dir`, converts each to a data
#' frame, stacks them, and produces a single faceted map with one panel per
#' demographic group.  Colour scale is log\u2081\u2080 population count on a
#' YlOrRd palette.
#'
#' @param pop_dir    Character. Directory containing processed group rasters
#'   (`{iso3}_{group}_{year}_{scope}_{res}.tif`).
#' @param ctry_sf    sf. Country boundary polygon.
#' @param admin1_sf  sf or `NULL`. Admin1 context lines.
#' @param ctry_code  Character. ISO3 country code.
#' @param year       Integer. Population reference year label.
#' @param group_meta Tibble. Defaults to [WP_GROUP_META].
#' @param ncol       Integer. Number of facet columns (default `3`).
#' @param out_path   Character or `NULL`. Save path.
#' @param width      Numeric. Inches (default `14`).
#' @param height     Numeric. Inches (default `16`).
#' @param dpi        Numeric (default `250`).
#'
#' @return ggplot object.
#' @export
viz_facet_population <- function(pop_dir, ctry_sf, admin1_sf = NULL,
                                  ctry_code, year = 2020,
                                  group_meta = WP_GROUP_META,
                                  ncol = 3,
                                  out_path = NULL, width = 14, height = 16,
                                  dpi = 250) {

  pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(pop_files) == 0) stop("No .tif files found in: ", pop_dir)

  file_meta <- pop_parse_group_filenames(pop_files) |>
    dplyr::filter(!is.na(group_name)) |>
    dplyr::left_join(group_meta, by = "group_name") |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), group_name, label))

  if (nrow(file_meta) == 0)
    stop("No files matching the expected naming pattern found in: ", pop_dir)

  # Build stacked data frame
  df_all <- purrr::map_dfr(seq_len(nrow(file_meta)), function(i) {
    row    <- file_meta[i, ]
    ras    <- terra::rast(row$file)
    ras_c  <- pop_clip(ras, ctry_sf)
    df     <- .rast_to_df(ras_c, "pop")
    df     <- df[df$pop > 0, ]
    df$pop_log   <- log10(df$pop + 1)
    df$group_label <- row$label
    df
  })

  # Ordered factor for consistent panel order
  group_order <- group_meta$label[group_meta$label %in% unique(df_all$group_label)]
  df_all$group_label <- factor(df_all$group_label, levels = group_order)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL,
                     color = "grey70", linewidth = 0.3) +
    ggplot2::geom_raster(data = df_all,
                          ggplot2::aes(x = x, y = y, fill = pop_log)) +
    ggplot2::scale_fill_distiller(
      palette  = "YlOrRd", direction = 1,
      name     = "Population\n(log\u2081\u2080 scale)",
      breaks   = c(0, 1, 2, 3, 4),
      labels   = c("1", "10", "100", "1,000", "10,000"),
      guide    = ggplot2::guide_colorbar(
        barwidth  = 6, barheight = 0.5,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA,
                         color = "grey45", linewidth = 0.15)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA,
                     color = "#333333", linewidth = 0.45) +
    ggplot2::facet_wrap(~group_label, ncol = ncol) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title    = paste0("Population Distribution by Demographic Group \u2014 ", ctry_code),
      subtitle = paste0("WorldPop ", year, " | 1 km resolution"),
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle    = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption     = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold", size = 9,
                                                margin = ggplot2::margin(3, 0, 3, 0)),
      strip.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing    = ggplot2::unit(0.4, "lines"),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin      = ggplot2::margin(8, 8, 8, 8)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# FACETED OVERLAY MAP — flood fraction on each demographic group
# ---------------------------------------------------------------------------

#' Faceted map of flood fraction overlaid on each demographic group's population
#'
#' One panel per demographic group: population shown in greyscale,
#' flood fraction shown in orange on top.
#'
#' @param pop_dir        Character. Directory of processed group rasters.
#' @param flood_frac_ras SpatRaster. Flood fraction raster (output of
#'   [fld_aggregate_to_pop()]).
#' @param ctry_sf        sf. Country boundary.
#' @param admin1_sf      sf or `NULL`. Admin1 context lines.
#' @param ctry_code      Character. ISO3 country code.
#' @param hazard_name    Character. Full hazard name.
#' @param return_period  Integer. Return period in years.
#' @param year           Integer. Population reference year label.
#' @param group_meta     Tibble. Defaults to [WP_GROUP_META].
#' @param ncol           Integer. Facet columns (default `3`).
#' @param out_path       Character or `NULL`. Save path.
#' @param width          Numeric. Inches (default `14`).
#' @param height         Numeric. Inches (default `16`).
#' @param dpi            Numeric (default `250`).
#'
#' @return ggplot object.
#' @export
viz_facet_overlay <- function(pop_dir, flood_frac_ras, ctry_sf,
                               admin1_sf = NULL, ctry_code,
                               hazard_name   = "River Flood",
                               return_period = 100,
                               year          = 2020,
                               group_meta    = WP_GROUP_META,
                               ncol          = 3,
                               out_path = NULL, width = 14, height = 16,
                               dpi = 250) {

  pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(pop_files) == 0) stop("No .tif files found in: ", pop_dir)

  file_meta <- pop_parse_group_filenames(pop_files) |>
    dplyr::filter(!is.na(group_name)) |>
    dplyr::left_join(group_meta, by = "group_name") |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), group_name, label))

  if (nrow(file_meta) == 0)
    stop("No files matching the expected naming pattern found in: ", pop_dir)

  # Pre-convert flood raster once
  df_flood_base <- .rast_to_df(flood_frac_ras, "flood_frac")
  df_flood_base <- df_flood_base[df_flood_base$flood_frac > 0.01, ]

  # Build stacked population data frame
  df_pop_all <- purrr::map_dfr(seq_len(nrow(file_meta)), function(i) {
    row   <- file_meta[i, ]
    ras   <- terra::rast(row$file)
    ras_c <- pop_clip(ras, ctry_sf)
    df    <- .rast_to_df(ras_c, "pop")
    df    <- df[df$pop > 0, ]
    df$pop_log     <- log10(df$pop + 1)
    df$group_label <- row$label
    df
  })

  # Replicate flood layer for every group (for faceting)
  group_labels <- unique(df_pop_all$group_label)
  df_flood_all <- purrr::map_dfr(group_labels, function(lbl) {
    df_flood_base$group_label <- lbl
    df_flood_base
  })

  # Ordered factor
  group_order <- group_meta$label[group_meta$label %in% group_labels]
  df_pop_all$group_label   <- factor(df_pop_all$group_label,   levels = group_order)
  df_flood_all$group_label <- factor(df_flood_all$group_label, levels = group_order)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL, color = NA) +
    # Population layer (greyscale background)
    ggplot2::geom_raster(data = df_pop_all,
                          ggplot2::aes(x = x, y = y, fill = pop_log),
                          alpha = 0.85) +
    ggplot2::scale_fill_distiller(
      palette = "Greys", direction = 1,
      name    = "Population\n(log\u2081\u2080 scale)",
      breaks  = c(0, 1, 2, 3, 4),
      labels  = c("1", "10", "100", "1,000", "10,000"),
      guide   = ggplot2::guide_colorbar(
        barwidth  = 5, barheight = 0.4, order = 2,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    # Flood fraction layer (orange overlay)
    ggnewscale::new_scale_fill() +
    ggplot2::geom_raster(data = df_flood_all,
                          ggplot2::aes(x = x, y = y, fill = flood_frac),
                          alpha = 0.70) +
    ggplot2::scale_fill_distiller(
      palette = "Oranges", direction = 1,
      name    = "Flood fraction",
      breaks  = c(0, 0.25, 0.50, 0.75, 1.00),
      labels  = c("0%", "25%", "50%", "75%", "100%"),
      guide   = ggplot2::guide_colorbar(
        barwidth  = 5, barheight = 0.4, order = 1,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    { if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA,
                         color = "grey45", linewidth = 0.15)
    } +
    ggplot2::geom_sf(data = ctry_sf, fill = NA,
                     color = "#333333", linewidth = 0.45) +
    ggplot2::facet_wrap(~group_label, ncol = ncol) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title    = paste0("Population Exposed to ", hazard_name,
                        " (RP", return_period, ") by Group \u2014 ", ctry_code),
      subtitle = paste0("Grey = population density | Orange = flood fraction | ",
                        year, " WorldPop 1 km"),
      caption  = .WP_CAPTION
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle    = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 12),
      plot.caption     = ggplot2::element_text(hjust = 0, size = 9, color = "grey45"),
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold", size = 9,
                                                margin = ggplot2::margin(3, 0, 3, 0)),
      strip.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing    = ggplot2::unit(0.4, "lines"),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin      = ggplot2::margin(8, 8, 8, 8)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# DEPRIVATION MAP
# ---------------------------------------------------------------------------

#' Side-by-side maps of GRDI index and resulting vulnerability mask
#'
#' @param grdi_ras  SpatRaster. Aligned GRDI raster (0-100).
#' @param dep_mask  SpatRaster. Binary vulnerability mask (1/0).
#' @param ctry_sf   sf. Country boundary.
#' @param admin1_sf sf or `NULL`. Admin1 context lines.
#' @param ctry_code Character. ISO3 country code.
#' @param threshold Numeric. Threshold used to create mask.
#' @param out_path  Character or `NULL`. Save path.
#' @param width     Numeric. Inches (default `12`).
#' @param height    Numeric. Inches (default `5`).
#' @param dpi       Numeric (default `200`).
#'
#' @return patchwork ggplot.
#' @export
viz_plot_deprivation <- function(grdi_ras, dep_mask, ctry_sf,
                                  admin1_sf = NULL, ctry_code,
                                  threshold = 50,
                                  out_path = NULL, width = 12, height = 5,
                                  dpi = 200) {

  df_grdi <- .rast_to_df(grdi_ras, "grdi")
  df_mask <- .rast_to_df(dep_mask,  "vuln")
  df_mask <- df_mask[!is.na(df_mask$vuln), ]

  base_map <- function() {
    list(
      ggplot2::geom_sf(data = ctry_sf, fill = .LAND_FILL,
                       color = "grey70", linewidth = 0.3),
      if (!is.null(admin1_sf))
        ggplot2::geom_sf(data = admin1_sf, fill = NA,
                         color = "grey45", linewidth = 0.2),
      ggplot2::geom_sf(data = ctry_sf, fill = NA,
                       color = "#333333", linewidth = 0.45),
      ggplot2::coord_sf(expand = FALSE),
      ggplot2::theme_void(base_size = 10),
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 11),
        legend.position = "bottom",
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.margin     = ggplot2::margin(4, 4, 4, 4)
      )
    )
  }

  p1 <- ggplot2::ggplot() +
    base_map() +
    ggplot2::geom_raster(data = df_grdi,
                          ggplot2::aes(x = x, y = y, fill = grdi)) +
    ggplot2::scale_fill_distiller(
      palette = "RdYlGn", direction = -1,
      name    = "GRDI score\n(0=least, 100=most deprived)",
      limits  = c(0, 100),
      guide   = ggplot2::guide_colorbar(
        barwidth = 5, barheight = 0.4,
        title.position = "top", title.hjust = 0.5,
        label.theme = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
      )
    ) +
    ggplot2::labs(
      title    = paste0("Relative Deprivation Index \u2014 ", ctry_code),
      subtitle = "CIESIN/SEDAC GRDI v1 (2010\u20132020) | 1 km"
    )

  p2 <- ggplot2::ggplot() +
    base_map() +
    ggplot2::geom_raster(data = df_mask[df_mask$vuln == 1, ],
                          ggplot2::aes(x = x, y = y),
                          fill = "#c0392b", alpha = 0.80) +
    ggplot2::labs(
      title    = paste0("Vulnerability Mask \u2014 ", ctry_code),
      subtitle = paste0("Red = cells with GRDI \u2265 ", threshold,
                        " (classified as vulnerable)")
    )

  combined <- p1 + p2 +
    patchwork::plot_annotation(
      caption = .WP_CAPTION,
      theme   = ggplot2::theme(
        plot.caption    = ggplot2::element_text(hjust = 0, size = 9,
                                                 color = "grey45"),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = combined, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(combined))
  }
  combined
}


# ---------------------------------------------------------------------------
# METHODOLOGY WORKFLOW DIAGRAM
# ---------------------------------------------------------------------------

#' Flowchart summarising the climate vulnerability and flood exposure methodology
#'
#' @param out_path Character or `NULL`. Save path.
#' @param width    Numeric. Inches (default `10`).
#' @param height   Numeric. Inches (default `7`).
#' @param dpi      Numeric (default `200`).
#'
#' @return ggplot object.
#' @export
viz_workflow_diagram <- function(out_path = NULL, width = 10, height = 7,
                                  dpi = 200) {

  bx <- data.frame(
    cx    = c(1.65, 5.0,  8.35,  1.65, 5.0,   8.35,  1.65, 6.7,   4.15,  4.15),
    cy    = c(6.40, 6.40, 6.40,  4.60, 4.60,  4.60,  2.90, 2.90,  1.40,  0.25),
    hw    = c(1.45, 1.45, 1.45,  1.45, 1.45,  1.45,  1.45, 2.55,  3.85,  3.85),
    hh    = c(0.52, 0.52, 0.52,  0.52, 0.52,  0.52,  0.52, 0.52,  0.42,  0.32),
    label = c(
      "Flood Tiles\n(GloFAS RP100)",
      "WorldPop\nAge\u2013Sex Bands",
      "GRDI Deprivation\nIndex (CIESIN)",
      "Binary Flood\nRaster (0/1)",
      "Demographic\nGroup Rasters",
      "Deprivation\nMask (0/1)",
      "Flood Fraction\n@ 1 km grid",
      "Vulnerable\nPopulation Rasters",
      "Exposed Vulnerable Population\n(Vulnerable Pop \u00d7 Flood Fraction)",
      "Indicators Table  \u00b7  Maps  \u00b7  Word Report"
    ),
    fill  = c(
      "#aed6f1", "#a9dfbf", "#f9e79f",
      "#5dade2", "#2ecc71", "#f1c40f",
      "#2980b9", "#e67e22",
      "#e74c3c",
      "#8e44ad"
    ),
    stringsAsFactors = FALSE
  )

  # Vertical arrows (column drops)
  v_arrows <- data.frame(
    x0 = c(1.65, 5.0,  8.35,  1.65, 5.0,  8.35),
    y0 = c(5.88, 5.88, 5.88,  4.08, 4.08, 4.08),
    x1 = c(1.65, 5.0,  8.35,  1.65, 5.0,  8.35),
    y1 = c(5.12, 5.12, 5.12,  3.42, 3.42, 3.42)
  )
  # Cross arrows: WorldPop + GRDI row2 → right box of row3
  cross_arrows <- data.frame(
    x0 = c(5.0,  8.35),
    y0 = c(4.08, 4.08),
    x1 = c(5.6,  7.5),
    y1 = c(3.42, 3.42)
  )
  # Converging arrows: row3 boxes → row4
  conv_arrows <- data.frame(
    x0 = c(1.65, 6.7,   4.15),
    y0 = c(2.38, 2.38,  0.98),
    x1 = c(4.15, 4.15,  4.15),
    y1 = c(1.82, 1.82,  0.57)
  )

  all_arrows <- rbind(
    data.frame(x0=v_arrows$x0,    y0=v_arrows$y0,    x1=v_arrows$x1,    y1=v_arrows$y1),
    data.frame(x0=cross_arrows$x0,y0=cross_arrows$y0,x1=cross_arrows$x1,y1=cross_arrows$y1),
    data.frame(x0=conv_arrows$x0, y0=conv_arrows$y0, x1=conv_arrows$x1, y1=conv_arrows$y1)
  )

  row_labs <- data.frame(
    x     = rep(-0.1, 5),
    y     = c(6.40, 4.60, 2.90, 1.40, 0.25),
    label = c("DATA\nINPUTS", "PROCESSING", "COMBINED\nLAYERS",
              "EXPOSURE\nANALYSIS", "OUTPUTS")
  )

  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin=-0.3, xmax=10.3, ymin=5.75, ymax=7.05,
                      fill="#eaf4fb", alpha=0.6) +
    ggplot2::annotate("rect", xmin=-0.3, xmax=10.3, ymin=3.95, ymax=5.25,
                      fill="#eafaf1", alpha=0.6) +
    ggplot2::annotate("rect", xmin=-0.3, xmax=10.3, ymin=2.25, ymax=3.55,
                      fill="#fef9e7", alpha=0.6) +
    ggplot2::annotate("rect", xmin=-0.3, xmax=10.3, ymin=0.85, ymax=1.95,
                      fill="#fdedec", alpha=0.6) +
    ggplot2::annotate("rect", xmin=-0.3, xmax=10.3, ymin=-0.15, ymax=0.65,
                      fill="#f5eef8", alpha=0.6) +
    ggplot2::geom_segment(
      data = all_arrows,
      ggplot2::aes(x=x0, y=y0, xend=x1, yend=y1),
      arrow     = ggplot2::arrow(length=ggplot2::unit(0.16,"cm"), type="closed"),
      linewidth = 0.5, color = "grey35"
    ) +
    ggplot2::geom_rect(
      data = bx,
      ggplot2::aes(xmin=cx-hw, xmax=cx+hw, ymin=cy-hh, ymax=cy+hh, fill=fill),
      color="grey25", linewidth=0.35
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(
      data = bx,
      ggplot2::aes(x=cx, y=cy, label=label),
      size=2.55, fontface="bold", lineheight=0.88, color="#111111"
    ) +
    ggplot2::geom_text(
      data = row_labs,
      ggplot2::aes(x=x, y=y, label=label),
      size=1.9, color="grey45", fontface="italic", hjust=1, lineheight=0.82
    ) +
    ggplot2::scale_x_continuous(limits=c(-1.2, 10.4), expand=c(0,0)) +
    ggplot2::scale_y_continuous(limits=c(-0.25, 7.2),  expand=c(0,0)) +
    ggplot2::labs(
      title   = "Climate Vulnerability & Flood Exposure \u2014 Methodology Workflow",
      caption = paste0("GRDI = Global Relative Deprivation Index  \u00b7  ",
                       "RP = Return Period  \u00b7  GloFAS = Global Flood Awareness System")
    ) +
    ggplot2::theme_void(base_size=11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face="bold", hjust=0.5, size=15,
                                               margin=ggplot2::margin(b=6)),
      plot.caption    = ggplot2::element_text(hjust=0.5, size=9, color="grey50"),
      plot.background = ggplot2::element_rect(fill="white", color=NA),
      plot.margin     = ggplot2::margin(8, 12, 8, 12)
    ) -> p

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# SCENARIO COMPARISON CHART
# ---------------------------------------------------------------------------

#' Compare % exposed across the three analysis scenarios
#'
#' Produces a grouped bar chart with one cluster per demographic group and
#' up to three bars:
#'   raw_exposure  — all population exposed (no deprivation filter)
#'   vuln_vs_vuln  — vulnerable pop exposed as % of vulnerable pop
#'   vuln_vs_total — vulnerable pop exposed as % of full group pop
#'
#' @param ctry_indicators Tibble. Country-level summary (output of
#'   [exp_summarise_by_country()]).  Must contain a `scenario` column.
#' @param ctry_code    Character. ISO3 code used in the subtitle.
#' @param hazard_name  Character.
#' @param return_period Integer.
#' @param out_path     Character or NULL.
#' @param width,height,dpi Numeric. Save dimensions.
#'
#' @return ggplot object.
#' @export
viz_compare_scenarios <- function(ctry_indicators, ctry_code,
                                   hazard_name   = "River Flood",
                                   return_period = 100,
                                   out_path      = NULL,
                                   width = 12, height = 6, dpi = 250) {

  scen_labels <- c(
    raw_exposure  = "Exposed population\n(% of total group pop)",
    vuln_vs_vuln  = "Deprived population\n(% of deprived pop)",
    vuln_vs_total = "Deprived population\n(% of total group pop)"
  )
  scen_colours <- c(
    raw_exposure  = "#4e9ecf",
    vuln_vs_vuln  = "#e07b39",
    vuln_vs_total = "#9b59b6"
  )

  footnote_text <- paste0(
    "\u25a0 Exposed population (% of total group pop): ",
    "numerator = group pop \u00d7 flood fraction; ",
    "denominator = total group population.\n",
    "\u25a0 Deprived population (% of deprived pop): ",
    "numerator = deprived pop \u00d7 flood fraction; ",
    "denominator = total deprived population.\n",
    "\u25a0 Deprived population (% of total group pop): ",
    "numerator = deprived pop \u00d7 flood fraction; ",
    "denominator = total group population.\n",
    "Deprivation defined as GRDI score \u2265 50 (CIESIN/SEDAC Global Relative Deprivation Index v1)."
  )

  df <- ctry_indicators |>
    dplyr::filter(scenario %in% names(scen_labels)) |>
    dplyr::mutate(
      scenario_label = factor(scen_labels[scenario],
                              levels = unname(scen_labels))
    )

  # Order groups by raw_exposure % (ascending)
  grp_order <- df |>
    dplyr::filter(scenario == "raw_exposure") |>
    dplyr::arrange(perc.pop.exposed) |>
    dplyr::pull(group.label) |>
    as.character()
  df$group.label <- factor(df$group.label, levels = grp_order)

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = group.label, y = perc.pop.exposed,
                      fill = scenario_label)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.65, colour = "white", linewidth = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", perc.pop.exposed)),
      position = ggplot2::position_dodge(width = 0.75),
      vjust = -0.4, size = 2.6, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(unname(scen_colours), unname(scen_labels)),
      name   = "Scenario"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::labs(
      title    = paste0("% Population Exposed to ", hazard_name,
                        " (RP", return_period, ")"),
      subtitle = paste0(ctry_code, "  |  Three analysis scenarios"),
      caption  = footnote_text,
      x = NULL, y = "% exposed"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(face = "bold"),
      axis.text.x        = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold", size = 16),
      plot.caption       = ggplot2::element_text(size = 9, colour = "grey45",
                                                  hjust = 0, lineheight = 1.3,
                                                  margin = ggplot2::margin(t = 8)),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}


# ---------------------------------------------------------------------------
# TOP-N DISTRICTS HEATMAP
# ---------------------------------------------------------------------------

#' Heatmap: top N most-exposed districts across all demographic groups
#'
#' Identifies the N admin2 districts with the highest mean % population exposed
#' (raw_exposure scenario, aggregated across groups), then displays % exposed
#' for every demographic group in those districts as an annotated heatmap.
#'
#' @param indicators  Tibble. Admin2-level indicators from [exp_run_all_groups()].
#' @param ctry_code   Character.
#' @param hazard_name Character.
#' @param return_period Integer.
#' @param scenario    Character. Which scenario to display (default
#'   `"raw_exposure"`).
#' @param n_districts Integer. Number of top districts (default `5L`).
#' @param out_path    Character or NULL.
#' @param width,height,dpi Numeric. Save dimensions.
#'
#' @return ggplot object.
#' @export
viz_top_districts_heatmap <- function(indicators, ctry_code,
                                       hazard_name   = "River Flood",
                                       return_period = 100,
                                       scenario      = "raw_exposure",
                                       n_districts   = 5L,
                                       out_path      = NULL,
                                       width = 11, height = 6, dpi = 250) {

  # Identify top-N districts by mean % exposed across all groups
  # (raw_exposure scenario used for ranking regardless of display scenario)
  top_districts <- indicators |>
    dplyr::filter(scenario == "raw_exposure") |>
    dplyr::group_by(admin.code, admin.name) |>
    dplyr::summarise(mean_perc = mean(perc.pop.exposed, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::slice_max(mean_perc, n = n_districts) |>
    dplyr::arrange(dplyr::desc(mean_perc))

  # Build heat matrix: chosen scenario × top districts × all groups
  df_heat <- indicators |>
    dplyr::filter(
      scenario   == !!scenario,
      admin.code %in% top_districts$admin.code
    ) |>
    dplyr::mutate(
      admin.name = factor(admin.name, levels = rev(top_districts$admin.name))
    )

  # Order groups by mean perc (ascending) → most-exposed groups on the right
  grp_order <- df_heat |>
    dplyr::group_by(group.label) |>
    dplyr::summarise(m = mean(perc.pop.exposed, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(m) |>
    dplyr::pull(group.label)
  df_heat$group.label <- factor(df_heat$group.label, levels = grp_order)

  scen_title <- dplyr::case_when(
    scenario == "raw_exposure"  ~ "All population",
    scenario == "vuln_vs_vuln"  ~ "Deprived (% of deprived)",
    scenario == "vuln_vs_total" ~ "Deprived (% of total group)",
    TRUE ~ scenario
  )

  # Compute normalised position [0,1] within the data range for text contrast:
  # cells in the darker upper half of the scale get white text, lower half dark.
  val_range <- range(df_heat$perc.pop.exposed, na.rm = TRUE)
  df_heat <- df_heat |>
    dplyr::mutate(
      fill_pos = (perc.pop.exposed - val_range[1]) /
                   max(val_range[2] - val_range[1], 1e-9),
      txt_white = fill_pos > 0.55
    )

  p <- ggplot2::ggplot(df_heat,
         ggplot2::aes(x = group.label, y = admin.name,
                      fill = perc.pop.exposed)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(
        label  = sprintf("%.1f%%", perc.pop.exposed),
        colour = txt_white
      ),
      size = 3.4, fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#ffffd4", "#fed976", "#fd8d3c", "#e31a1c", "#800026"),
      name    = "% exposed",
      labels  = scales::label_percent(scale = 1)
    ) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "grey10", "TRUE" = "white"),
      guide  = "none"
    ) +
    ggplot2::labs(
      title    = paste0("Top ", n_districts, " Most-Exposed Districts \u2014 ",
                        hazard_name, " (RP", return_period, ")"),
      subtitle = paste0(ctry_code, "  |  Scenario: ", scen_title,
                        "  |  Districts ranked by mean exposure across groups"),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = -30, hjust = 0, size = 10),
      axis.text.y      = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "right",
      plot.title       = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle    = ggplot2::element_text(size = 12, colour = "grey40"),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA)
    )

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    return(invisible(p))
  }
  p
}
