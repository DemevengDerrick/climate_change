#################################################################################################
#                                                                                               #
#                                CLIMATE CHANGE PROJECT                                         #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Purpose: Flood exposure analysis across demographic groups at admin2 level.                  #
#  Run:     source("main.R")                                                                    #
#                                                                                               #
#  Population groups analysed:                                                                  #
#    Total population, Women 15-49, Youth 15-24, Young Women 15-24,                            #
#    Adolescents 10-19, Adolescent Girls 10-19, Children <5,                                   #
#    Population 65+, Women 65+                                                                  #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr, stringr, sf, ggplot2,
  geodata, terra, ggspatial,
  openxlsx, scales, lwgeom, purrr, tibble,
  ggrepel, patchwork, ggnewscale,
  officer, flextable
)

# LOAD PACKAGE FUNCTIONS -----------------------------------------------
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  source("R/worldpop_data.R")
  source("R/flood_utils.R")
  source("R/pop_utils.R")
  source("R/deprivation_utils.R")
  source("R/exposure_utils.R")
  source("R/viz_utils.R")
  source("R/report_utils.R")
}

# INPUT PARAMETERS -----------------------------------------------------
ctry_code       <- "KEN"
ctry_name       <- "Kenya"
return_period   <- 100
flood_threshold <- 0
year            <- 2020

hazard_name     <- "River Flood"
hazard_code     <- "riv_flood"

# Deprivation index settings
# GRDI score range: 0 (least deprived) to 100 (most deprived)
# Threshold of 50 = above-median deprivation (relatively deprived population)
dep_threshold <- 50
grdi_path     <- paste0(
  "input/gridded_relative_deprivation_index/",
  "CIESIN_SEDAC_PMP_GRDI_2010_2020_1.00-20260316_120329/",
  "povmap-grdi-v1-geotiff/povmap-grdi-v1.tif"
)

flood_dir        <- "input/flood_layers_RP100/"
flood_tiles_path <- "input/flood_tiles/tile_extends.geojson"
admin0_path      <- "input/Kenya_Admin_Boundaries/KEN_Admin_0.shp"
admin1_path      <- "input/Kenya_Admin_Boundaries/KEN_Admin_1.shp"
admin2_path      <- "input/Kenya_Admin_Boundaries/KEN_Admin_2.shp"
download_dir     <- "input/worldpop_raw"  # raw global 1km CN band files
pop_dir          <- "input/pop"           # processed group rasters from wp_build_all_groups()

# Output paths — all outputs are organised under output/{ctry_code}/
out_base      <- file.path("output", ctry_code)
out_maps      <- file.path(out_base, "flood_maps")
out_stats     <- file.path(out_base, "zonal_stats")
out_reports   <- file.path(out_base, "reports")

indicators_out     <- file.path(out_stats,   paste0(ctry_code, "_indicators.xlsx"))
map_out            <- file.path(out_maps,    paste0(ctry_code, "_exposure_by_group.jpg"))
bars_out           <- file.path(out_maps,    paste0(ctry_code, "_exposure_bars.jpg"))
dotplot_out        <- file.path(out_maps,    paste0(ctry_code, "_exposure_dotplot.jpg"))
bubble_out         <- file.path(out_maps,    paste0(ctry_code, "_exposure_bubble.jpg"))
hazard_map_out     <- file.path(out_maps,    paste0(ctry_code, "_hazard_layer.jpg"))
pop_map_out        <- file.path(out_maps,    paste0(ctry_code, "_population_layer.jpg"))
overlay_out        <- file.path(out_maps,    paste0(ctry_code, "_hazard_pop_overlay.jpg"))
pop_facet_out      <- file.path(out_maps,    paste0(ctry_code, "_pop_by_group.jpg"))
overlay_facet_out  <- file.path(out_maps,    paste0(ctry_code, "_flood_overlay_by_group.jpg"))
dep_map_out        <- file.path(out_maps,    paste0(ctry_code, "_deprivation.jpg"))
workflow_out       <- file.path(out_maps,    paste0(ctry_code, "_methodology_workflow.jpg"))
scenario_chart_out  <- file.path(out_maps,    paste0(ctry_code, "_scenario_comparison.jpg"))
top5_heatmap_out    <- file.path(out_maps,    paste0(ctry_code, "_top5_districts_heatmap.jpg"))
map_vuln_vuln_out   <- file.path(out_maps,    paste0(ctry_code, "_exposure_vuln_vs_vuln.jpg"))
map_vuln_total_out  <- file.path(out_maps,    paste0(ctry_code, "_exposure_vuln_vs_total.jpg"))
report_out          <- file.path(out_reports, paste0(ctry_code, "_flood_exposure_summary.docx"))

# WORLDPOP DATA CHECK --------------------------------------------------
# Check which global 1km constrained UN-adjusted band files are downloaded.
# If processed group rasters are missing, build them from the raw band files.
wp_status <- wp_check_global_downloaded(year, download_dir)
n_found   <- sum(wp_status$exists)
n_total   <- nrow(wp_status)

message(
  "\n--- WorldPop Population Data ---\n",
  "  Dataset : Global 1km Constrained UN-adjusted (R2025A)\n",
  "  Country : ", ctry_name, " (", ctry_code, ")  |  Year: ", year, "\n",
  "  Raw band files found: ", n_found, " / ", n_total,
  " in '", download_dir, "'\n",
  if (n_found == n_total) {
    "  Status  : All band files present.\n"
  } else {
    paste0("  Status  : ", n_total - n_found,
           " band file(s) missing — run wp_build_all_groups() to download.\n")
  }
)

# Check whether processed group rasters exist in pop_dir.
pop_files_exist <- length(list.files(
  pop_dir,
  pattern = paste0("^", tolower(ctry_code), "_.+_", year, "_global_1km_CN\\.tif$")
)) == length(WP_GROUPS)

if (!pop_files_exist) {
  message("  Processed group rasters not found in '", pop_dir,
          "' — building now (this may take several minutes) ...")

  # Load boundaries first (needed for clipping); repeat after this block
  .tmp_admin0 <- sf::read_sf(admin0_path) |>
    dplyr::mutate(shapeGroup = ctry_code)

  wp_build_all_groups(
    iso3         = ctry_code,
    year         = year,
    download_dir = download_dir,
    pop_dir      = pop_dir,
    boundary     = .tmp_admin0,
    scope        = "global"
  )
  rm(.tmp_admin0)
  message("  Processed rasters ready in '", pop_dir, "'.")
} else {
  message("  Processed group rasters found in '", pop_dir,
          "' — skipping wp_build_all_groups().\n",
          "  Using: Global 1km Constrained UN-adjusted rasters clipped to ",
          ctry_name, " (", ctry_code, ").\n",
          "  NOTE : For 100m resolution use wp_build_all_groups(scope = 'country').\n")
}
message("--------------------------------\n")

# LOAD BOUNDARY DATA ---------------------------------------------------
# Kenya-specific boundaries; remap columns to the standard names used by
# exposure functions (shapeGroup, shapeID, shapeName).
ctry_admin0 <- sf::read_sf(admin0_path) |>
  dplyr::mutate(shapeGroup = ctry_code)

admin1_ctry <- sf::read_sf(admin1_path) |>
  dplyr::mutate(shapeGroup = ctry_code,
                shapeID    = paste0(ctry_code, "_ADM1_", dplyr::row_number()),
                shapeName  = Admin_1)

admin2_ctry <- sf::read_sf(admin2_path) |>
  dplyr::mutate(shapeGroup = ctry_code,
                shapeID    = paste0(ctry_code, "_ADM2_", dplyr::row_number()),
                shapeName  = NAME_2)

# FLOOD MOSAIC & PRE-PROCESSING ----------------------------------------
tile_files     <- fld_select_tiles(ctry_admin0, flood_tiles_path, flood_dir,
                                   return_period)
rp100          <- fld_build_vrt(tile_files, file.path(
                    out_base, "vrt_flood",
                    paste0(ctry_code, "_RP", return_period, "_depth.vrt")))
rp100_bin      <- fld_binarise(rp100, flood_threshold)
rp100_bin_clip <- fld_clip(rp100_bin, ctry_admin0)

# Reference grid: use the total_pop raster (all group rasters share this grid)
pop_ref_file <- list.files(pop_dir,
                            pattern = paste0("^", tolower(ctry_code), "_total_pop_"),
                            full.names = TRUE)[1]
pop_ref      <- terra::rast(pop_ref_file)
pop_ref_c    <- pop_clip(pop_ref, ctry_admin0)

flood_frac   <- fld_aggregate_to_pop(rp100_bin_clip, pop_ref_c)

# DEPRIVATION / VULNERABILITY MASK ------------------------------------
dep_mask <- dep_build_mask(grdi_path, pop_ref_c, threshold = dep_threshold)
message("Deprivation mask built at GRDI threshold >= ", dep_threshold)

# DIAGNOSTIC MAPS (hazard, population, deprivation, overlay) ----------
dir.create(out_maps,                           recursive = TRUE, showWarnings = FALSE)
dir.create(out_stats,                          recursive = TRUE, showWarnings = FALSE)
dir.create(out_reports,                        recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_base, "vrt_flood"),   recursive = TRUE, showWarnings = FALSE)

# Workflow diagram (methodology overview)
viz_workflow_diagram(out_path = workflow_out)
message("Workflow diagram saved to: ", workflow_out)

viz_plot_hazard(
  flood_frac_ras = flood_frac,
  ctry_sf        = ctry_admin0,
  admin1_sf      = admin1_ctry,
  ctry_code      = ctry_code,
  hazard_name    = hazard_name,
  return_period  = return_period,
  out_path       = hazard_map_out
)
message("Hazard map saved to: ", hazard_map_out)

viz_plot_population(
  pop_ras   = pop_ref_c,
  ctry_sf   = ctry_admin0,
  admin1_sf = admin1_ctry,
  ctry_code = ctry_code,
  year      = year,
  out_path  = pop_map_out
)
message("Population map saved to: ", pop_map_out)

viz_plot_overlay(
  pop_ras        = pop_ref_c,
  flood_frac_ras = flood_frac,
  ctry_sf        = ctry_admin0,
  admin1_sf      = admin1_ctry,
  ctry_code      = ctry_code,
  hazard_name    = hazard_name,
  return_period  = return_period,
  year           = year,
  out_path       = overlay_out
)
message("Overlay map saved to: ", overlay_out)

# Align GRDI for visualisation (already clipped via dep_mask, need raw values too)
grdi_aligned <- dep_align_to_pop(grdi_path, pop_ref_c)
grdi_c       <- pop_clip(grdi_aligned, ctry_admin0)
dep_mask_c   <- pop_clip(dep_mask,     ctry_admin0)

viz_plot_deprivation(
  grdi_ras  = grdi_c,
  dep_mask  = dep_mask_c,
  ctry_sf   = ctry_admin0,
  admin1_sf = admin1_ctry,
  ctry_code = ctry_code,
  threshold = dep_threshold,
  out_path  = dep_map_out
)
message("Deprivation map saved to: ", dep_map_out)

viz_facet_population(
  pop_dir   = pop_dir,
  ctry_sf   = ctry_admin0,
  admin1_sf = admin1_ctry,
  ctry_code = ctry_code,
  year      = year,
  out_path  = pop_facet_out
)
message("Faceted population map saved to: ", pop_facet_out)

viz_facet_overlay(
  pop_dir        = pop_dir,
  flood_frac_ras = flood_frac,
  ctry_sf        = ctry_admin0,
  admin1_sf      = admin1_ctry,
  ctry_code      = ctry_code,
  hazard_name    = hazard_name,
  return_period  = return_period,
  year           = year,
  out_path       = overlay_facet_out
)
message("Faceted overlay map saved to: ", overlay_facet_out)

# EXPOSURE ACROSS ALL DEMOGRAPHIC GROUPS --------------------------------
indicators <- exp_run_all_groups(
  pop_dir        = pop_dir,
  flood_frac_ras = flood_frac,
  ctry_boundary  = ctry_admin0,
  admin_zones    = admin2_ctry,
  dep_mask       = dep_mask,
  hazard_name    = hazard_name,
  hazard_code    = hazard_code,
  return_period  = return_period,
  admin_level    = 2L
)

# EXPORT RESULTS -------------------------------------------------------
ctry_indicators <- exp_summarise_by_country(indicators)

# Convenience subsets by scenario
ind_raw         <- dplyr::filter(indicators,       scenario == "raw_exposure")
ind_vuln_vuln   <- dplyr::filter(indicators,       scenario == "vuln_vs_vuln")
ind_vuln_total  <- dplyr::filter(indicators,       scenario == "vuln_vs_total")
ctry_raw        <- dplyr::filter(ctry_indicators,  scenario == "raw_exposure")
ctry_vuln_vuln  <- dplyr::filter(ctry_indicators,  scenario == "vuln_vs_vuln")
ctry_vuln_total <- dplyr::filter(ctry_indicators,  scenario == "vuln_vs_total")

wb <- openxlsx::createWorkbook()

# Admin2 — one sheet per scenario
openxlsx::addWorksheet(wb, "Admin2 Raw Exposure")
openxlsx::writeData(wb, "Admin2 Raw Exposure",    ind_raw)
openxlsx::addWorksheet(wb, "Admin2 Vuln vs Vuln")
openxlsx::writeData(wb, "Admin2 Vuln vs Vuln",    ind_vuln_vuln)
openxlsx::addWorksheet(wb, "Admin2 Vuln vs Total")
openxlsx::writeData(wb, "Admin2 Vuln vs Total",   ind_vuln_total)

# Country summary — all scenarios together (scenario column distinguishes them)
openxlsx::addWorksheet(wb, "Country Summary")
openxlsx::writeData(wb, "Country Summary",        ctry_indicators)

openxlsx::saveWorkbook(wb, indicators_out, overwrite = TRUE)
message("Indicators saved to: ", indicators_out)

# Footnotes for each exposure map (explain numerator / denominator)
fn_raw <- paste0(
  "Numerator: group population \u00d7 flood fraction (share of 1km cell inundated). ",
  "Denominator: total group population.\n",
  "Interpretation: of all people in this demographic group, what share lives in a flood-prone area?"
)
fn_vuln_vuln <- paste0(
  "Numerator: deprived population \u00d7 flood fraction ",
  "(deprived = GRDI score \u2265 50, CIESIN/SEDAC GRDI v1). ",
  "Denominator: total deprived (GRDI \u2265 50) population in each group.\n",
  "Interpretation: of the deprived members of this group, what share also lives in a flood-prone area?"
)
fn_vuln_total <- paste0(
  "Numerator: deprived population \u00d7 flood fraction ",
  "(deprived = GRDI score \u2265 50, CIESIN/SEDAC GRDI v1). ",
  "Denominator: total group population (deprived + non-deprived).\n",
  "Interpretation: of all members of this group, what share is both deprived AND flood-exposed?"
)

# VISUALISE ------------------------------------------------------------

## 1a. Choropleth — raw exposure (all group pop, % of total group) ------
admin2_join <- admin2_ctry |>
  dplyr::left_join(
    dplyr::select(ind_raw, admin.code, group.label, year,
                  total.pop, pop.exposed, perc.pop.exposed),
    by = c("shapeID" = "admin.code")
  )

viz_choropleth(
  admin_sf      = admin2_join,
  ctry_sf       = ctry_admin0,
  admin1_sf     = admin1_ctry,
  ctry_code     = ctry_code,
  hazard_name   = hazard_name,
  return_period = return_period,
  year          = year,
  footnote      = fn_raw,
  out_path      = map_out,
  ncol          = 3
)
message("Choropleth (raw) saved to: ", map_out)

## 1b. Choropleth — deprived pop exposed, % of deprived pop -------------
admin2_join_vv <- admin2_ctry |>
  dplyr::left_join(
    dplyr::select(ind_vuln_vuln, admin.code, group.label, year,
                  total.pop, pop.exposed, perc.pop.exposed),
    by = c("shapeID" = "admin.code")
  )

viz_choropleth(
  admin_sf      = admin2_join_vv,
  ctry_sf       = ctry_admin0,
  admin1_sf     = admin1_ctry,
  ctry_code     = ctry_code,
  hazard_name   = paste0(hazard_name, " \u2014 Deprived Pop (% of deprived)"),
  return_period = return_period,
  year          = year,
  footnote      = fn_vuln_vuln,
  out_path      = map_vuln_vuln_out,
  ncol          = 3
)
message("Choropleth (vuln vs vuln) saved to: ", map_vuln_vuln_out)

## 1c. Choropleth — deprived pop exposed, % of total group pop ----------
admin2_join_vt <- admin2_ctry |>
  dplyr::left_join(
    dplyr::select(ind_vuln_total, admin.code, group.label, year,
                  total.pop, pop.exposed, perc.pop.exposed),
    by = c("shapeID" = "admin.code")
  )

viz_choropleth(
  admin_sf      = admin2_join_vt,
  ctry_sf       = ctry_admin0,
  admin1_sf     = admin1_ctry,
  ctry_code     = ctry_code,
  hazard_name   = paste0(hazard_name, " \u2014 Deprived Pop (% of total group)"),
  return_period = return_period,
  year          = year,
  footnote      = fn_vuln_total,
  out_path      = map_vuln_total_out,
  ncol          = 3
)
message("Choropleth (vuln vs total) saved to: ", map_vuln_total_out)

## 2. Horizontal bar chart — absolute exposed by group (raw) -----------
viz_group_bars(
  ctry_indicators = ctry_raw,
  ctry_code       = ctry_code,
  hazard_name     = hazard_name,
  return_period   = return_period,
  out_path        = bars_out
)
message("Bar chart saved to: ", bars_out)

## 3. Lollipop chart — % exposed by group (raw, ranked) ----------------
viz_exposure_dotplot(
  ctry_indicators = ctry_raw,
  ctry_code       = ctry_code,
  hazard_name     = hazard_name,
  return_period   = return_period,
  out_path        = dotplot_out
)
message("Dot plot saved to: ", dotplot_out)

## 4. Bubble chart — group size vs exposure rate (raw) -----------------
viz_exposure_bubble(
  ctry_indicators = ctry_raw,
  ctry_code       = ctry_code,
  hazard_name     = hazard_name,
  return_period   = return_period,
  out_path        = bubble_out
)
message("Bubble chart saved to: ", bubble_out)

## 5. Scenario comparison — raw vs vuln/vuln vs vuln/total -------------
viz_compare_scenarios(
  ctry_indicators = ctry_indicators,
  ctry_code       = ctry_code,
  hazard_name     = hazard_name,
  return_period   = return_period,
  out_path        = scenario_chart_out
)
message("Scenario comparison chart saved to: ", scenario_chart_out)

## 6. Top-5 districts heatmap (raw exposure) ---------------------------
viz_top_districts_heatmap(
  indicators    = ind_raw,
  ctry_code     = ctry_code,
  hazard_name   = hazard_name,
  return_period = return_period,
  scenario      = "raw_exposure",
  n_districts   = 5L,
  out_path      = top5_heatmap_out
)
message("Top-5 districts heatmap saved to: ", top5_heatmap_out)

# GENERATE WORD REPORT -------------------------------------------------
rpt_generate_word(
  ctry_code            = ctry_code,
  ctry_name            = ctry_name,
  hazard_name          = hazard_name,
  return_period        = return_period,
  year                 = year,
  ctry_indicators      = ctry_raw,
  ctry_indicators_vuln = ctry_vuln_total,   # deprived pop exposed, % of total group
  img_hazard           = hazard_map_out,
  img_population       = pop_map_out,
  img_overlay          = overlay_out,
  img_deprivation      = dep_map_out,
  img_workflow         = workflow_out,
  img_choropleth       = map_out,
  img_scenario_chart   = scenario_chart_out,
  img_top5_heatmap     = top5_heatmap_out,
  img_bars             = bars_out,
  img_dotplot          = dotplot_out,
  img_bubble           = bubble_out,
  dep_threshold        = dep_threshold,
  out_path             = report_out
)

message("\nDone. All outputs saved.")


