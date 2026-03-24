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
# Always source directly so edits to R/ are picked up immediately.
# devtools::load_all() can silently serve a stale in-memory cache when the
# package byte-code or installed version hasn't been rebuilt after edits.
source("R/worldpop_data.R")
source("R/flood_utils.R")
source("R/pop_utils.R")
source("R/deprivation_utils.R")
source("R/exposure_utils.R")
source("R/degurba_utils.R")
source("R/viz_utils.R")
source("R/report_utils.R")

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

dug_l1_path      <- "input/DUG/grids/GHS-DUG_GRID_L1.tif"
dug_l2_path      <- "input/DUG/grids/GHS-DUG_GRID_L2.tif"

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
# Individual group maps: paths built dynamically inside the per-group loop
# (saved under out_maps/exposure_groups/ and out_maps/vuln_groups/)
dep_map_out        <- file.path(out_maps,    paste0(ctry_code, "_deprivation.jpg"))
workflow_out       <- file.path(out_maps,    paste0(ctry_code, "_methodology_workflow.jpg"))
scenario_chart_out  <- file.path(out_maps,    paste0(ctry_code, "_scenario_comparison.jpg"))
top5_heatmap_out    <- file.path(out_maps,    paste0(ctry_code, "_top5_districts_heatmap.jpg"))
dug_l1_out          <- file.path(out_maps, "degurba", paste0(ctry_code, "_degurba_l1.jpg"))
dug_l2_out          <- file.path(out_maps, "degurba", paste0(ctry_code, "_degurba_l2.jpg"))
map_vuln_vuln_out   <- file.path(out_maps,    paste0(ctry_code, "_exposure_vuln_vs_vuln.jpg"))
map_vuln_total_out  <- file.path(out_maps,    paste0(ctry_code, "_exposure_vuln_vs_total.jpg"))
report_out          <- file.path(out_reports, paste0(ctry_code, "_flood_exposure_summary.docx"))
ppt_out             <- file.path(out_reports, paste0(ctry_code, "_management_presentation.pptx"))

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
message("Poverty mask built at GRDI threshold >= ", dep_threshold)

# DEGURBA GRIDS — align to population reference grid ------------------
message("Aligning DEGURBA grids to population grid ...")
dug_l1 <- deg_align_to_pop(dug_l1_path, pop_ref_c)
dug_l2 <- deg_align_to_pop(dug_l2_path, pop_ref_c)
message("  -> L1 and L2 grids aligned.")

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
message("Poverty map saved to: ", dep_map_out)

# INDIVIDUAL MAPS PER DEMOGRAPHIC GROUP --------------------------------
# One exposure map and one vulnerability-exposure map per group.
# Files saved to: out_maps/exposure_groups/ and out_maps/vuln_groups/
out_exposure_grp <- file.path(out_maps, "exposure_groups")
out_vuln_grp     <- file.path(out_maps, "vuln_groups")
dir.create(out_exposure_grp, recursive = TRUE, showWarnings = FALSE)
dir.create(out_vuln_grp,     recursive = TRUE, showWarnings = FALSE)

purrr::walk(seq_len(nrow(WP_GROUP_META)), function(i) {
  gname  <- WP_GROUP_META$group_name[i]
  glabel <- WP_GROUP_META$label[i]

  # Locate the processed group raster
  grp_files <- list.files(pop_dir,
                           pattern = paste0("^", tolower(ctry_code), "_", gname, "_"),
                           full.names = TRUE)
  if (length(grp_files) == 0) {
    message("  [skip] No raster found for group: ", gname)
    return(invisible(NULL))
  }
  pop_grp <- terra::rast(grp_files[1])
  pop_grp_c <- pop_clip(pop_grp, ctry_admin0)

  # --- Exposure map (all population × flood fraction) ---
  exp_out <- file.path(out_exposure_grp,
                        paste0(tolower(ctry_code), "_exposure_", gname, ".jpg"))
  viz_plot_group_exposure(
    pop_ras        = pop_grp_c,
    flood_frac_ras = flood_frac,
    ctry_sf        = ctry_admin0,
    admin1_sf      = admin1_ctry,
    ctry_code      = ctry_code,
    group_label    = glabel,
    hazard_name    = hazard_name,
    return_period  = return_period,
    year           = year,
    out_path       = exp_out
  )

  # --- Vulnerability-exposure map (deprived pop × flood fraction) ---
  vuln_out <- file.path(out_vuln_grp,
                         paste0(tolower(ctry_code), "_vuln_exposure_", gname, ".jpg"))
  viz_plot_group_vuln(
    pop_ras        = pop_grp_c,
    dep_mask       = dep_mask_c,
    flood_frac_ras = flood_frac,
    ctry_sf        = ctry_admin0,
    admin1_sf      = admin1_ctry,
    ctry_code      = ctry_code,
    group_label    = glabel,
    hazard_name    = hazard_name,
    return_period  = return_period,
    year           = year,
    dep_threshold  = dep_threshold,
    out_path       = vuln_out
  )

  message("  Group maps saved: ", glabel)
})
message("Individual group maps saved to: ", out_exposure_grp,
        "\n                                ", out_vuln_grp)

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

# DEGURBA — urbanisation-stratified exposure analysis ------------------
message("\n--- DEGURBA Urbanisation-Stratified Exposure ---")
dug_exp_l1 <- deg_run_exposure(
  pop_dir       = pop_dir,
  flood_frac_ras = flood_frac,
  dug_ras       = dug_l1,
  dep_mask      = dep_mask,
  level_labels  = DEG_L1_LABELS,
  ctry_boundary = ctry_admin0,
  hazard_name   = hazard_name,
  return_period = return_period,
  year          = year
)
message("  -> L1 done (", nrow(dug_exp_l1), " rows)")

dug_exp_l2 <- deg_run_exposure(
  pop_dir       = pop_dir,
  flood_frac_ras = flood_frac,
  dug_ras       = dug_l2,
  dep_mask      = dep_mask,
  level_labels  = DEG_L2_LABELS,
  ctry_boundary = ctry_admin0,
  hazard_name   = hazard_name,
  return_period = return_period,
  year          = year
)
message("  -> L2 done (", nrow(dug_exp_l2), " rows)")

# DEGURBA sheets in the workbook
openxlsx::addWorksheet(wb, "DEGURBA L1 Exposure")
openxlsx::writeData(wb, "DEGURBA L1 Exposure", dug_exp_l1)
openxlsx::addWorksheet(wb, "DEGURBA L2 Exposure")
openxlsx::writeData(wb, "DEGURBA L2 Exposure", dug_exp_l2)

openxlsx::saveWorkbook(wb, indicators_out, overwrite = TRUE)
message("Indicators saved to: ", indicators_out)

# Footnotes for each exposure map (explain numerator / denominator)
fn_raw <- paste0(
  "Numerator: group population \u00d7 flood fraction (share of 1km cell inundated). ",
  "Denominator: total group population.\n",
  "Interpretation: of all people in this demographic group, what share lives in a flood-prone area?"
)
fn_vuln_vuln <- paste0(
  "Numerator: poor population \u00d7 flood fraction ",
  "(poor = GRDI score \u2265 50, CIESIN/SEDAC GRDI v1). ",
  "Denominator: total poor (GRDI \u2265 50) population in each group.\n",
  "Interpretation: of the poor members of this group, what share also lives in a flood-prone area?"
)
fn_vuln_total <- paste0(
  "Numerator: poor population \u00d7 flood fraction ",
  "(poor = GRDI score \u2265 50, CIESIN/SEDAC GRDI v1). ",
  "Denominator: total group population (poor + non-poor).\n",
  "Interpretation: of all members of this group, what share is both poor AND flood-exposed?"
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
  hazard_name   = paste0(hazard_name, " \u2014 Poor Pop (% of poor)"),
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
  hazard_name   = paste0(hazard_name, " \u2014 Poor Pop (% of total group)"),
  return_period = return_period,
  year          = year,
  footnote      = fn_vuln_total,
  out_path      = map_vuln_total_out,
  ncol          = 3
)
message("Choropleth (vuln vs total) saved to: ", map_vuln_total_out)

## 1d. Individual two-panel choropleth per group (exposure + vulnerability)
out_choropleth_grp <- file.path(out_maps, "choropleth_groups")
dir.create(out_choropleth_grp, recursive = TRUE, showWarnings = FALSE)

# Build a named list of paths — used directly in rpt_generate_word()
choropleth_grp_paths <- purrr::map(
  stats::setNames(WP_GROUP_META$group_name, WP_GROUP_META$label),
  function(gname) {
    glabel <- WP_GROUP_META$label[WP_GROUP_META$group_name == gname]

    a2_raw  <- dplyr::filter(admin2_join,    group.label == glabel)
    a2_vuln <- dplyr::filter(admin2_join_vv, group.label == glabel)

    if (nrow(a2_raw) == 0 || nrow(a2_vuln) == 0) {
      message("  [skip choropleth] no data for group: ", glabel)
      return(NULL)
    }

    grp_out <- file.path(out_choropleth_grp,
                          paste0(tolower(ctry_code), "_choropleth_", gname, ".jpg"))

    viz_choropleth_group(
      admin_sf_raw  = a2_raw,
      admin_sf_vuln = a2_vuln,
      ctry_sf       = ctry_admin0,
      admin1_sf     = admin1_ctry,
      ctry_code     = ctry_code,
      group_label   = glabel,
      hazard_name   = hazard_name,
      return_period = return_period,
      year          = year,
      dep_threshold = dep_threshold,
      out_path      = grp_out
    )
    message("  Choropleth saved: ", glabel)
    grp_out
  }
) |> purrr::compact()   # drop any NULLs from skipped groups
message("Individual group choropleths saved to: ", out_choropleth_grp)

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

## 7. DEGURBA L1 — grouped bar chart -----------------------------------
dir.create(dirname(dug_l1_out), recursive = TRUE, showWarnings = FALSE)
viz_degurba_l1_bars(
  degurba_df    = dug_exp_l1,
  ctry_code     = ctry_code,
  hazard_name   = hazard_name,
  return_period = return_period,
  dep_threshold = dep_threshold,
  out_path      = dug_l1_out
)
message("DEGURBA L1 chart saved to: ", dug_l1_out)

## 8. DEGURBA L2 — heatmap ---------------------------------------------
viz_degurba_l2_heatmap(
  degurba_df    = dug_exp_l2,
  ctry_code     = ctry_code,
  hazard_name   = hazard_name,
  return_period = return_period,
  dep_threshold = dep_threshold,
  out_path      = dug_l2_out
)
message("DEGURBA L2 chart saved to: ", dug_l2_out)

# GENERATE WORD REPORT -------------------------------------------------
rpt_generate_word(
  ctry_code             = ctry_code,
  ctry_name             = ctry_name,
  hazard_name           = hazard_name,
  return_period         = return_period,
  year                  = year,
  ctry_indicators       = ctry_raw,
  ctry_indicators_vuln  = ctry_vuln_total,
  img_hazard            = hazard_map_out,
  img_population        = pop_map_out,
  img_overlay           = overlay_out,
  img_deprivation       = dep_map_out,
  img_workflow          = workflow_out,
  img_choropleth_groups = choropleth_grp_paths,
  img_scenario_chart    = scenario_chart_out,
  img_top5_heatmap      = top5_heatmap_out,
  img_degurba_l1        = dug_l1_out,
  img_degurba_l2        = dug_l2_out,
  img_bars              = bars_out,
  img_dotplot           = dotplot_out,
  img_bubble            = bubble_out,
  dep_threshold         = dep_threshold,
  out_path              = report_out
)

# GENERATE POWERPOINT PRESENTATION ------------------------------------
# rpt_generate_ppt(
#   ctry_code            = ctry_code,
#   ctry_name            = ctry_name,
#   hazard_name          = hazard_name,
#   return_period        = return_period,
#   year                 = year,
#   ctry_indicators      = ctry_raw,
#   ctry_indicators_vuln = ctry_vuln_total,
#   img_hazard           = hazard_map_out,
#   img_population       = pop_map_out,
#   img_overlay          = overlay_out,
#   img_deprivation      = dep_map_out,
#   img_workflow         = workflow_out,
#   img_scenario_chart   = scenario_chart_out,
#   img_top5_heatmap     = top5_heatmap_out,
#   choropleth_grp_paths = choropleth_grp_paths,
#   dep_threshold        = dep_threshold,
#   out_path             = ppt_out
# )

# message("\nDone. All outputs saved.")


