# Climate Change Project — Function Reference Manual

**Author:** Derrick DEMEVENG
**Last updated:** 2026-03-15
**Project:** Flood & Conflict Exposure Analysis

---

## Table of Contents

1. [Overview](#1-overview)
2. [Project Structure](#2-project-structure)
3. [How to Load the Functions](#3-how-to-load-the-functions)
4. [Module: `R/flood_utils.R`](#4-module-rflood_utilsr) — Flood data processing
   - [fld_select_tiles](#fld_select_tiles)
   - [fld_build_vrt](#fld_build_vrt)
   - [fld_binarise](#fld_binarise)
   - [fld_clip](#fld_clip)
   - [fld_aggregate_to_pop](#fld_aggregate_to_pop)
   - [fld_download_tiles](#fld_download_tiles)
5. [Module: `R/pop_utils.R`](#5-module-rpop_utilsr) — Population metadata
   - [pop_parse_filename](#pop_parse_filename)
   - [pop_parse_filenames](#pop_parse_filenames)
   - [pop_clip](#pop_clip)
6. [Module: `R/exposure_utils.R`](#6-module-rexposure_utilsr) — Exposure computation
   - [exp_compute](#exp_compute)
   - [exp_zonal_stats](#exp_zonal_stats)
   - [exp_build_indicator_tibble](#exp_build_indicator_tibble)
   - [exp_summarise_by_country](#exp_summarise_by_country)
   - [exp_run_country](#exp_run_country)
7. [Module: `R/viz_utils.R`](#7-module-rviz_utilsr) — Visualisation
   - [viz_choropleth](#viz_choropleth)
   - [viz_trend_histogram](#viz_trend_histogram)
   - [viz_prep_pyramid_data](#viz_prep_pyramid_data)
   - [viz_exposure_pyramid](#viz_exposure_pyramid)
8. [Full Analysis Workflow](#8-full-analysis-workflow)
9. [Running Tests](#9-running-tests)
10. [Data Sources & Input File Conventions](#10-data-sources--input-file-conventions)
11. [Package Dependencies](#11-package-dependencies)

---

## 1. Overview

This project estimates the population exposed to river flooding using:

- **Flood hazard data** — Copernicus GLOFAS return-period depth rasters (RP30, RP100, RP500)
- **Population data** — WorldPop age/sex-disaggregated rasters at 100 m or 1 km resolution
- **Administrative boundaries** — GeoBoundaries or GADM (admin0, admin1, admin2)

The analysis pipeline is split into four reusable modules and three thin orchestration scripts:

| Module | Purpose |
|---|---|
| `R/flood_utils.R` | Tile selection, mosaic building, binary reclassification, clipping, aggregation |
| `R/pop_utils.R` | WorldPop filename parsing, population raster clipping |
| `R/exposure_utils.R` | Exposure computation, zonal statistics, indicator table building |
| `R/viz_utils.R` | Choropleth maps, trend histograms, population pyramids |

| Script | Purpose |
|---|---|
| `main.R` | Kenya (KEN) — admin2 flood exposure for women 15–49 |
| `main_exposure_pyramid.R` | Age/sex population pyramid of flood exposure |
| `main_by_country.R` | Multi-country loop using the same pipeline |

---

## 2. Project Structure

```
climate_change/
├── R/
│   ├── flood_utils.R          ← flood processing functions
│   ├── pop_utils.R            ← population metadata functions
│   ├── exposure_utils.R       ← exposure computation functions
│   └── viz_utils.R            ← visualisation functions
├── tests/
│   ├── run_tests.R            ← test runner (source this to run all tests)
│   └── testthat/
│       ├── test-flood_utils.R
│       ├── test-pop_utils.R
│       ├── test-exposure_utils.R
│       └── test-viz_utils.R
├── docs/
│   └── MANUAL.md              ← this document
├── input/
│   ├── flood_layers_RP100/    ← downloaded flood tiles (.tif)
│   ├── flood_tiles/           ← tile index (tile_extents.geojson)
│   ├── geoboundaries/         ← admin boundaries (ADM0, ADM1, ADM2)
│   └── pop/                   ← WorldPop rasters by country & year
├── output/
│   ├── vrt_flood/             ← auto-generated VRT mosaics
│   ├── zonal_stats/           ← Excel indicator outputs
│   └── flood_maps/            ← map and chart outputs
├── main.R
├── main_exposure_pyramid.R
└── main_by_country.R
```

---

## 3. How to Load the Functions

All utility functions are loaded with `source()` at the top of each main script. When working interactively, source them manually:

```r
source("R/flood_utils.R")
source("R/pop_utils.R")
source("R/exposure_utils.R")
source("R/viz_utils.R")
```

> **Note:** Always run R from the project root directory so that relative paths (`"input/"`, `"output/"`) resolve correctly.
> In RStudio: *Session > Set Working Directory > To Project Directory*

---

## 4. Module: `R/flood_utils.R`

Handles all flood raster processing: tile selection, mosaic construction, binary reclassification, spatial clipping, resolution aggregation, and data download.

---

### `fld_select_tiles`

**Select flood raster tile files that cover a country.**

Intersects the country boundary with the global tile index and returns the paths of raster files whose tile codes match the intersection.

#### Signature

```r
fld_select_tiles(ctry_shp, v_tiles_path, r_tiles_dir, return_period = 100)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `ctry_shp` | `sf` | Country boundary polygon (admin0). |
| `v_tiles_path` | `character` | Path to the tile-index GeoJSON or shapefile. Must have `id` and `name` columns. |
| `r_tiles_dir` | `character` | Directory containing the downloaded flood raster tiles. |
| `return_period` | `integer` | Flood return period in years. Default `100`. Filters files matching `_RP{return_period}_depth.tif`. |

#### Returns

`character` vector of matched file paths.

#### Errors

- `"No flood tiles intersect the provided country boundary."` — the country geometry does not overlap any tile in the index.
- `"No flood raster files matched tile codes for return period RP..."` — tiles intersect the boundary but no matching `.tif` files exist in `r_tiles_dir`.

#### Example

```r
ctry_shp   <- sf::read_sf("input/geoboundaries/geoBoundariesCGAZ_ADM0/...shp") |>
  dplyr::filter(shapeGroup == "KEN")

tile_files <- fld_select_tiles(
  ctry_shp      = ctry_shp,
  v_tiles_path  = "input/flood_tiles/tile_extents.geojson",
  r_tiles_dir   = "input/flood_layers_RP100/",
  return_period = 100
)
# [1] "input/flood_layers_RP100/ID42_E036N-004_RP100_depth.tif"
# [2] "input/flood_layers_RP100/ID43_E036N-005_RP100_depth.tif"
```

---

### `fld_build_vrt`

**Build a Virtual Raster (VRT) mosaic from individual flood tile files.**

Wraps `terra::vrt()`. The parent directory of `vrt_path` is created automatically if it does not exist.

#### Signature

```r
fld_build_vrt(tile_paths, vrt_path, overwrite = TRUE)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `tile_paths` | `character` vector | Paths to raster tile `.tif` files. |
| `vrt_path` | `character` | Full output path for the `.vrt` file. |
| `overwrite` | `logical` | Whether to overwrite an existing VRT. Default `TRUE`. |

#### Returns

A `SpatRaster` (terra) object pointing at the created VRT.

#### Errors

- `"'tile_paths' must contain at least one file."` — empty input vector.

#### Example

```r
rp100 <- fld_build_vrt(
  tile_paths = tile_files,
  vrt_path   = "output/vrt_flood/KEN_RP100_depth.vrt"
)
```

---

### `fld_binarise`

**Reclassify a flood depth raster to binary (flooded / not flooded).**

Applies a depth threshold: cells ≥ threshold become 1 (flooded), cells < threshold become 0 (not flooded). NA values are preserved.

#### Signature

```r
fld_binarise(flood_ras, threshold = 0.1)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `flood_ras` | `SpatRaster` | Flood depth raster. Units are assumed to be metres. |
| `threshold` | `numeric` | Depth threshold in metres. Default `0.1` (10 cm). |

#### Returns

Binary `SpatRaster`: `1` = flooded, `0` = not flooded, `NA` preserved.

#### Example

```r
rp100_bin <- fld_binarise(rp100, threshold = 0.1)
```

#### Notes

- The GLOFAS RP100 dataset uses metres as depth units.
- The 0.1 m threshold is the standard UNFPA/UNICEF definition for significant flood exposure.

---

### `fld_clip`

**Clip a raster to a spatial boundary (crop then mask).**

The boundary is automatically reprojected to the raster's CRS before clipping.

#### Signature

```r
fld_clip(ras, boundary)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `ras` | `SpatRaster` | Input raster to clip. |
| `boundary` | `sf` or `SpatVector` | Boundary polygon. Reprojected automatically to the raster CRS. |

#### Returns

Clipped and masked `SpatRaster`. Cells outside the boundary become `NA`.

#### Example

```r
rp100_bin_clip <- fld_clip(rp100_bin, ctry_admin0)
```

---

### `fld_aggregate_to_pop`

**Aggregate a fine-resolution binary flood raster to the population raster resolution.**

Computes the **fraction** of each population cell classified as flooded (mean of the fine binary cells), then resamples to align exactly to the population grid. NA cells in the flood raster are treated as 0 (not flooded) before aggregation, preventing inflated fractions at coastlines and borders.

#### Signature

```r
fld_aggregate_to_pop(flood_bin_ras, pop_ras)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `flood_bin_ras` | `SpatRaster` | Binary flood raster (0/1) at fine resolution (e.g. 100 m). |
| `pop_ras` | `SpatRaster` | Population raster that defines the target grid (e.g. 1 km). |

#### Returns

`SpatRaster` of flood fraction at population raster resolution. Values are in **[0, 1]**, representing the proportion of each 1 km cell that is classified as flooded.

#### Errors

- `"fact < 2: flood raster is not finer than population raster."` — the flood raster resolution is equal to or coarser than the population raster. Check that both rasters are in a compatible CRS before calling this function.

#### Example

```r
# Compute once before the year loop — flood doesn't change per year
pop_ref    <- terra::rast(pop_files[[1]])
pop_ref_c  <- pop_clip(pop_ref, ctry_admin0)
flood_frac <- fld_aggregate_to_pop(rp100_bin_clip, pop_ref_c)
```

#### Notes

- This function should be called **once** before looping over population years. The resulting `flood_frac` raster can be reused for all years since the flood hazard is static.
- CRS alignment between `flood_bin_ras` and `pop_ras` is handled internally.

---

### `fld_download_tiles`

**Download flood raster tiles from the Copernicus GLOFAS server.**

Reads the directory listing HTML at `base_url`, extracts all `.tif` links, and downloads each file. Already-downloaded files are skipped.

#### Signature

```r
fld_download_tiles(base_url, out_dir, return_period = 100)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `base_url` | `character` | URL of the server directory listing (must end with `/`). |
| `out_dir` | `character` | Local directory where tiles will be saved. Created if it does not exist. |
| `return_period` | `integer` | Used in the status message only. Default `100`. |

#### Returns

`NULL` invisibly. Files are written to `out_dir`.

#### Example

```r
fld_download_tiles(
  base_url      = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/CEMS-GLOFAS/flood_hazard/RP100/",
  out_dir       = "input/flood_layers_RP100/",
  return_period = 100
)
```

---

## 5. Module: `R/pop_utils.R`

Handles WorldPop raster metadata extraction and spatial clipping.

---

### `pop_parse_filename`

**Parse metadata from a single WorldPop raster filename.**

Extracts sex, age group, year, and resolution from the WorldPop naming convention:

```
{iso3}_{sex_code}[_{age}]_{year}_{CN}_{resolution}_...
```

#### Signature

```r
pop_parse_filename(file_path)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `file_path` | `character` | Full path or basename of a WorldPop raster file. |

#### Returns

A one-row `tibble` with columns:

| Column | Type | Description |
|---|---|---|
| `file` | `character` | Original `file_path` as provided. |
| `sex_code` | `character` | Raw code from filename: `"f"`, `"m"`, `"t"`, `"T_F"`, or `"T_M"`. |
| `age_group` | `character` | Two-digit lower bound of age group (e.g. `"15"` for 15–19), or `NA` for total-sex files. |
| `year` | `integer` | Population reference year. |
| `resolution` | `character` | Spatial resolution string, e.g. `"100m"` or `"1km"`. |
| `sex` | `character` | Human-readable label derived from `sex_code` (see table below). |

**`sex_code` → `sex` mapping:**

| `sex_code` | `sex` |
|---|---|
| `"f"` | `"female"` |
| `"m"` | `"male"` |
| `"t"` | `"total"` |
| `"T_F"` | `"female_total"` |
| `"T_M"` | `"male_total"` |
| other | `NA` |

#### Example

```r
pop_parse_filename("ken_f_15_2025_UN_100m_population_v1.tif")
# # A tibble: 1 × 6
#   file                                  sex_code age_group  year resolution sex
#   <chr>                                 <chr>    <chr>     <int> <chr>      <chr>
# 1 ken_f_15_2025_UN_100m_population_v1… f        15         2025 100m       female

pop_parse_filename("gmb_T_F_2022_CN_100m_population_v1.tif")
# # A tibble: 1 × 6
#   file      sex_code age_group  year resolution sex
#   <chr>     <chr>    <chr>     <int> <chr>      <chr>
# 1 gmb_T_F… T_F      NA         2022 100m       female_total
```

---

### `pop_parse_filenames`

**Parse metadata from multiple WorldPop raster filenames.**

Vectorised wrapper around [`pop_parse_filename`](#pop_parse_filename).

#### Signature

```r
pop_parse_filenames(file_paths)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `file_paths` | `character` vector | Paths to WorldPop raster files. |

#### Returns

`tibble` with one row per file and the same columns as `pop_parse_filename`.

#### Example

```r
pop_files <- dir("input/pop/gambia/", full.names = TRUE)
pop_meta  <- pop_parse_filenames(pop_files)
# # A tibble: 18 × 6
#   file              sex_code age_group  year resolution sex
#   ...
```

---

### `pop_clip`

**Clip a population raster to a spatial boundary (crop + mask).**

The boundary is automatically reprojected to the raster CRS.

#### Signature

```r
pop_clip(pop_ras, boundary)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `pop_ras` | `SpatRaster` | Population raster. |
| `boundary` | `sf` or `SpatVector` | Boundary polygon. Reprojected automatically. |

#### Returns

Clipped and masked `SpatRaster`.

#### Example

```r
pop_c <- pop_clip(terra::rast(pop_files[[1]]), ctry_admin0)
```

---

## 6. Module: `R/exposure_utils.R`

Handles the core exposure computation chain: from rasters to zonal statistics to standardised indicator tables.

---

### `exp_compute`

**Compute exposed population from a population raster and a flood fraction raster.**

Performs elementwise multiplication: `pop_exposed = pop_ras × flood_frac_ras`. Each cell of `flood_frac_ras` represents the fraction of that cell covered by flooding (values in [0, 1]).

#### Signature

```r
exp_compute(pop_ras, flood_frac_ras,
            pop_layer_name     = "pop_total",
            exposed_layer_name = "pop_exposed")
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `pop_ras` | `SpatRaster` | Population counts per cell. Both rasters must share the same grid. |
| `flood_frac_ras` | `SpatRaster` | Flood fraction raster at population resolution (output of `fld_aggregate_to_pop`). |
| `pop_layer_name` | `character` | Layer name for the total population output. Default `"pop_total"`. |
| `exposed_layer_name` | `character` | Layer name for the exposure output. Default `"pop_exposed"`. |

#### Returns

Named `list` with two `SpatRaster` elements:

| Element | Description |
|---|---|
| `pop_total` | Total population raster (renamed layer). |
| `pop_exposed` | Exposed population raster (= `pop_ras × flood_frac_ras`). |

#### Example

```r
pops <- exp_compute(pop_c, flood_frac)
terra::global(pops$pop_exposed, sum, na.rm = TRUE)  # total exposed population
```

---

### `exp_zonal_stats`

**Compute zonal sums of exposed and total population at an administrative level.**

Runs `terra::zonal(sum)` twice against the same zone polygons (once for exposed, once for total), joins the results, and computes the percentage exposed.

#### Signature

```r
exp_zonal_stats(exposed_ras, pop_ras, zones)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `exposed_ras` | `SpatRaster` | Exposed population raster. Layer must be named `"pop_exposed"`. |
| `pop_ras` | `SpatRaster` | Total population raster. Layer must be named `"pop_total"`. |
| `zones` | `sf` or `SpatVector` | Zone polygons (e.g. admin2 boundaries). |

#### Returns

`sf` data frame containing all zone attribute columns plus:

| Added column | Description |
|---|---|
| `pop_exposed` | Sum of exposed population within each zone. |
| `pop_total` | Sum of total population within each zone. |
| `perc_exposed` | `(pop_exposed / pop_total) × 100`. |

#### Example

```r
zonal <- exp_zonal_stats(pops$pop_exposed, pops$pop_total, admin2_ctry)
head(zonal[, c("shapeName", "pop_total", "pop_exposed", "perc_exposed")])
```

---

### `exp_build_indicator_tibble`

**Build a standardised indicator tibble from zonal statistics.**

Converts the output of `exp_zonal_stats` into the project's canonical indicator format, one row per administrative zone.

#### Signature

```r
exp_build_indicator_tibble(zonal_sf, year, indicator_name,
                            indicator_code, admin_level = 2L)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `zonal_sf` | `sf` | Output of `exp_zonal_stats`. Must have `shapeGroup`, `shapeID`, `shapeName`, `pop_total`, `pop_exposed`, `perc_exposed`. |
| `year` | `integer` | Population reference year. |
| `indicator_name` | `character` | Human-readable indicator description. |
| `indicator_code` | `character` | Short code (e.g. `"wraf100"` = Women of Reproductive Age exposed to Floods, RP100). |
| `admin_level` | `integer` | Administrative level of `zonal_sf`. Default `2L`. |

#### Returns

`tibble` with the following standardised columns:

| Column | Type | Description |
|---|---|---|
| `id` | `integer` | Row identifier (`NA` at creation; filled by `exp_run_country`). |
| `indicator.name` | `character` | Human-readable indicator name. |
| `indicator.code` | `character` | Short indicator code. |
| `country.code` | `character` | ISO3 country code (from `shapeGroup`). |
| `admin.level` | `integer` | Administrative level. |
| `admin.code` | `character` | Admin zone unique ID (from `shapeID`). |
| `admin.name` | `character` | Admin zone name (from `shapeName`). |
| `year` | `integer` | Reference year. |
| `total.pop` | `numeric` | Total population in zone. |
| `pop.exposed` | `numeric` | Exposed population in zone. |
| `perc.pop.exposed` | `numeric` | Percentage of population exposed. |

#### Example

```r
year_df <- exp_build_indicator_tibble(
  zonal_sf       = zonal,
  year           = 2025L,
  indicator_name = "Women 15-49 exposed to RP100 floods",
  indicator_code = "wraf100",
  admin_level    = 2L
)
```

---

### `exp_summarise_by_country`

**Aggregate admin-level indicators to country level.**

Groups by `country.code × year × indicator.name`, sums `total.pop` and `pop.exposed`, and recomputes `perc.pop.exposed`.

#### Signature

```r
exp_summarise_by_country(indicators)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `indicators` | `tibble` | Output from `exp_build_indicator_tibble` (one or more years stacked). |

#### Returns

`tibble` with one row per `country.code × year × indicator.name`:

| Column | Description |
|---|---|
| `country.code` | ISO3 country code. |
| `year` | Reference year. |
| `indicator.name` | Indicator description. |
| `total.pop` | National total population. |
| `pop.exposed` | National exposed population. |
| `perc.pop.exposed` | `(pop.exposed / total.pop) × 100`. |

#### Example

```r
ctry_indicators <- exp_summarise_by_country(indicators)
```

---

### `exp_run_country`

**Run the full flood exposure pipeline for a country across multiple population years.**

This is the primary orchestration function. It loops over population files, clips each to the country boundary, multiplies by the pre-built flood fraction raster, computes zonal statistics at the specified admin level, and accumulates results into a standardised indicator table.

#### Signature

```r
exp_run_country(pop_files, flood_frac_ras, ctry_boundary, admin_zones,
                indicator_name = "Women 15-49 exposed to RP100 floods",
                indicator_code = "wraf100",
                admin_level    = 2L)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `pop_files` | `character` vector | Paths to population rasters, one per year. Years are extracted from the filename (first four-digit number). |
| `flood_frac_ras` | `SpatRaster` | Flood fraction raster at population resolution (output of `fld_aggregate_to_pop`). Computed once and reused for all years. |
| `ctry_boundary` | `sf` or `SpatVector` | Country boundary (admin0) used to clip each population raster. |
| `admin_zones` | `sf` or `SpatVector` | Admin-level zones for zonal statistics (e.g. admin2). |
| `indicator_name` | `character` | Indicator description. Default `"Women 15-49 exposed to RP100 floods"`. |
| `indicator_code` | `character` | Short indicator code. Default `"wraf100"`. |
| `admin_level` | `integer` | Administrative level of `admin_zones`. Default `2L`. |

#### Returns

`tibble` of standardised indicators with a sequential `id` column filled. One block of rows per year per zone.

#### Example

```r
indicators <- exp_run_country(
  pop_files      = list.files("input/pop/", full.names = TRUE),
  flood_frac_ras = flood_frac,
  ctry_boundary  = ctry_admin0,
  admin_zones    = admin2_ctry,
  indicator_name = "Women 15-49 exposed to RP100 floods",
  indicator_code = "wraf100",
  admin_level    = 2L
)
```

---

## 7. Module: `R/viz_utils.R`

Produces all visualisation outputs: choropleth maps, trend histograms, and population pyramids.

---

### `viz_choropleth`

**Create a faceted choropleth map of flood exposure.**

Produces one map panel per year showing `perc.pop.exposed` at the admin level, with a country boundary outline, scale bar, and north arrow.

#### Signature

```r
viz_choropleth(admin_sf, ctry_sf, ctry_code, out_path = NULL, scale = 2)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `admin_sf` | `sf` | Admin boundaries joined to indicator data. Must have columns `perc.pop.exposed` and `year`. |
| `ctry_sf` | `sf` | Country boundary (admin0) drawn as a black outline on top. |
| `ctry_code` | `character` | ISO3 country code used in the map title. |
| `out_path` | `character` or `NULL` | If provided, the plot is saved to this path (JPG recommended). Parent directory created automatically. |
| `scale` | `numeric` | `ggsave` scale factor. Default `2`. |

#### Returns

A `ggplot` object. Returned invisibly when `out_path` is not `NULL`.

#### Example

```r
viz_choropleth(
  admin_sf  = admin2_join,
  ctry_sf   = ctry_admin0,
  ctry_code = "KEN",
  out_path  = "output/flood_maps/KEN_river_flood_exposure.jpg"
)
```

---

### `viz_trend_histogram`

**Create a dual-axis bar + line trend chart of flood exposure over time.**

Bars show the absolute exposed population; the overlaid line and points show the percentage exposed (right axis). The secondary axis is scaled to align visually with the bars.

#### Signature

```r
viz_trend_histogram(ctry_indicators, out_path = NULL,
                     width = 10, height = 8, scale = 1.2)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `ctry_indicators` | `tibble` | Country-level indicators from `exp_summarise_by_country`. Must have `year`, `pop.exposed`, `perc.pop.exposed`, `country.code`. |
| `out_path` | `character` or `NULL` | If provided, the plot is saved here. |
| `width` | `numeric` | Plot width in inches. Default `10`. |
| `height` | `numeric` | Plot height in inches. Default `8`. |
| `scale` | `numeric` | `ggsave` scale factor. Default `1.2`. |

#### Returns

A `ggplot` object. Returned invisibly when `out_path` is not `NULL`.

#### Example

```r
viz_trend_histogram(
  ctry_indicators = ctry_indicators,
  out_path        = "output/flood_maps/trend/KEN_river_flood_exposure_hist.jpg"
)
```

---

### `viz_prep_pyramid_data`

**Prepare exposure data for a mirrored population pyramid.**

Filters to `"male"` and `"female"` rows only, computes `perc_exposed`, and **negates** all female values (`pop_exposed`, `pop_tot`, `perc_exposed`) so the pyramid is mirrored — females appear on the left, males on the right.

#### Signature

```r
viz_prep_pyramid_data(exposure_stat)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `exposure_stat` | `tibble` | Exposure summary table. Must have columns `sex`, `pop_exposed`, `pop_tot`, `age_group`, `year`. |

#### Returns

Filtered and transformed `tibble` ready for `viz_exposure_pyramid`. Female values are negative; male values are positive.

#### Example

```r
pyramid_data <- viz_prep_pyramid_data(exposure_stat)
```

---

### `viz_exposure_pyramid`

**Create a stacked population pyramid of flood exposure.**

Produces a two-panel `patchwork` figure:

- **Top panel:** absolute exposed population count (in thousands), by age group and sex.
- **Bottom panel:** percentage of each age/sex group exposed.

Both panels are faceted by year.

#### Signature

```r
viz_exposure_pyramid(pyramid_data, ctry_code, out_path = NULL,
                      width = 15, height = 10, dpi = 600)
```

#### Parameters

| Parameter | Type | Description |
|---|---|---|
| `pyramid_data` | `tibble` | Output of `viz_prep_pyramid_data`. Must have `pop_exposed`, `perc_exposed`, `age_group`, `sex`, `year`. |
| `ctry_code` | `character` | ISO3 country code used in the plot title. |
| `out_path` | `character` or `NULL` | If provided, the combined plot is saved here. |
| `width` | `numeric` | Plot width in inches. Default `15`. |
| `height` | `numeric` | Plot height in inches. Default `10`. |
| `dpi` | `numeric` | Output resolution in dots per inch. Default `600`. |

#### Returns

A `patchwork` / `ggplot` object. Returned invisibly when `out_path` is not `NULL`.

#### Example

```r
viz_exposure_pyramid(
  pyramid_data = pyramid_data,
  ctry_code    = "KEN",
  out_path     = "output/flood_maps/exposure_pyramid.jpg"
)
```

---

## 8. Full Analysis Workflow

The diagram below shows the complete pipeline and which function handles each step:

```
Input files
    │
    ├── Tile index (.geojson)  ─────────────────────────┐
    ├── Flood rasters (.tif)   ──────────────────────┐  │
    │                                                │  │
    │                                      fld_select_tiles()
    │                                                │
    │                                      fld_build_vrt()
    │                                                │
    │                                      fld_binarise()
    │                                                │
    ├── Country boundary (admin0) ──────── fld_clip()
    │                                                │
    │                          ┌─── fld_aggregate_to_pop() ◄── pop_clip() ──┐
    │                          │    (computed once)                          │
    ├── Population rasters ────┘                     │              pop rasters
    │   (one per year)                               │
    │                          ┌─── pop_clip() ──────┘
    │                          │   (per year)
    │                          │
    │                          └─── exp_compute()
    │                                    │
    ├── Admin zones (admin2) ──── exp_zonal_stats()
    │                                    │
    │                          exp_build_indicator_tibble()
    │                                    │
    │                          exp_summarise_by_country()
    │                                    │
    │              ┌─────────────────────┤
    │              │                     │
    │         viz_choropleth()  viz_trend_histogram()
    │
    └── (for pyramid analysis)
        pop_parse_filenames() ──► exp_compute() ──► viz_prep_pyramid_data()
                                                          │
                                               viz_exposure_pyramid()
```

---

## 9. Running Tests

The project uses the `testthat` framework. Tests do not require a package structure — all utility functions are sourced before tests run.

### Run all tests

```r
# From RStudio (with working directory set to project root):
source("tests/run_tests.R")

# From terminal:
Rscript tests/run_tests.R
```

### Test files

| File | Functions tested |
|---|---|
| `tests/testthat/test-flood_utils.R` | `fld_binarise`, `fld_clip`, `fld_aggregate_to_pop`, `fld_build_vrt`, `fld_select_tiles` |
| `tests/testthat/test-pop_utils.R` | `pop_parse_filename`, `pop_parse_filenames`, `pop_clip` |
| `tests/testthat/test-exposure_utils.R` | `exp_compute`, `exp_build_indicator_tibble`, `exp_summarise_by_country` |
| `tests/testthat/test-viz_utils.R` | `viz_prep_pyramid_data`, `viz_trend_histogram`, `viz_choropleth`, `viz_exposure_pyramid` |

### What is and is not tested

Tests use **in-memory synthetic data** (small `terra::rast()` and `sf` objects) so no input files are needed to run the test suite. Functions that require actual flood raster files on disk (`fld_select_tiles`, `exp_run_country`, `exp_zonal_stats`) have integration-style guards using `skip_if_not(file.exists(...))`.

---

## 10. Data Sources & Input File Conventions

### Flood tiles

- **Source:** Copernicus GLOFAS / JRC
- **Format:** GeoTIFF (`.tif`), one tile per geographic region
- **Filename pattern:** `ID{n}_{region}_RP{period}_depth.tif`
  e.g. `ID42_E036N-004_RP100_depth.tif`
- **Tile index:** `input/flood_tiles/tile_extents.geojson` — polygon layer with `id` and `name` columns

### Population rasters

- **Source:** WorldPop (open.worldpop.org)
- **Format:** GeoTIFF, 100 m or 1 km resolution
- **Filename pattern:** `{iso3}_{sex_code}[_{age}]_{year}_{CN}_{resolution}_...tif`
  e.g. `ken_f_15_2025_UN_100m_population_v1.tif`

### Administrative boundaries

- **Source:** GeoBoundaries (geoboundaries.org) or GADM (gadm.org)
- **Format:** Shapefile (`.shp`) with `shapeGroup` (ISO3), `shapeID`, `shapeName` columns
- **Levels:** ADM0 (country), ADM1 (region), ADM2 (district)

---

## 11. Package Dependencies

All packages are managed via `renv`. Install missing packages with `pacman::p_load(...)`.

| Package | Purpose |
|---|---|
| `terra` | Raster processing (crop, mask, aggregate, zonal, vrt) |
| `sf` | Vector data handling (read, transform, intersect) |
| `dplyr` | Data manipulation |
| `stringr` | String operations (filename parsing, regex) |
| `tibble` / `purrr` | Tidy data structures and functional programming |
| `ggplot2` | Core plotting |
| `ggspatial` | Map scale bars and north arrows |
| `patchwork` | Combining multiple ggplot panels |
| `scales` | Number formatting (comma labels) |
| `openxlsx` | Excel export |
| `cli` | Progress bars in exposure loops |
| `testthat` | Unit testing framework |

---

*End of manual.*
