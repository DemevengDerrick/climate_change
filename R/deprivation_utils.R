#################################################################################################
#                                                                                               #
#                       DEPRIVATION INDEX UTILITY FUNCTIONS                                     #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Source: CIESIN/SEDAC Global Relative Deprivation Index (GRDI) v1, 2010-2020                 #
#  Index range: 0 (least deprived) to 100 (most deprived)                                      #
#                                                                                               #
#  Threshold rationale (dep_binarise default = 50):                                             #
#  The GRDI is a relative index — a score of 50 marks the global median deprivation             #
#  level. Cells scoring >= 50 are considered "relatively deprived" and are used to              #
#  identify vulnerable populations for climate risk analysis.  This aligns with                 #
#  UNDP and UN-Habitat practice of using the upper half of multidimensional                     #
#  deprivation distributions as a programmatic targeting threshold                              #
#  (cf. Kummu et al. 2016; CIESIN GRDI v1 Technical Documentation, 2022).                      #
#  Users may raise this threshold (e.g. 60 = moderately-highly deprived top 40%;               #
#  80 = highly deprived top 20%) depending on the targeting stringency required.                #
#                                                                                               #
#################################################################################################


#' Align the GRDI raster to a population raster grid
#'
#' Loads the GRDI GeoTIFF, reprojects it to the population raster CRS, and
#' resamples it to exactly match the population raster's extent and resolution
#' using bilinear interpolation (appropriate for a continuous index).
#'
#' @param grdi_path Character. Path to the GRDI `.tif` file.
#' @param pop_ras   SpatRaster. Reference population raster (defines target grid).
#'
#' @return SpatRaster aligned to `pop_ras` grid, values 0–100.
#' @export
dep_align_to_pop <- function(grdi_path, pop_ras) {
  grdi <- terra::rast(grdi_path)

  if (!terra::same.crs(grdi, pop_ras))
    grdi <- terra::project(grdi, pop_ras, method = "bilinear")

  terra::resample(grdi, pop_ras, method = "bilinear")
}


#' Binarise an aligned GRDI raster into a vulnerability mask
#'
#' Cells with GRDI >= `threshold` are coded 1 (vulnerable); all others 0.
#' NA cells (water bodies, data gaps) are preserved as NA.
#'
#' This has no effect on zonal statistics: `terra::zonal(..., na.rm = TRUE)`
#' treats NA and 0 identically (both contribute 0 to sums).  Preserving NA is
#' preferred for visualisation — NA renders as transparent (no data) on maps,
#' which is more honest than colouring data-gap cells as "not deprived".
#'
#' @param grdi_ras  SpatRaster. Aligned GRDI raster (from [dep_align_to_pop()]).
#' @param threshold Numeric. GRDI score above which a cell is considered
#'   vulnerable (default `50`; see file-level documentation for rationale).
#'
#' @return Binary SpatRaster: 1 = vulnerable, 0 = not vulnerable, NA preserved.
#' @export
dep_binarise <- function(grdi_ras, threshold = 50) {
  terra::ifel(grdi_ras >= threshold, 1L, 0L)
}


#' Apply a deprivation mask to a population raster
#'
#' Multiplies the population raster by the binary deprivation mask, retaining
#' only the population in cells classified as vulnerable.
#'
#' @param pop_ras  SpatRaster. Population raster.
#' @param dep_mask SpatRaster. Binary mask (1 = vulnerable, 0 = not) from
#'   [dep_binarise()].
#'
#' @return SpatRaster of vulnerable population counts.
#' @export
dep_apply_to_pop <- function(pop_ras, dep_mask) {
  pop_ras * dep_mask
}


#' Full deprivation pre-processing pipeline
#'
#' Convenience wrapper: loads GRDI, aligns to population grid, binarises at
#' threshold, and returns the ready-to-use mask.
#'
#' @param grdi_path Character. Path to the GRDI `.tif` file.
#' @param pop_ras   SpatRaster. Reference population raster.
#' @param threshold Numeric. Vulnerability threshold (default `50`).
#'
#' @return Binary SpatRaster (1 = vulnerable, 0 = not vulnerable, NA preserved).
#' @export
dep_build_mask <- function(grdi_path, pop_ras, threshold = 50) {
  message("Loading and aligning GRDI deprivation index ...")
  grdi_aligned <- dep_align_to_pop(grdi_path, pop_ras)
  message("  -> Binarising at threshold GRDI >= ", threshold)
  dep_binarise(grdi_aligned, threshold)
}
