#################################################################################################
#                                                                                               #
#                          WORD REPORT GENERATION UTILITIES                                     #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

#' Generate a management-summary Word document for a flood exposure analysis
#'
#' Produces a non-technical summary report covering methodology, input data,
#' processing steps, and all output images. Suitable for a management audience
#' that needs to understand the analysis without deep technical knowledge.
#'
#' @param ctry_code       Character. ISO3 country code (e.g. `"KEN"`).
#' @param ctry_name       Character. Full country name (e.g. `"Kenya"`).
#' @param hazard_name     Character. Full hazard name (e.g. `"River Flood"`).
#' @param return_period   Integer. Return period in years (e.g. `100`).
#' @param year            Integer. Population reference year (e.g. `2020`).
#' @param ctry_indicators Tibble. Country-level indicators from
#'   [exp_summarise_by_country()].
#' @param img_hazard      Character. Path to the hazard layer map image.
#' @param img_population  Character. Path to the population layer map image.
#' @param img_overlay     Character. Path to the hazard-on-population overlay image.
#' @param img_choropleth  Character. Path to the faceted choropleth map image.
#' @param img_bars        Character. Path to the bar chart image.
#' @param img_dotplot     Character. Path to the dot plot image.
#' @param img_bubble      Character. Path to the bubble chart image (optional).
#' @param out_path        Character. Full file path for the output `.docx`.
#' @param author          Character. Report author name (default `"UNFPA"`).
#'
#' @return Invisibly returns the `officer` document object.
#' @export
rpt_generate_word <- function(ctry_code, ctry_name, hazard_name, return_period,
                               year, ctry_indicators,
                               ctry_indicators_vuln = NULL,
                               img_hazard        = NULL,
                               img_population    = NULL,
                               img_overlay       = NULL,
                               img_deprivation   = NULL,
                               img_workflow      = NULL,
                               img_choropleth    = NULL,
                               img_scenario_chart = NULL,
                               img_top5_heatmap  = NULL,
                               img_bars          = NULL,
                               img_dotplot       = NULL,
                               img_bubble        = NULL,
                               dep_threshold     = 50,
                               out_path,
                               author = "UNFPA") {

  if (!requireNamespace("officer", quietly = TRUE))
    stop("Package 'officer' is required. Install with: install.packages('officer')")
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Package 'flextable' is required. Install with: install.packages('flextable')")

  doc <- officer::read_docx()

  # ------------------------------------------------------------------
  # STYLES helper (officer uses built-in Word styles)
  # ------------------------------------------------------------------
  add_h1 <- function(doc, text) {
    officer::body_add_par(doc, text, style = "heading 1")
  }
  add_h2 <- function(doc, text) {
    officer::body_add_par(doc, text, style = "heading 2")
  }
  add_body <- function(doc, text) {
    officer::body_add_par(doc, text, style = "Normal")
  }
  add_blank <- function(doc) {
    officer::body_add_par(doc, "", style = "Normal")
  }
  add_img <- function(doc, path, width = 5.5, height = 4.5, caption = NULL) {
    if (!is.null(path) && file.exists(path)) {
      doc <- officer::body_add_img(doc, src = path, width = width, height = height)
      if (!is.null(caption)) {
        doc <- officer::body_add_par(
          doc,
          paste0("Figure: ", caption),
          style = "Normal"
        )
      }
    }
    doc
  }

  # ------------------------------------------------------------------
  # TITLE PAGE
  # ------------------------------------------------------------------
  doc <- officer::body_add_par(
    doc,
    paste0("Climate Vulnerability and Flood Exposure Analysis"),
    style = "heading 1"
  )
  doc <- officer::body_add_par(
    doc,
    paste0(ctry_name, " \u2013 ", hazard_name, " (1-in-", return_period, "-year event)"),
    style = "heading 2"
  )
  doc <- add_body(doc, paste0("Prepared by: ", author))
  doc <- add_body(doc, paste0("Reference year: ", year))
  doc <- add_body(doc, paste0("Date: ", format(Sys.Date(), "%B %Y")))
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # SECTION 1 — BACKGROUND
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "1. Why This Analysis Matters")
  doc <- add_body(doc,
    paste0(
      "Climate change is increasing the frequency and severity of extreme weather events, ",
      "including floods. For UNFPA's programmes in ", ctry_name, ", understanding ",
      "which communities and population groups are most exposed to flood risk is essential ",
      "for planning life-saving services, targeting resources, and advocating for the most ",
      "vulnerable."
    )
  )
  doc <- add_blank(doc)
  doc <- add_body(doc,
    paste0(
      "This report presents a climate vulnerability and flood exposure analysis for ",
      ctry_name, ". It goes beyond simply mapping who lives near floods: it first identifies ",
      "the most deprived communities using an internationally recognised deprivation index, ",
      "and then assesses how many people within those deprived areas would be affected by a ",
      "major flood event. The analysis focuses on a '1-in-", return_period, "-year' flood, ",
      "meaning a flood so large that it has only a ", round(100 / return_period, 1),
      "% chance of occurring in any given year."
    )
  )
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # SECTION 2 — WHAT DATA WAS USED
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "2. What Data Was Used")
  doc <- add_body(doc,
    "Three datasets were combined for this analysis:"
  )
  doc <- add_blank(doc)

  # Data sources table
  sources_df <- data.frame(
    Dataset = c(
      paste0(hazard_name, " Hazard Map"),
      "Population Data (WorldPop)",
      "Deprivation Index (GRDI)"
    ),
    Source = c(
      "European Commission \u2013 Copernicus Emergency Management Service (GloFAS)",
      "WorldPop, University of Southampton",
      "CIESIN/SEDAC, Columbia University"
    ),
    Description = c(
      paste0(
        "Shows which areas would be flooded during a 1-in-", return_period,
        "-year event. Each grid cell records the estimated water depth in metres."
      ),
      paste0(
        "Estimated number of people living in each 1 km\u00b2 grid cell, for ", year,
        ". Disaggregated by age group and sex."
      ),
      paste0(
        "The Global Relative Deprivation Index (GRDI) scores each 1 km\u00b2 cell from ",
        "0 (least deprived) to 100 (most deprived), combining sub-national indicators of ",
        "poverty, child mortality, sanitation access, and economic deprivation. Cells ",
        "scoring \u2265 ", dep_threshold, " are classified as 'vulnerable' for this analysis."
      )
    ),
    stringsAsFactors = FALSE
  )

  ft_sources <- flextable::flextable(sources_df)
  ft_sources <- flextable::set_header_labels(
    ft_sources, Dataset = "Dataset", Source = "Source", Description = "Description"
  )
  ft_sources <- flextable::bold(ft_sources, part = "header")
  ft_sources <- flextable::width(ft_sources, j = 1, width = 1.4)
  ft_sources <- flextable::width(ft_sources, j = 2, width = 2.0)
  ft_sources <- flextable::width(ft_sources, j = 3, width = 2.8)
  ft_sources <- flextable::theme_box(ft_sources)
  ft_sources <- flextable::fontsize(ft_sources, size = 9, part = "all")

  doc <- flextable::body_add_flextable(doc, ft_sources)
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # SECTION 3 — HOW THE ANALYSIS WAS DONE (methodology with images)
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "3. How the Analysis Was Done")
  doc <- add_body(doc,
    paste0(
      "The analysis combined three data streams in four steps. The workflow diagram below ",
      "provides a visual summary of the methodology before each step is described in detail."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_workflow, width = 6.0, height = 4.2,
                 caption = "Methodology workflow: from data inputs to climate vulnerability indicators")
  doc <- add_blank(doc)

  # Step 1 — Hazard
  doc <- add_h2(doc, "Step 1: Mapping the Flood Hazard")
  doc <- add_body(doc,
    paste0(
      "The first step was to identify which parts of ", ctry_name, " could be flooded. ",
      "The GloFAS flood model provides a detailed map showing the expected water depth ",
      "across the country during a 1-in-", return_period, "-year flood. Areas with water ",
      "depth greater than 10 centimetres were classified as 'flooded'. The map below shows ",
      "the proportion of each 1 km\u00b2 area that falls within the flood zone."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_hazard, width = 5.5, height = 4.5,
                 caption = paste0("Flood hazard extent \u2013 ", ctry_name,
                                  " (", hazard_name, ", RP", return_period, ")"))
  doc <- add_blank(doc)

  # Step 2 — Population
  doc <- add_h2(doc, "Step 2: Mapping Where People Live")
  doc <- add_body(doc,
    paste0(
      "The second step was to map the distribution of people across ", ctry_name, ". ",
      "WorldPop provides population estimates for every 1 km\u00b2 grid cell, disaggregated ",
      "by age group and sex. This allows the analysis to identify not just how many people ",
      "live in flood-prone areas, but which groups \u2014 such as women of reproductive age, ",
      "young children, or older adults \u2014 are most at risk."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_population, width = 5.5, height = 4.5,
                 caption = paste0("Total population distribution \u2013 ", ctry_name,
                                  " (", year, ", WorldPop 1 km)"))
  doc <- add_blank(doc)

  # Step 3 — Deprivation / Vulnerability filter
  doc <- add_h2(doc, "Step 3: Identifying the Most Deprived Communities")
  doc <- add_body(doc,
    paste0(
      "Not all people living near floods are equally vulnerable. Communities that are ",
      "already deprived \u2014 with limited access to healthcare, sanitation, and economic ",
      "resources \u2014 are far less able to prepare for, respond to, or recover from a ",
      "flood. To capture this dimension, the analysis uses the Global Relative Deprivation ",
      "Index (GRDI), a composite score produced by CIESIN/SEDAC at Columbia University. ",
      "Each 1 km\u00b2 cell receives a score from 0 (least deprived) to 100 (most deprived). ",
      "Cells scoring \u2265 ", dep_threshold, " \u2014 the upper half of the global deprivation ",
      "distribution \u2014 are classified as 'vulnerable'. Only the population living in these ",
      "cells is carried forward into the flood exposure step."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_deprivation, width = 6.0, height = 2.8,
                 caption = paste0("Left: GRDI deprivation scores | Right: vulnerability mask ",
                                  "(GRDI \u2265 ", dep_threshold, ") \u2014 ", ctry_name))
  doc <- add_blank(doc)

  # Step 4 — Overlay
  doc <- add_h2(doc, "Step 4: Combining Flood Hazard with Vulnerable Population")
  doc <- add_body(doc,
    paste0(
      "The final step overlays the flood hazard map with the vulnerable population ",
      "rasters. Where a flood zone overlaps with a deprived, populated area, those people ",
      "are counted as 'exposed vulnerable population'. The map below shows this overlap: ",
      "dark areas are where flooding, dense population, and high deprivation coincide \u2014 ",
      "representing the highest concentrations of climate-vulnerable people."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_overlay, width = 5.5, height = 4.5,
                 caption = paste0("Vulnerable population in flood-prone areas \u2013 overlay map, ",
                                  ctry_name))
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # SECTION 4 — RESULTS
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "4. Key Findings")

  # 4a — Country summary table
  doc <- add_h2(doc, "4.1 Population Exposed by Group and Scenario")
  doc <- add_body(doc,
    paste0(
      "The table below presents flood exposure for each demographic group under two scenarios: ",
      "(1) all group members regardless of deprivation status; and (2) only those living in ",
      "deprived areas (GRDI score \u2265 ", dep_threshold, "). For the deprived scenario, ",
      "the percentage is expressed as a share of the total group population, showing what ",
      "fraction of each group faces the combined burden of deprivation and flood risk."
    )
  )
  doc <- add_blank(doc)

  # Build combined table: raw exposure joined with vuln-vs-total exposure
  raw_tbl <- ctry_indicators |>
    dplyr::select(group.label, total.pop, pop.exposed, perc.pop.exposed) |>
    dplyr::rename(
      raw_total   = total.pop,
      raw_exposed = pop.exposed,
      raw_pct     = perc.pop.exposed
    )

  if (!is.null(ctry_indicators_vuln) && nrow(ctry_indicators_vuln) > 0) {
    vuln_tbl <- ctry_indicators_vuln |>
      dplyr::select(group.label, pop.exposed, perc.pop.exposed) |>
      dplyr::rename(
        vuln_exposed = pop.exposed,
        vuln_pct     = perc.pop.exposed
      )
    tbl_data <- dplyr::left_join(raw_tbl, vuln_tbl, by = "group.label")
  } else {
    tbl_data <- raw_tbl |>
      dplyr::mutate(vuln_exposed = NA_real_, vuln_pct = NA_real_)
  }

  tbl_data <- tbl_data |>
    dplyr::mutate(
      raw_total    = round(raw_total),
      raw_exposed  = round(raw_exposed),
      raw_pct      = round(raw_pct, 1),
      vuln_exposed = round(vuln_exposed),
      vuln_pct     = round(vuln_pct, 1)
    ) |>
    dplyr::rename(`Population Group` = group.label) |>
    as.data.frame()

  tbl_data[["raw_total"]]    <- format(tbl_data[["raw_total"]],
                                        big.mark = ",", scientific = FALSE)
  tbl_data[["raw_exposed"]]  <- format(tbl_data[["raw_exposed"]],
                                        big.mark = ",", scientific = FALSE)
  tbl_data[["vuln_exposed"]] <- format(tbl_data[["vuln_exposed"]],
                                        big.mark = ",", scientific = FALSE)

  ft_results <- flextable::flextable(tbl_data)
  ft_results <- flextable::set_header_labels(
    ft_results,
    `Population Group` = "Population Group",
    raw_total          = "Total Pop",
    raw_exposed        = "Exposed Pop\n(all)",
    raw_pct            = "% Exposed\n(all)",
    vuln_exposed       = "Deprived &\nExposed",
    vuln_pct           = "% Deprived\n& Exposed"
  )
  ft_results <- flextable::add_header_row(
    ft_results,
    values = c("", "All Population", "Deprived Population\n(GRDI \u2265 50)"),
    colwidths = c(1, 3, 2)
  )
  ft_results <- flextable::bold(ft_results, part = "header")
  ft_results <- flextable::merge_h(ft_results, part = "header")
  ft_results <- flextable::align(ft_results, align = "center", part = "header")
  ft_results <- flextable::width(ft_results, j = 1, width = 1.9)
  ft_results <- flextable::width(ft_results, j = 2, width = 0.8)
  ft_results <- flextable::width(ft_results, j = 3, width = 1.0)
  ft_results <- flextable::width(ft_results, j = 4, width = 0.8)
  ft_results <- flextable::width(ft_results, j = 5, width = 1.0)
  ft_results <- flextable::width(ft_results, j = 6, width = 0.8)
  ft_results <- flextable::theme_box(ft_results)
  ft_results <- flextable::fontsize(ft_results, size = 9, part = "all")
  ft_results <- flextable::align(ft_results, j = 2:6, align = "right", part = "body")
  ft_results <- flextable::bg(ft_results, i = 1, bg = "#dce6f0", part = "header")

  doc <- flextable::body_add_flextable(doc, ft_results)
  doc <- add_body(doc,
    paste0(
      "Note: 'All Population' columns show every person in the group regardless of deprivation. ",
      "'Deprived & Exposed' shows only those in cells with GRDI \u2265 ", dep_threshold,
      " who are also in flood-prone areas; '% Deprived & Exposed' uses total group ",
      "population as denominator."
    )
  )
  doc <- add_blank(doc)

  # 4b — Scenario comparison chart
  doc <- add_h2(doc, "4.2 Scenario Comparison: How Exposure Changes with Deprivation Filter")
  doc <- add_body(doc,
    paste0(
      "The chart below compares the percentage of each group exposed to flooding across ",
      "three scenarios: (1) all population exposed; (2) deprived population as a share ",
      "of deprived population; and (3) deprived population as a share of the total group. ",
      "Comparing scenarios (1) and (3) shows the additional burden of combined deprivation ",
      "and flood risk. Comparing (2) and (3) reveals whether the deprived sub-group is ",
      "disproportionately concentrated in flood zones."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_scenario_chart, width = 6.0, height = 4.0,
                 caption = paste0("% population exposed by demographic group and analysis scenario \u2013 ",
                                  ctry_name))
  doc <- add_blank(doc)

  # 4c — Choropleth
  doc <- add_h2(doc, "4.3 Geographic Distribution of Exposure")
  doc <- add_body(doc,
    paste0(
      "The maps below show the percentage of each district's population that is exposed ",
      "to flooding, for each demographic group. Darker orange indicates a higher proportion ",
      "exposed. This allows programme planners to identify which districts require priority ",
      "attention for each specific population group."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_choropleth, width = 6.0, height = 5.5,
                 caption = paste0("Percentage of population exposed by district and group \u2013 ",
                                  ctry_name))
  doc <- add_blank(doc)

  # 4d — Top-5 heatmap
  doc <- add_h2(doc, "4.4 Top 5 Most-Exposed Districts Across All Groups")
  doc <- add_body(doc,
    paste0(
      "The heatmap below highlights the five districts with the highest mean flood exposure ",
      "across all demographic groups. Each cell shows the percentage of that group's population ",
      "in the district that is exposed to flooding. Districts in the top rows are those facing ",
      "the greatest overall flood burden, while columns reveal which groups are ",
      "disproportionately at risk within each district."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_top5_heatmap, width = 6.0, height = 3.5,
                 caption = paste0("Top 5 most-exposed districts by demographic group \u2013 ",
                                  ctry_name, " (raw exposure scenario)"))
  doc <- add_blank(doc)

  # 4f — Bar chart
  doc <- add_h2(doc, "4.6 Absolute Numbers of People Exposed")
  doc <- add_body(doc,
    "The chart below compares the total number of people exposed to flooding across all demographic groups (raw exposure: all group members, no deprivation filter). This identifies which groups have the largest absolute numbers at flood risk."
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_bars, width = 5.5, height = 3.5,
                 caption = paste0("People exposed to flooding by demographic group \u2013 ", ctry_name))
  doc <- add_blank(doc)

  # 4g — Dotplot
  doc <- add_h2(doc, "4.7 Relative Exposure Rate by Group")
  doc <- add_body(doc,
    "The chart below ranks groups by the percentage of their population that is exposed to flooding (raw exposure scenario). This reveals which groups are disproportionately concentrated in flood-prone areas relative to their total size."
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_dotplot, width = 5.5, height = 3.5,
                 caption = paste0("Percentage of population exposed by group (ranked, raw exposure) \u2013 ", ctry_name))
  doc <- add_blank(doc)

  # 4h — Bubble chart (optional)
  if (!is.null(img_bubble) && file.exists(img_bubble)) {
    doc <- add_h2(doc, "4.8 Group Size vs. Exposure Rate")
    doc <- add_body(doc,
      "The bubble chart below positions each population group by its vulnerable population size (x-axis) and exposure rate (y-axis). Bubble size represents the absolute number exposed. Groups in the upper-right area are both large in deprived population and highly exposed to flooding."
    )
    doc <- add_blank(doc)
    doc <- add_img(doc, img_bubble, width = 5.5, height = 4.0,
                   caption = paste0("Population size vs. exposure rate by group \u2013 ", ctry_name))
    doc <- add_blank(doc)
  }

  # ------------------------------------------------------------------
  # SECTION 5 — WHAT THIS MEANS FOR UNFPA
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "5. What This Means for UNFPA")
  doc <- add_body(doc,
    paste0(
      "These findings have direct implications for UNFPA's work in ", ctry_name, ":"
    )
  )
  doc <- add_blank(doc)

  implications <- c(
    paste0(
      "Reproductive health services: Districts with high concentrations of women aged 15\u201349 ",
      "in flood-prone areas should be prioritised for pre-positioned supplies, mobile clinics, ",
      "and emergency obstetric care plans."
    ),
    paste0(
      "Child protection and nutrition: Areas with large numbers of children under 5 exposed ",
      "to flooding face increased risk of malnutrition, disease outbreak, and displacement. ",
      "These districts warrant targeted early-warning and response planning."
    ),
    paste0(
      "Youth and adolescents: Flood exposure among youth (15\u201324) and adolescents (10\u201319) ",
      "can disrupt education, increase gender-based violence risk, and accelerate early marriage. ",
      "UNFPA's youth-focused programmes should map their service points against these exposure zones."
    ),
    paste0(
      "Older populations: People aged 65 and over face higher mortality and mobility challenges ",
      "during floods. Districts with elevated exposure among this group need tailored evacuation ",
      "and social protection responses."
    ),
    paste0(
      "Data for advocacy: These numbers can be used in donor proposals, situation reports, and ",
      "government engagement to make the case for climate-resilient SRHR services and adequate ",
      "emergency preparedness funding."
    )
  )

  for (impl in implications) {
    doc <- officer::body_add_par(doc, paste0("\u2022  ", impl), style = "Normal")
    doc <- add_blank(doc)
  }

  # ------------------------------------------------------------------
  # SECTION 6 — HOW TO REPLICATE THIS ANALYSIS
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "6. How to Replicate or Extend This Analysis")
  doc <- add_body(doc,
    paste0(
      "This analysis was conducted entirely in the open-source R programming environment ",
      "and can be replicated or adapted for any country. The code, input data, and outputs ",
      "are organised in a structured project folder. To run the analysis for a different ",
      "country or a different hazard scenario, only a few settings need to change at the ",
      "top of the main script."
    )
  )
  doc <- add_blank(doc)

  steps_df <- data.frame(
    Step = c("1", "2", "3", "4", "5"),
    Action = c(
      "Obtain flood hazard tiles",
      "Download population data",
      "Run the main analysis script",
      "Review outputs",
      "Extend to new hazards"
    ),
    Detail = c(
      paste0(
        "Download GloFAS flood depth rasters for the relevant return period from the ",
        "Copernicus Emergency Management Service. Place tiles in the 'input/flood_layers_RP",
        return_period, "/' folder."
      ),
      paste0(
        "Population rasters are downloaded automatically from WorldPop when running ",
        "the script for the first time. Processed group rasters are saved to 'input/pop/'."
      ),
      paste0(
        "Open the project in R, set 'ctry_code' to the ISO3 country code, and run ",
        "'source(\"main.R\")'. All outputs are saved to the 'output/' folder."
      ),
      paste0(
        "The Excel file in 'output/zonal_stats/' contains the full indicator table. ",
        "Maps and charts are in 'output/flood_maps/'. This Word report is in 'output/reports/'."
      ),
      paste0(
        "Future hazards (coastal flood, heat, aridity) follow the same pipeline. ",
        "Set 'hazard_name' and 'hazard_code' in the main script and supply the relevant ",
        "hazard raster."
      )
    ),
    stringsAsFactors = FALSE
  )

  ft_steps <- flextable::flextable(steps_df)
  ft_steps <- flextable::bold(ft_steps, part = "header")
  ft_steps <- flextable::width(ft_steps, j = 1, width = 0.4)
  ft_steps <- flextable::width(ft_steps, j = 2, width = 1.5)
  ft_steps <- flextable::width(ft_steps, j = 3, width = 4.3)
  ft_steps <- flextable::theme_box(ft_steps)
  ft_steps <- flextable::fontsize(ft_steps, size = 9, part = "all")

  doc <- flextable::body_add_flextable(doc, ft_steps)
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # FOOTER — limitations note
  # ------------------------------------------------------------------
  doc <- add_h1(doc, "7. Important Notes")
  doc <- add_body(doc,
    paste0(
      "This analysis represents a modelled estimate of flood exposure, not actual flood ",
      "observations. The flood hazard model simulates a statistical return-period event; ",
      "actual flood extents will vary based on local conditions, land use change, and ",
      "climate variability. Population figures are based on WorldPop modelled estimates ",
      "for ", year, " and may differ from census data. Results should be interpreted as ",
      "indicative guidance for programme planning, not precise counts."
    )
  )
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # ANNEX — Technical note on flood fraction methodology
  # ------------------------------------------------------------------
  doc <- officer::body_add_break(doc)   # page break before annex
  doc <- add_h1(doc, "Annex: Technical Note \u2013 Why Flood Fractions Are Used")

  doc <- add_h2(doc, "The Resolution Mismatch Problem")
  doc <- add_body(doc,
    paste0(
      "The flood hazard raster from GloFAS is produced at approximately 90-metre resolution, ",
      "while the WorldPop population raster used in this analysis is at 1-kilometre resolution. ",
      "A single population cell (1 km\u00b2) therefore contains roughly 11 \u00d7 11 = 121 flood ",
      "sub-cells. A population cell almost never falls entirely inside or entirely outside a ",
      "flood zone \u2014 in most cases it straddles the flood boundary."
    )
  )
  doc <- add_blank(doc)
  doc <- add_body(doc,
    paste0(
      "If the fine binary flood raster (0 = not flooded, 1 = flooded) were simply resampled ",
      "to 1 km using a nearest-neighbour rule, each population cell would be classified as ",
      "either fully flooded or completely dry. This discards the partial-overlap information ",
      "and systematically over- or under-counts exposed people along every flood boundary."
    )
  )
  doc <- add_blank(doc)

  doc <- add_h2(doc, "What a Flood Fraction Represents")
  doc <- add_body(doc,
    paste0(
      "To preserve sub-grid information, the binary flood raster is first aggregated to 1 km ",
      "by computing the mean of all sub-cells within each population cell. The result is a ",
      "'flood fraction' \u2014 a number between 0 and 1 that represents the proportion of that ",
      "1 km\u00b2 area classified as flooded. For example, a flood fraction of 0.30 means that ",
      "30% of the cell's area lies within the flood zone."
    )
  )
  doc <- add_blank(doc)

  doc <- add_h2(doc, "How Exposed Population Is Calculated")
  doc <- add_body(doc,
    "The exposed population for each cell is then estimated as:"
  )
  doc <- add_blank(doc)
  doc <- officer::body_add_par(
    doc,
    "    Exposed population = Total population in cell \u00d7 Flood fraction",
    style = "Normal"
  )
  doc <- add_blank(doc)
  doc <- add_body(doc,
    paste0(
      "So if a 1 km cell holds 1,000 people and its flood fraction is 0.30, the analysis ",
      "counts 300 people as exposed \u2014 rather than either 0 or 1,000. This proportional ",
      "approach is far more accurate for cells that partially overlap the flood boundary, ",
      "which is the case for the majority of cells along any flood edge."
    )
  )
  doc <- add_blank(doc)

  doc <- add_h2(doc, "Summary")
  doc <- add_body(doc,
    paste0(
      "Flood fractions act as a bridge between the fine-resolution hazard model and the ",
      "coarser population grid. They preserve spatial detail that would otherwise be lost, ",
      "producing more realistic estimates of how many people \u2014 and from which demographic ",
      "groups \u2014 are genuinely at risk from flooding."
    )
  )
  doc <- add_blank(doc)

  # ------------------------------------------------------------------
  # SAVE
  # ------------------------------------------------------------------
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = out_path)
  message("Word report saved to: ", out_path)
  invisible(doc)
}
