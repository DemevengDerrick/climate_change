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
                               ctry_indicators_vuln  = NULL,
                               img_hazard            = NULL,
                               img_population        = NULL,
                               img_overlay           = NULL,
                               img_deprivation       = NULL,
                               img_workflow          = NULL,
                               img_choropleth_groups = NULL,
                               img_scenario_chart    = NULL,
                               img_top5_heatmap      = NULL,
                               img_degurba_l1        = NULL,
                               img_degurba_l2        = NULL,
                               img_bars              = NULL,
                               img_dotplot           = NULL,
                               img_bubble            = NULL,
                               dep_threshold         = 50,
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
    paste0("Poverty and Flood Exposure Analysis"),
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
      "This report presents a poverty and flood exposure analysis for ",
      ctry_name, ". It goes beyond simply mapping who lives near floods: it first identifies ",
      "communities living in poverty using an internationally recognised poverty index, ",
      "and then assesses how many people within those poor areas would be affected by a ",
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
      "Poverty Index (GRDI)"
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
        "0 (lowest poverty) to 100 (highest poverty), combining sub-national indicators of ",
        "poverty, child mortality, sanitation access, and economic conditions. Cells ",
        "scoring \u2265 ", dep_threshold, " are classified as 'in poverty' for this analysis."
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
                 caption = "Methodology workflow: from data inputs to poverty and flood exposure indicators")
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
  doc <- add_h2(doc, "Step 3: Identifying Communities Living in Poverty")
  doc <- add_body(doc,
    paste0(
      "Not all people living near floods are equally at risk. Communities living in poverty ",
      "\u2014 with limited access to healthcare, sanitation, and economic ",
      "resources \u2014 are far less able to prepare for, respond to, or recover from a ",
      "flood. To capture this dimension, the analysis uses the Global Relative Deprivation ",
      "Index (GRDI), a composite score produced by CIESIN/SEDAC at Columbia University. ",
      "Each 1 km\u00b2 cell receives a score from 0 (lowest poverty) to 100 (highest poverty). ",
      "Cells scoring \u2265 ", dep_threshold, " \u2014 the upper half of the global poverty ",
      "distribution \u2014 are classified as 'in poverty'. Only the population living in these ",
      "cells is carried forward into the flood exposure step."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_deprivation, width = 5.5, height = 8.0,
                 caption = paste0("Top: GRDI poverty scores | Bottom: poverty mask ",
                                  "(GRDI \u2265 ", dep_threshold, ") \u2014 ", ctry_name))
  doc <- add_blank(doc)

  # Step 4 — Overlay
  doc <- add_h2(doc, "Step 4: Combining Flood Hazard with Population in Poverty")
  doc <- add_body(doc,
    paste0(
      "The final step overlays the flood hazard map with the population in poverty ",
      "rasters. Where a flood zone overlaps with a poor, populated area, those people ",
      "are counted as 'exposed poor population'. The map below shows this overlap: ",
      "dark areas are where flooding, dense population, and high poverty coincide \u2014 ",
      "representing the highest concentrations of people exposed to poverty and floods."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_overlay, width = 5.5, height = 4.5,
                 caption = paste0("Population in poverty in flood-prone areas \u2013 overlay map, ",
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
      "(1) all group members regardless of poverty status; and (2) only those living in ",
      "poor areas (GRDI score \u2265 ", dep_threshold, "). For the poverty scenario, ",
      "the percentage is expressed as a share of the total group population, showing what ",
      "fraction of each group faces the combined burden of poverty and flood risk."
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
    vuln_exposed       = "In Poverty &\nExposed",
    vuln_pct           = "% In Poverty\n& Exposed"
  )
  ft_results <- flextable::add_header_row(
    ft_results,
    values = c("", "All Population", "Population in Poverty\n(GRDI \u2265 50)"),
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
      "Note: 'All Population' columns show every person in the group regardless of poverty status. ",
      "'In Poverty & Exposed' shows only those in cells with GRDI \u2265 ", dep_threshold,
      " who are also in flood-prone areas; '% In Poverty & Exposed' uses total group ",
      "population as denominator."
    )
  )
  doc <- add_blank(doc)

  # 4b — Scenario comparison chart
  doc <- add_h2(doc, "4.2 Scenario Comparison: How Exposure Changes with Poverty Filter")
  doc <- add_body(doc,
    paste0(
      "The chart below compares the percentage of each group exposed to flooding across ",
      "three scenarios: (1) all population exposed; (2) poor population as a share ",
      "of poor population; and (3) poor population as a share of the total group. ",
      "Comparing scenarios (1) and (3) shows the additional burden of combined poverty ",
      "and flood risk. Comparing (2) and (3) reveals whether the poor sub-group is ",
      "disproportionately concentrated in flood zones."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_scenario_chart, width = 6.0, height = 4.0,
                 caption = paste0("% population exposed by demographic group and analysis scenario \u2013 ",
                                  ctry_name))
  doc <- add_blank(doc)

  # 4c — Per-group choropleths
  doc <- add_h2(doc, "4.3 Geographic Distribution of Exposure by Demographic Group")
  doc <- add_body(doc,
    paste0(
      "The following maps show district-level flood exposure for each demographic group. ",
      "Each map contains two panels: the left panel (yellow\u2013red scale) shows the ",
      "percentage of all group members exposed to flooding; the right panel (white\u2013purple ",
      "scale) shows the percentage of the poor sub-group (GRDI \u2265 ", dep_threshold,
      ") exposed. Independent colour scales are used so each panel is fully informative ",
      "at its own range. Darker colours indicate higher exposure."
    )
  )
  doc <- add_blank(doc)

  if (!is.null(img_choropleth_groups) && length(img_choropleth_groups) > 0) {
    for (i in seq_along(img_choropleth_groups)) {
      grp_label <- names(img_choropleth_groups)[i]
      grp_path  <- img_choropleth_groups[[i]]
      if (!is.null(grp_label) && nchar(grp_label) > 0) {
        doc <- officer::body_add_par(doc, grp_label, style = "heading 3")
      }
      doc <- add_img(doc, grp_path, width = 6.5, height = 3.8,
                     caption = paste0(grp_label,
                                      ": flood exposure (left) and poverty exposure ",
                                      "(right) by district \u2013 ", ctry_name))
      doc <- add_blank(doc)
    }
  }

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

  # 4e — DEGURBA urban-rural breakdown
  doc <- add_h2(doc, "4.5 Exposure by Degree of Urbanisation")
  doc <- add_body(doc,
    paste0(
      "The following charts break down flood exposure by the Degree of Urbanisation ",
      "(DEGURBA), a global classification of 1 km\u00b2 grid cells into urbanisation ",
      "categories produced by the Global Human Settlement Layer (GHS-DUG R2023A). ",
      "This analysis reveals whether flood risk is concentrated in rural or urban ",
      "settings, and how poverty compounds that risk within each urbanisation context."
    )
  )
  doc <- add_blank(doc)

  doc <- add_h2(doc, "4.5.1 Level 1: Rural, Urban Cluster and Urban Centre")
  doc <- add_body(doc,
    paste0(
      "DEGURBA Level 1 groups cells into three broad categories: Rural areas ",
      "(low-density, dispersed settlement), Urban Clusters (small and medium cities ",
      "with \u226550,000 inhabitants and density \u2265300 inhabitants/km\u00b2), and ",
      "Urban Centres (large cities with \u2265100,000 inhabitants and density ",
      "\u22651,500 inhabitants/km\u00b2). ",
      "The chart below shows the percentage of each demographic group exposed to ",
      "flooding within each of these three classes."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_degurba_l1, width = 6.5, height = 4.0,
                 caption = paste0(
                   "Flood exposure by DEGURBA Level 1 class (L) raw exposure, ",
                   "(R) poor population \u2013 ", ctry_name))
  doc <- add_blank(doc)

  doc <- add_h2(doc, "4.5.2 Level 2: Fine-grained Urbanisation Classes")
  doc <- add_body(doc,
    paste0(
      "DEGURBA Level 2 disaggregates the three L1 classes into seven meaningful ",
      "categories (excluding Water), from Very Low Density Rural through to Urban ",
      "Centre. The heatmap below shows exposure rates across all groups and all ",
      "seven classes simultaneously, making it easy to identify which combinations ",
      "of urbanisation context and demographic group face the highest flood burden."
    )
  )
  doc <- add_blank(doc)
  doc <- add_img(doc, img_degurba_l2, width = 6.5, height = 4.5,
                 caption = paste0(
                   "Flood exposure by DEGURBA Level 2 class (L) raw exposure, ",
                   "(R) poor population \u2013 ", ctry_name))
  doc <- add_blank(doc)

  # 4f — Bar chart
  doc <- add_h2(doc, "4.6 Absolute Numbers of People Exposed")
  doc <- add_body(doc,
    "The chart below compares the total number of people exposed to flooding across all demographic groups (raw exposure: all group members, no poverty filter). This identifies which groups have the largest absolute numbers at flood risk."
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
      "The bubble chart below positions each population group by its poor population size (x-axis) and exposure rate (y-axis). Bubble size represents the absolute number exposed. Groups in the upper-right area are both large in poor population and highly exposed to flooding."
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


# =============================================================================
#  POWERPOINT PRESENTATION — UNFPA management buy-in deck
# =============================================================================

#' Generate a high-impact UNFPA-styled PowerPoint presentation
#'
#' Produces a 10-slide management deck covering the full climate vulnerability
#' and flood exposure analysis. Follows UNFPA brand guidelines: navy, sky-blue,
#' and orange accent palette with Calibri typeface.
#'
#' @param ctry_code           Character. ISO3 code.
#' @param ctry_name           Character. Full country name.
#' @param hazard_name         Character. E.g. `"River Flood"`.
#' @param return_period       Integer. Return period in years.
#' @param year                Integer. Population year.
#' @param ctry_indicators     Tibble. Raw-scenario country summary
#'   (output of [exp_summarise_by_country()] filtered to `raw_exposure`).
#' @param ctry_indicators_vuln Tibble or NULL. Vuln-vs-total country summary.
#' @param img_hazard          Character or NULL. Path to hazard map.
#' @param img_population      Character or NULL. Path to population map.
#' @param img_overlay         Character or NULL. Path to overlay map.
#' @param img_deprivation     Character or NULL. Path to deprivation map.
#' @param img_workflow        Character or NULL. Path to workflow diagram.
#' @param img_scenario_chart  Character or NULL. Path to scenario comparison chart.
#' @param img_top5_heatmap    Character or NULL. Path to top-5 district heatmap.
#' @param choropleth_grp_paths Named list. Per-group choropleth paths.
#' @param dep_threshold       Numeric. GRDI threshold (default 50).
#' @param out_path            Character. Output `.pptx` path.
#' @param author              Character. Author name (default `"UNFPA"`).
#'
#' @return Invisibly returns the `officer` presentation object.
#' @export
rpt_generate_ppt <- function(ctry_code, ctry_name, hazard_name, return_period,
                              year, ctry_indicators,
                              ctry_indicators_vuln  = NULL,
                              img_hazard            = NULL,
                              img_population        = NULL,
                              img_overlay           = NULL,
                              img_deprivation       = NULL,
                              img_workflow          = NULL,
                              img_scenario_chart    = NULL,
                              img_top5_heatmap      = NULL,
                              choropleth_grp_paths  = NULL,
                              dep_threshold         = 50,
                              out_path,
                              author = "UNFPA") {

  if (!requireNamespace("officer", quietly = TRUE))
    stop("Package 'officer' is required.")

  # Null-coalescing operator (base R >= 4.4; provide fallback for older versions)
  `%||%` <- function(a, b) if (!is.null(a) && file.exists(a)) a else b

  # ── UNFPA Brand System ─────────────────────────────────────────────────────
  NAVY  <- "#1C3F5F"   # headers, backgrounds
  BLUE  <- "#009FDA"   # accent, sub-titles
  ORNGE <- "#F47920"   # statistics callouts
  WHITE <- "#FFFFFF"
  LGREY <- "#F4F6F8"   # slide backgrounds
  DGREY <- "#3D3D3D"   # body text
  MGREY <- "#8A9BB0"   # secondary text
  FONT  <- "Calibri"

  # ── Style helpers ──────────────────────────────────────────────────────────
  # fp_text shortcut
  ts <- function(sz, col = DGREY, bold = FALSE, italic = FALSE)
    officer::fp_text(color = col, font.size = sz, bold = bold,
                     italic = italic, font.family = FONT)

  # fp_par shortcut — compatible with officer versions that lack space_before/after
  ps <- function(align = "left", bg = NULL) {
    args <- list(text.align = align)
    if (!is.null(bg)) args$shading.color <- bg
    do.call(officer::fp_par, args)
  }

  # Produce a list of N identical colored filler paragraphs to fill a box.
  # Each para height ≈ font_pt / 72 inches (single-spaced).
  fill_paras <- function(color, n, font_pt = 18) {
    lapply(seq_len(n), function(i)
      officer::fpar(
        officer::ftext(" ", ts(font_pt, color)),
        fp_p = ps("left", bg = color)
      ))
  }

  # Build block_list, accepting a plain list of fpars (do.call wrapper)
  bl <- function(...) officer::block_list(...)
  bll <- function(para_list) do.call(officer::block_list, para_list)

  # Place a block_list in a precisely positioned text box
  tb <- function(pptx, content, left, top, w, h)
    officer::ph_with(pptx, value = content,
                     location = officer::ph_location(left = left, top = top,
                                                      width = w, height = h))

  # Place an external image (no-op if path is NULL or missing)
  ig <- function(pptx, path, left, top, w, h) {
    if (!is.null(path) && file.exists(path))
      officer::ph_with(pptx,
                       value = officer::external_img(path, width = w, height = h),
                       location = officer::ph_location(left = left, top = top,
                                                        width = w, height = h))
    else pptx
  }

  # Standard navy header bar (0.72 inch, full width) + UNFPA footer line
  # Returns modified pptx.
  add_header <- function(pptx, title_text) {
    pptx <- tb(pptx,
               bl(officer::fpar(
                 officer::ftext(paste0("  ", title_text),
                                ts(19, WHITE, bold = TRUE)),
                 fp_p = ps("left", bg = NAVY))),
               0, 0, 10, 0.72)
    pptx <- tb(pptx,
               bl(officer::fpar(
                 officer::ftext(
                   paste0(author, "  \u2022  ", ctry_name,
                          "  \u2022  CONFIDENTIAL"),
                   ts(7.5, MGREY)),
                 fp_p = ps("center"))),
               0, 7.2, 10, 0.3)
    pptx
  }

  # ── Extract key statistics ─────────────────────────────────────────────────
  tot_row <- ctry_indicators[ctry_indicators$group.label == "Total Population", ]
  n_exp   <- if (nrow(tot_row) > 0) format(round(tot_row$pop.exposed[1]),
                                            big.mark = ",") else "N/A"
  n_tot   <- if (nrow(tot_row) > 0) format(round(tot_row$total.pop[1]),
                                            big.mark = ",") else "N/A"
  pct_exp <- if (nrow(tot_row) > 0)
    paste0(round(tot_row$perc.pop.exposed[1], 1), "%") else "N/A"

  sub_ind  <- ctry_indicators[ctry_indicators$group.label != "Total Population", ]
  top_row  <- if (nrow(sub_ind) > 0) sub_ind[which.max(sub_ind$perc.pop.exposed), ]
              else NULL
  top_lbl  <- if (!is.null(top_row)) top_row$group.label[1]          else "N/A"
  top_pct  <- if (!is.null(top_row))
    paste0(round(top_row$perc.pop.exposed[1], 1), "%") else "N/A"

  vuln_lbl <- "N/A"; vuln_pct <- "N/A"
  if (!is.null(ctry_indicators_vuln) && nrow(ctry_indicators_vuln) > 0) {
    sv <- ctry_indicators_vuln[
      ctry_indicators_vuln$group.label != "Total Population", ]
    tv <- if (nrow(sv) > 0) sv[which.max(sv$perc.pop.exposed), ] else NULL
    if (!is.null(tv)) {
      vuln_lbl <- tv$group.label[1]
      vuln_pct <- paste0(round(tv$perc.pop.exposed[1], 1), "%")
    }
  }

  n_groups <- nrow(ctry_indicators[ctry_indicators$group.label !=
                                     "Total Population", ])

  # ── BUILD PRESENTATION ─────────────────────────────────────────────────────
  pptx <- officer::read_pptx()

  # ============================================================
  # SLIDE 1 — COVER
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")

  # Left panel: navy filled column (5.4 wide × 7.5 tall)
  # Fill with stacked colored paragraphs (each ~0.46 inch tall at 18pt + 8+8 pt spacing)
  n_fillers <- 14
  left_content <- c(
    # Spacer rows at top
    fill_paras(NAVY, 3, font_pt = 18),
    # UNFPA label
    list(officer::fpar(
      officer::ftext("UNITED NATIONS POPULATION FUND",
                     ts(8.5, BLUE, bold = TRUE)),
      fp_p = ps("left", bg = NAVY))),
    # Spacer
    fill_paras(NAVY, 1, font_pt = 10),
    # Main title
    list(
      officer::fpar(officer::ftext("POVERTY &",
                                   ts(26, WHITE, bold = TRUE)),
                    fp_p = ps("left", bg = NAVY)),
      officer::fpar(officer::ftext("FLOOD EXPOSURE",
                                   ts(26, WHITE, bold = TRUE)),
                    fp_p = ps("left", bg = NAVY)),
      officer::fpar(officer::ftext("ANALYSIS",
                                   ts(26, WHITE, bold = TRUE)),
                    fp_p = ps("left", bg = NAVY)),
      # Blue divider text
      officer::fpar(officer::ftext(
        paste0(ctry_name, "  \u2014  ", hazard_name,
               " (1-in-", return_period, "-year)  \u2014  ", year),
        ts(12, BLUE, bold = TRUE)),
        fp_p = ps("left", bg = NAVY)),
      # Author / date
      officer::fpar(officer::ftext(
        paste0("Prepared by: ", author, "  \u2022  ",
               format(Sys.Date(), "%B %Y")),
        ts(10, MGREY)),
        fp_p = ps("left", bg = NAVY))
    ),
    # Remaining fillers
    fill_paras(NAVY, 4, font_pt = 18)
  )
  pptx <- tb(pptx, bll(left_content), 0, 0, 5.3, 7.5)

  # Right panel: image
  pptx <- ig(pptx, img_overlay %||% img_population,
             5.4, 0.15, 4.5, 6.6)

  # Bottom UNFPA-blue accent strip across full width
  pptx <- tb(pptx,
             bl(officer::fpar(officer::ftext(" ", ts(10, BLUE)),
                              fp_p = ps("left", bg = BLUE))),
             0, 7.17, 10, 0.33)

  # ============================================================
  # SLIDE 2 — THE CHALLENGE
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx, "The Challenge: Climate Risk is a Development Crisis")

  pptx <- tb(pptx, bl(
    officer::fpar(officer::ftext(
      paste0("In ", ctry_name, ", extreme weather events are becoming more frequent ",
             "and more severe. Floods threaten lives, displace families, and ",
             "destroy the health infrastructure that UNFPA depends on to deliver ",
             "life-saving services."),
      ts(13, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "Communities living in poverty \u2014 already lacking access to healthcare, clean water, ",
      ts(13, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "and economic security \u2014 face the greatest risk and the least capacity to recover.",
      ts(13, DGREY, bold = TRUE)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "Until now, UNFPA's response planning has lacked the spatial precision to answer:",
      ts(13, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "\u2022  Which districts face the highest flood risk?",
      ts(13, NAVY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "\u2022  Which demographic groups are most exposed?",
      ts(13, NAVY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "\u2022  Where do flood risk and poverty overlap?",
      ts(13, NAVY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "\u2022  How many poor people face combined flood and poverty risk?",
      ts(13, NAVY)), fp_p = ps("left")),
    officer::fpar(officer::ftext(
      "This analysis answers all four questions for the first time.",
      ts(14, ORNGE, bold = TRUE)), fp_p = ps("left"))
  ), 0.5, 0.9, 9.0, 6.1)

  # ============================================================
  # SLIDE 3 — METHODOLOGY
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx, "Our Approach: Three Layers, One Answer")

  # Three step boxes
  steps <- list(
    list("1", "Map the Flood",
         paste0("GloFAS flood hazard model identifies areas inundated in a ",
                "1-in-", return_period, "-year event. Flood depth rasters ",
                "processed to 1 km\u00b2 flood-fraction grids.")),
    list("2", "Map the People",
         paste0("WorldPop ", year, " constrained UN-adjusted 1 km grids ",
                "disaggregated by 9 demographic groups: women 15\u201349, ",
                "adolescents, children <5, elderly, and more.")),
    list("3", "Identify the Poor",
         paste0("CIESIN/SEDAC Global Relative Deprivation Index (GRDI) applied to ",
                "isolate communities scoring \u2265 ", dep_threshold,
                "/100 \u2014 the poorest half of the global distribution."))
  )
  step_cols <- c(0.35, 3.55, 6.75)
  for (k in seq_along(steps)) {
    s <- steps[[k]]
    pptx <- tb(pptx,
               bl(officer::fpar(officer::ftext(s[[1]], ts(28, WHITE, bold = TRUE)),
                                fp_p = ps("center", bg = BLUE))),
               step_cols[k], 0.9, 2.9, 0.62)
    pptx <- tb(pptx,
               bl(officer::fpar(officer::ftext(s[[2]], ts(13, NAVY, bold = TRUE)),
                                fp_p = ps("left")),
                  officer::fpar(officer::ftext(s[[3]], ts(11, DGREY)),
                                fp_p = ps("left"))),
               step_cols[k], 1.58, 2.9, 1.6)
  }

  # Workflow image
  pptx <- ig(pptx, img_workflow, 0.5, 3.3, 9.0, 3.65)

  # ============================================================
  # SLIDE 4 — FLOOD HAZARD
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(
    pptx, paste0("The Threat: ", hazard_name,
                 " (1-in-", return_period, "-Year Event) \u2014 ", ctry_name))

  pptx <- ig(pptx, img_hazard, 0.3, 0.85, 6.1, 6.1)

  pptx <- tb(pptx, bl(
    officer::fpar(officer::ftext("What this shows",
                                 ts(12, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext(
      paste0("Orange cells indicate the fraction of each 1 km\u00b2 area ",
             "within the flood zone. Brighter = more of the cell is flooded."),
      ts(11, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext("Why it matters",
                                 ts(12, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext(
      paste0("Using flood fractions \u2014 rather than binary flooded/not-flooded \u2014 ",
             "captures partial exposure along flood boundaries, producing more ",
             "accurate population counts."),
      ts(11, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext("Return period",
                                 ts(12, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext(
      paste0("A 1-in-", return_period,
             "-year event has a ", round(100 / return_period, 1),
             "% probability of occurring in any given year. ",
             "Under climate change, such events are projected to become ",
             "more frequent."),
      ts(11, DGREY)), fp_p = ps("left"))
  ), 6.55, 0.9, 3.2, 6.0)

  # ============================================================
  # SLIDE 5 — DEPRIVATION & VULNERABILITY
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx,
                     paste0("Who is Most At Risk: Poverty Map \u2014 ",
                            ctry_name))

  pptx <- ig(pptx, img_deprivation, 0.3, 0.85, 5.5, 5.0)

  pptx <- tb(pptx, bl(
    officer::fpar(officer::ftext("The GRDI Index",
                                 ts(12, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext(
      paste0("The Global Relative Deprivation Index scores every 1 km\u00b2 ",
             "cell from 0 (lowest poverty) to 100 (highest poverty), ",
             "combining poverty, child mortality, sanitation, and economic ",
             "indicators."),
      ts(11, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext("Poverty threshold",
                                 ts(12, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext(
      paste0("Cells scoring \u2265 ", dep_threshold,
             " are classified as 'in poverty' \u2014 above-median ",
             "poverty globally. Only the population in these cells ",
             "is carried into the poverty and flood exposure analysis."),
      ts(11, DGREY)), fp_p = ps("left")),
    officer::fpar(officer::ftext("Top panel (map): ",
                                 ts(11, NAVY, bold = TRUE)),
                  officer::ftext("GRDI score gradient (green = low, red = high poverty).",
                                 ts(11, DGREY)),
                  fp_p = ps("left")),
    officer::fpar(officer::ftext("Bottom panel (map): ",
                                 ts(11, NAVY, bold = TRUE)),
                  officer::ftext(paste0("Binary poverty mask \u2014 red cells = in poverty ",
                                        "(GRDI \u2265 ", dep_threshold, ")."),
                                 ts(11, DGREY)),
                  fp_p = ps("left"))
  ), 5.95, 0.9, 3.8, 6.0)

  # ============================================================
  # SLIDE 6 — EXPOSURE OVERVIEW (Key Numbers)
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx,
                     paste0("Headline Findings: Flood Exposure in ", ctry_name))

  # Three stat callout boxes
  stats_data <- list(
    list(n_exp,   "people exposed to flooding",
         paste0("Total population in the ", hazard_name, " zone")),
    list(pct_exp, "of total population exposed",
         "Share of the country's population at flood risk"),
    list(top_pct, paste0("of ", top_lbl, " exposed"),
         "The highest-exposed demographic group")
  )
  stat_lefts <- c(0.4, 3.6, 6.8)
  for (k in seq_along(stats_data)) {
    sd_k <- stats_data[[k]]
    pptx <- tb(pptx,
               bl(
                 officer::fpar(officer::ftext(sd_k[[1]], ts(34, ORNGE, bold = TRUE)),
                               fp_p = ps("center")),
                 officer::fpar(officer::ftext(sd_k[[2]], ts(11, NAVY, bold = TRUE)),
                               fp_p = ps("center")),
                 officer::fpar(officer::ftext(sd_k[[3]], ts(9, MGREY)),
                               fp_p = ps("center"))
               ),
               stat_lefts[k], 0.9, 2.9, 1.5)

    # Blue underline bar
    pptx <- tb(pptx,
               bl(officer::fpar(officer::ftext(" ", ts(8, BLUE)),
                                fp_p = ps("center", bg = BLUE))),
               stat_lefts[k], 2.4, 2.9, 0.18)
  }

  # Overlay map below the stats
  pptx <- ig(pptx, img_overlay, 0.4, 2.75, 9.2, 4.2)

  # ============================================================
  # SLIDE 7 — EXPOSURE BY GROUP
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx,
                     paste0("Exposure Across Demographic Groups \u2014 ",
                            n_groups, " Groups Analysed"))

  pptx <- ig(pptx, img_scenario_chart, 0.3, 0.85, 9.4, 5.7)

  pptx <- tb(pptx, bl(
    officer::fpar(
      officer::ftext("Bar 1 (dark): ", ts(9, NAVY, bold = TRUE)),
      officer::ftext("% of all group members in flood zone.  ",
                     ts(9, DGREY)),
      officer::ftext("Bar 2 (medium): ", ts(9, NAVY, bold = TRUE)),
      officer::ftext("% of poor sub-group exposed (vs poor total).  ",
                     ts(9, DGREY)),
      officer::ftext("Bar 3 (light): ", ts(9, NAVY, bold = TRUE)),
      officer::ftext("% of poor sub-group exposed (vs total group).",
                     ts(9, DGREY)),
      fp_p = ps("left"))
  ), 0.3, 6.7, 9.4, 0.42)

  # ============================================================
  # SLIDE 8 — PRIORITY DISTRICTS
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx,
                     paste0("Priority Districts: Top 5 Most-Exposed Areas \u2014 ",
                            ctry_name))

  pptx <- ig(pptx, img_top5_heatmap, 0.3, 0.85, 9.4, 5.0)

  pptx <- tb(pptx, bl(
    officer::fpar(
      officer::ftext(
        paste0("Heatmap shows % of each group's population exposed to the ",
               hazard_name, " (RP", return_period, ") in the 5 most-exposed ",
               "districts. Darker red = higher exposure. Use this to identify ",
               "which districts AND groups should be prioritised."),
        ts(10, DGREY)),
      fp_p = ps("left"))
  ), 0.3, 6.0, 9.4, 1.1)

  # ============================================================
  # SLIDE 9 — GEOGRAPHIC DISTRIBUTION (select key group maps)
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx,
                     paste0("Where: District-Level Exposure by Group \u2014 ",
                            ctry_name))

  # Show the first two per-group choropleths if available
  grp_paths  <- choropleth_grp_paths[!sapply(choropleth_grp_paths, is.null)]
  show_paths <- grp_paths[seq_len(min(2, length(grp_paths)))]
  map_tops   <- c(0.85, 4.15)
  for (k in seq_along(show_paths)) {
    pptx <- ig(pptx, show_paths[[k]], 0.3, map_tops[k], 9.4, 3.05)
    pptx <- tb(pptx,
               bl(officer::fpar(
                 officer::ftext(
                   paste0(names(show_paths)[k],
                          ": Left = % of total group exposed; ",
                          "Right = % of poor sub-group exposed."),
                   ts(8.5, MGREY)),
                 fp_p = ps("left"))),
               0.3, map_tops[k] + 3.08, 9.4, 0.24)
  }

  # ============================================================
  # SLIDE 10 — CALL TO ACTION
  # ============================================================
  pptx <- officer::add_slide(pptx, layout = "Blank", master = "Office Theme")
  pptx <- add_header(pptx, "What This Means for UNFPA: Implications & Next Steps")

  # Left column: programme implications
  pptx <- tb(pptx, bl(
    officer::fpar(officer::ftext("Programme Implications",
                                 ts(13, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(
      officer::ftext("\u2192  Reproductive health: ", ts(11, ORNGE, bold = TRUE)),
      officer::ftext(
        paste0("Prioritise mobile clinics and emergency obstetric care ",
               "pre-positioning in high-exposure districts for women 15\u201349."),
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext("\u2192  Child & adolescent services: ", ts(11, ORNGE, bold = TRUE)),
      officer::ftext(
        "Districts with high child/adolescent exposure need early-warning ",
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext(
        "links, nutrition pre-positioning, and GBV risk mitigation plans.",
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext("\u2192  Older persons: ", ts(11, ORNGE, bold = TRUE)),
      officer::ftext(
        "Evacuation plans must address mobility constraints for 65+ in identified districts.",
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext("\u2192  Data for advocacy: ", ts(11, ORNGE, bold = TRUE)),
      officer::ftext(
        paste0("These results provide an evidence base for donor proposals, ",
               "government engagement, and cluster coordination in ", ctry_name, "."),
        ts(11, DGREY)),
      fp_p = ps("left"))
  ), 0.4, 0.85, 4.7, 6.1)

  # Divider
  pptx <- tb(pptx,
             bl(officer::fpar(officer::ftext(" ", ts(6, BLUE)),
                              fp_p = ps("left", bg = BLUE))),
             5.35, 0.85, 0.12, 6.1)

  # Right column: next steps / the ask
  pptx <- tb(pptx, bl(
    officer::fpar(officer::ftext("We Are Asking For",
                                 ts(13, NAVY, bold = TRUE)),
                  fp_p = ps("left")),
    officer::fpar(
      officer::ftext("1  ", ts(18, BLUE, bold = TRUE)),
      officer::ftext("Endorse this methodology as the standard UNFPA approach ",
                     ts(11, DGREY, bold = TRUE)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext(
        paste0("     for poverty and flood exposure mapping across the ",
               "country programme."),
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext("2  ", ts(18, BLUE, bold = TRUE)),
      officer::ftext("Allocate resources to extend this analysis ",
                     ts(11, DGREY, bold = TRUE)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext(
        "     to all priority districts, additional hazards (drought, heat), ",
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext(
        "     and future population projections (2025, 2030).",
        ts(11, DGREY)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext("3  ", ts(18, BLUE, bold = TRUE)),
      officer::ftext("Integrate outputs into programme planning cycles, ",
                     ts(11, DGREY, bold = TRUE)),
      fp_p = ps("left")),
    officer::fpar(
      officer::ftext(
        "     situation reports, and donor-facing documents immediately.",
        ts(11, DGREY)),
      fp_p = ps("left"))
  ), 5.6, 0.85, 4.1, 6.1)

  # ── SAVE ───────────────────────────────────────────────────────────────────
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  print(pptx, target = out_path)
  message("PowerPoint saved to: ", out_path)
  invisible(pptx)
}

