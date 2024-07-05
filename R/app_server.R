#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import pkgload
#' @noRd
app_server <- function(input, output, session) {
  #
  project <- "pam_in_chemnitz"
  min_confidence <- 0.85

  # used to invalidate the cache every 10 minutes
  rounded_time <- reactive({
    invalidateLater(300000)
    lubridate::round_date(Sys.time(), unit = "10 mins")
  })

  birdnames <- birdnet_codes_v24 |>
    dplyr::select(
      species_code = speciesCode,
      species_name_common = comName_de,
      species_name_scientific = sciName
    )

  # Get and cache Data
  recorder_species_count_all <- reactive({
    ecopiapi::get_recorderspeciescounts(
      project_name = project,
      start_date = "2020-01-01",
      min_confidence = min_confidence
      ) |>
      left_join(birdnames)
  }) |>
    bindCache(rounded_time())


  recorder_species_count_today <- reactive({
    ecopiapi::get_recorderspeciescounts(
      project_name = project,
      min_confidence = min_confidence
      # start_date by default set to today
      ) |>
      left_join(birdnames)
  }) |>
    bindCache(rounded_time())

  detections_today <- reactive({
    ecopiapi::get_detections(
      order_by = "-datetime",
      limit = "none",
      confidence__gte = min_confidence,
      datetime__gte = Sys.Date() |> paste0("T00:00:00Z"),
      only = c("species_code", "datetime"),
      project_name = project
    ) |>
      left_join(birdnames)
  }) |>
    bindCache(rounded_time())

  # Use nested modules for value boxes
  mod_value_box_server("box_today", recorder_species_count_today, "Alle Erkennungen Heute")
  mod_value_box_server("box_all", recorder_species_count_all, "Alle Erkennungen")
  mod_top10_birdnetpi_server("top10_birdnetpi_1", detections_today, top_n = 10)

  mod_top_detections_server("top_detections_1", project = project, n = 12)
  # mod_summary_server("valueboxes_detections_1", project = project)
}
