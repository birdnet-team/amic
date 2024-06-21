# R/mod_summary.R

#' valueboxes_detections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Erkennungen"),
    layout_columns(
      width = 1/2,
      fill = FALSE,
      mod_value_box_ui(ns("box_today"), "Heute", "purple", "120px", "120px", "150px"),
      mod_value_box_ui(ns("box_all"), "Gesamt", "teal", "120px", "120px", "150px")
    )
  )
}

#' valueboxes_detections Server Functions
#'
#' @noRd
mod_summary_server <- function(id, project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # used to invalidate the cache every 10 minutes
    rounded_time <- reactive({
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
      ecopiapi::get_recorderspeciescounts(project_name = project, start_date = "2020-01-01") |>
        left_join(birdnames)
    }) |>
      bindCache(rounded_time())


    recorder_species_count_today <- reactive({
      ecopiapi::get_recorderspeciescounts(project_name = project) |>
        left_join(birdnames)
    }) |>
      bindCache(rounded_time())

    # Use nested modules for value boxes
    mod_value_box_server("box_today", recorder_species_count_today, "Alle Erkennungen Heute")
    mod_value_box_server("box_all", recorder_species_count_all, "Alle Erkennungen")
  })
}

## To be copied in the UI
# mod_summary_ui("valueboxes_detections_1")

## To be copied in the server
# mod_summary_server("valueboxes_detections_1")
