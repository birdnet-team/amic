#' valueboxes_detections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_valueboxes_detections_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      value_box(
        title = "Detektionen Heute",
        value = textOutput(ns("n_detections_today_total")),
        theme = "purple",
        height = "100px",
        textOutput(ns("n_species_today_total"))
      ),
      value_box(
        title = "Detektionen gesamt",
        value = textOutput(ns("n_detections_all_total")),
        theme = "teal",
        height = "100px",
        textOutput(ns("n_species_all_total"))
      )
    ),
  )
}

#' valueboxes_detections Server Functions
#'
#' @noRd
mod_valueboxes_detections_server <- function(id, project){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    recorder_species_count_today <- ecopiapi::get_recorderspeciescounts(project_name = project, min_confidence = 0.7)
    recorder_species_count_all <- ecopiapi::get_recorderspeciescounts(project_name = project, start_date = "2020-01-01", min_confidence = 0.7)

    # Card 1; Detections and species today
    output$n_detections_today_total <- renderText({
      sum(recorder_species_count_today$species_count)
    })
    output$n_species_today_total <- renderText({
      n_species_today <- unique(recorder_species_count_today$species_code) |> length()
      sprintf("von %s Arten", n_species_today)
    })

    # Card 1; Detections and species total
    output$n_detections_all_total <- renderText({
      sum(recorder_species_count_all$species_count)
    })
    output$n_species_all_total <- renderText({
      n_species_all <- unique(recorder_species_count_all$species_code) |> length()
      sprintf("von %s Arten", n_species_all)
    })
  })
}

## To be copied in the UI
# mod_valueboxes_detections_ui("valueboxes_detections_1")

## To be copied in the server
# mod_valueboxes_detections_server("valueboxes_detections_1")
