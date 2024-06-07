#' top_detections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ecopiapi
#' @importFrom dplyr select left_join
mod_top_detections_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns("species_cards"))
    )
  )
}

#' top_detections Server Functions
#'
#' @noRd
mod_top_detections_server <- function(id, project, n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # prepare the data
    birdnames <- birdnet_codes_v24 |>
      select(species_code = speciesCode, species_name_common = comName_de, species_name_scientific = sciName)

    top_detections <-
      get_top_detection(project, n) |>
      left_join(birdnames) |>
      left_join(species_images) |>
      select(-species_code, -author)
    top_detections$id <- seq_len(nrow(top_detections)) # id is used for namespacing

    golem::print_dev(top_detections)
    golem::print_dev(top_detections$url_media)

    list_top_detections <- convert_df_to_named_lists(top_detections)

    output$species_cards <- renderUI({
      species_cards <- lapply(list_top_detections, function(x) {
        do.call(mod_species_card_ui, args = x)
      })
      list(
        species_cards[[1]],
        species_cards[[3]]
      )


    })

  })
}

## To be copied in the UI
# mod_top_detections_ui("top_detections_1")

## To be copied in the server
# mod_top_detections_server("top_detections_1")
