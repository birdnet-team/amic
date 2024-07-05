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
#' @importFrom dplyr select left_join mutate rowwise
mod_top_detections_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(src = "www/audio_player.js"),
    h3("Rufe und Gesänge anhören"),
    htmlOutput(ns("species_cards"))
  )
}

#' top_detections Server Functions
#'
#' @noRd
mod_top_detections_server <- function(id, project, n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare the data
    birdnames <- birdnet_codes_v24 |>
      select(species_code = speciesCode, species_name_common = comName_de, species_name_scientific = sciName)

    top_detections <- get_top_detection(project, n) |>
      left_join(birdnames) |>
      left_join(species_images) |>
      select(-species_code, -author) |>
      # should fix an error in safari that complains about too many redirects
      mutate(url_media = sub("http://", "https://", url_media))
    top_detections$id <- seq_len(nrow(top_detections)) # id is used for namespacing

    # golem::message_dev("TOP DETECTIONS")
    # golem::print_dev(top_detections)
    # golem::print_dev(top_detections$url_media)

    list_top_detections <- convert_df_to_named_lists(top_detections)

    list_species_cards <- lapply(list_top_detections, function(x) {
      do.call(mod_species_card_ui, args = x)
    })

    output$species_cards <- renderUI({
      layout_columns(
        col_widths = breakpoints(
          sm = c(6,6),
          md = c(3,3,3,3)
        ),
        !!!list_species_cards
      )
    })

    session$sendCustomMessage(type = "newCardsAdded", message = list(targetLength = length(list_species_cards)))
  })
}


## To be copied in the UI
# mod_top_detections_ui("top_detections_1")

## To be copied in the server
# mod_top_detections_server("top_detections_1")
