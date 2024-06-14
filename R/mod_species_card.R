#' species_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
#' @import bsicons
mod_species_card_ui <- function(
    id,
    recorder_field_id,
    confidence,
    datetime,
    species_name_common,
    species_name_scientific,
    img_src,
    url_media) {
  ns <- NS(id)

  card_id <- ns("card")
  player_id <- ns("player")
  icon_id <- ns("icon")
  species_image_id <- ns("species_image")

  tagList(card(
    class = "species_card",
    fill = FALSE,
    id = card_id,
    # that's the clickable image that plays sound
    card_body(
      class = "body_sound_image",
      div(
        class = "detection_sound_image",
        tags$audio(id = player_id, src = url_media, type = "application/mp3"),
        tags$img(
          id = species_image_id,
          class = "img-fluid card-img-top",
          src = img_src
        ),
        div(
          id = icon_id,
          class = "overlay_icon",
          HTML('<i class="bi bi-play-fill"></i>')
        ),
      ),
    ),

    # Species metadata chain
    card_body(
      class = "body_data",
      fill = TRUE,
      # padding = 0,
      # gap = 0,
      layout_columns(
        div(
          p(
            strong(species_name_common),
            br(),
            em(species_name_scientific, class = "text-muted")
          ),
          p(
            format(strptime(datetime, "%Y-%m-%d %H:%M:%S"), "%d.%m.%y %R"),
            br(),
            paste0("Rekorder #", recorder_field_id),
            class = "text-muted"
          )
        )
      )
    )
  ))
}


#' species_card Server Functions
#'
#' @noRd
mod_species_card_server <- function(id, rv_detection) {
  moduleServer(id, function(input, output, session) {
  })
}
#
