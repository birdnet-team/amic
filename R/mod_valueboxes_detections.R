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

  )
}

#' valueboxes_detections Server Functions
#'
#' @noRd
mod_valueboxes_detections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns




  })
}

## To be copied in the UI
# mod_valueboxes_detections_ui("valueboxes_detections_1")

## To be copied in the server
# mod_valueboxes_detections_server("valueboxes_detections_1")
