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


  mod_top_detections_server("top_detections_1", project = "oekofor", n = 4)
}
