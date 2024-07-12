#' value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs

# UI for the value box module
mod_value_box_ui <- function(id, title, theme, height, min_height, max_height) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "skeleton.css"),
      tags$script(HTML(paste0('
        $(document).on("click", "#', ns('value_box'), '", function() {
          Shiny.setInputValue("', ns('value_box_clicked'), '", Math.random());
        });
      ')))
    ),
    div(
      id = ns("loading_skeleton"),
      class = "skeleton",
      div(class = "skeleton-box"),
      # div(class = "skeleton-title"),
      # div(class = "skeleton-text"),
      # div(class = "skeleton-text")
    ),
    hidden(
      value_box(
        title = title,
        value = textOutput(ns("value")),
        theme = theme,
        height = height,
        min_height = min_height,
        max_height = max_height,
        id = ns("value_box"),
        class = "value-box-clickable",
        textOutput(ns("details"))
      )
    )
  )
}


# Server logic for the value box module
mod_value_box_server <- function(id, data_reactive, data_title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_loaded <- reactiveVal(FALSE)

    observe({
      data <- data_reactive()
      if (!is.null(data)) {
      # if(FALSE) { # use this to show loading skeleton permanently
        data_loaded(TRUE)
        shinyjs::hide(id = "loading_skeleton")
        shinyjs::show(id = "value_box")
      } else {
        data_loaded(FALSE)
        shinyjs::show(id = "loading_skeleton")
        shinyjs::hide(id = "value_box")
      }
    })

    output$value <- renderText({
      req(data_reactive())
      sum(data_reactive()$species_count)
    })

    output$details <- renderText({
      req(data_reactive())
      n_species <- unique(data_reactive()$species_code) |> length()
      sprintf("von %s Arten", n_species)
    })

    observeEvent(input$value_box_clicked, {
      showModal(
        modalDialog(
          title = data_title,
          mod_reactable_detections_total_percent_ui(ns("reactable_detections")),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      mod_reactable_detections_total_percent_server("reactable_detections", data_reactive)
    })
  })
}




## To be copied in the UI
# mod_value_box_ui("value_box_1")

## To be copied in the server
# mod_value_box_server("value_box_1")
# R/mod_value_box.R
