#' mod_reactable_detections_total_percent UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr tidyr reactable reactablefmtr

mod_reactable_detections_total_percent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(
      ns("table"),
      width = "auto",
      height = "auto"
    )
  )
}

#' reactable_detections Server Functions
#'
#' @noRd
mod_reactable_detections_total_percent_server <-
  function(id, data_reactive) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      rv_table <- reactiveValues(data = NULL)

      observe({
        data <- data_reactive() |>
          select(-species_code, -species_name_scientific) |>
          summarise(n = sum(species_count), .by = "species_name_common") |>
          mutate(prop = n / sum(n))
        rv_table$data <- data
      }) |> bindEvent(data_reactive)


      output$table <- reactable::renderReactable({

        golem::message_dev("renderreactable")
        reactable::reactable(
          data = rv_table$data,
          compact = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          sortable = TRUE,
          showSortable = TRUE,
          defaultSorted = list("n" = "desc"),
          columns = list(
            species_name_common = colDef(
              name = "Art",
              align = "left",
              minWidth = 130,
            ),
            n = colDef(
              name = "Anzahl",
              cell = reactablefmtr::data_bars(
                data = rv_table$data,
                fill_color = "purple", # "#fc5181",
                background = "#ffffff",
                text_position = "outside-end",
                bar_height = 10,
                round_edges = TRUE

              )
            ),
            prop = colDef(
              name = "Anteil",
              format = colFormat(percent = TRUE, digits = 1)
              # cell = reactablefmtr::data_bars(
              #   data = foo,
              #   fill_color = "#3fc1c9",
              #   background = "#ffffff",
              #   text_position = "inside-base",
              #   number_fmt = scales::percent,
              # )
            )
          )
        )
      }) |>
        bindEvent(rv_table$data)
    })
  }

## To be copied in the UI
# mod_reactable_detections_total_percent_ui("reactable_detections_1")

## To be copied in the server
# mod_reactable_detections_total_percent_server("reactable_detections_1")
