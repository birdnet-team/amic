#' mod_reactable_detections_per_recorder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr tidyr reactable reactablefmtr

mod_reactable_detections_per_recorder_ui <- function(id) {
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
mod_reactable_detections_per_recorder_server <-
  function(id, data_reactive) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      rv_table <- reactiveValues(data = NULL)

      observe({
        # Prepare the data

        data <- data_reactive() |>
          select(-species_code, -species_name_scientific) |>
          pivot_wider(
            names_from = recorder_field_id,
            values_from = species_count,
            values_fill = 0
          ) |>
          mutate(Summe = rowSums(across(-species_name_common)), .after = "species_name_common")

        rv_table$data <- data
      }) |> bindEvent(data_reactive)


      output$table <- reactable::renderReactable({

        reactable::reactable(
          data = rv_table$data,
          compact = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          sortable = TRUE,
          showSortable = TRUE,
          defaultSorted = list("Summe" = "desc"),
          defaultColDef = colDef(
            defaultSortOrder = "desc",
            align = "left",
            cell = reactablefmtr::data_bars(
              data = rv_table$data,
              fill_color = "#3fc1c9", # rev(viridis::mako(1000)),
              background = "#ffffff",
              text_position = "outside-base",
              # number_fmt = scales::comma,
              max_value = max(rv_table$data$Summe)
            )
          ),
          columns = list(
            species_name_common = colDef(
              name = "Art",
              align = "left",
              minWidth = 130,
            ),
            Summe = colDef(
              cell = reactablefmtr::data_bars(
                data = rv_table$data,
                fill_color = "#fc5185", # rev(viridis::mako(1000)),
                background = "#ffffff",
                text_position = "outside-base"
              )
            )
          )
        )
      }) |>
        bindEvent(rv_table$data)
    })
  }

## To be copied in the UI
# mod_reactable_detections_per_recorder_ui("reactable_detections_1")

## To be copied in the server
# mod_reactable_detections_per_recorder_server("reactable_detections_1")
