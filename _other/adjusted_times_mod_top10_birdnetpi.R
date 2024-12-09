#' top10_birdnetpi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import lubridate hms suncalc dplyr tidyr tibble
mod_top10_birdnetpi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_title("Aktivität der häufigsten Arten", container = htmltools::h3),
      min_height = 450,
      layout_columns(
        col_widths = c(5, 7),
        min_height = 300,
        echarts4rOutput(
          ns("barchart"),
          width = "100%",
          height = "100%"
        ),
        echarts4rOutput(
          ns("activity_heatmap"),
          width = "100%",
          height = "100%"
        )
      )
    )
  )
}

#' top10_birdnetpi Server Functions
#'
#' @noRd
mod_top10_birdnetpi_server <-
  function(id, data_reactive, top_n = 10, last_24h) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      symbol_moon <-
        "path://M6 .278a.77.77 0 0 1 .08.858 7.2 7.2 0 0 0-.878 3.46c0 4.021 3.278 7.277 7.318 7.277q.792-.001 1.533-.16a.79.79 0 0 1 .81.316.73.73 0 0 1-.031.893A8.35 8.35 0 0 1 8.344 16C3.734 16 0 12.286 0 7.71 0 4.266 2.114 1.312 5.124.06A.75.75 0 0 1 6 .278"
      symbol_sunrise <-
        "path://M7.646 1.146a.5.5 0 0 1 .708 0l1.5 1.5a.5.5 0 0 1-.708.708L8.5 2.707V4.5a.5.5 0 0 1-1 0V2.707l-.646.647a.5.5 0 1 1-.708-.708zM2.343 4.343a.5.5 0 0 1 .707 0l1.414 1.414a.5.5 0 0 1-.707.707L2.343 5.05a.5.5 0 0 1 0-.707m11.314 0a.5.5 0 0 1 0 .707l-1.414 1.414a.5.5 0 1 1-.707-.707l1.414-1.414a.5.5 0 0 1 .707 0M11.709 11.5a4 4 0 1 0-7.418 0H.5a.5.5 0 0 0 0 1h15a.5.5 0 0 0 0-1h-3.79zM0 10a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2A.5.5 0 0 1 0 10m13 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5"
      symbol_sunset <-
        "path://M7.646 4.854a.5.5 0 0 0 .708 0l1.5-1.5a.5.5 0 0 0-.708-.708l-.646.647V1.5a.5.5 0 0 0-1 0v1.793l-.646-.647a.5.5 0 1 0-.708.708zm-5.303-.51a.5.5 0 0 1 .707 0l1.414 1.413a.5.5 0 0 1-.707.707L2.343 5.05a.5.5 0 0 1 0-.707zm11.314 0a.5.5 0 0 1 0 .706l-1.414 1.414a.5.5 0 1 1-.707-.707l1.414-1.414a.5.5 0 0 1 .707 0zM11.709 11.5a4 4 0 1 0-7.418 0H.5a.5.5 0 0 0 0 1h15a.5.5 0 0 0 0-1h-3.79zM0 10a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2A.5.5 0 0 1 0 10m13 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5"

      # To keep the order of hours even when it spans over midnight, we need to do some trickery.
      # We only need to do this because echarts can't plot bins nicely with timestamps
      days_adjustment <- tibble(
        datetime = seq(last_24h["start_datetime"], last_24h["end_datetime"] + hours(1), by = "hour"),
        hour = hour(datetime),
        day = yday(datetime),
        multiplier = dense_rank(day) - 1 # multiplier should start at 0
      ) |> select(-datetime)


      # to get a moving 24h activity chart, we need to get the suntimes for two days (except it is exactly midnight, but that doesn't happen)
      sunlighttimes <-
        suncalc::getSunlightTimes(
          date = date(last_24h),
          lat = 50.8,
          lon = 12.9,
          tz = "CET",
          keep = c("sunrise", "sunset")
        ) |>
        pivot_longer(cols = c("sunrise", "sunset")) |>
        mutate(
          hour = hour(round_date(value, "hour")),
          day = yday(date)
          ) |>
        select(name, day, hour)


      # Calc data for Barchart ------------------------------------------------------------------------------------------
      top_dets_per_species <- reactive({
        req(data_reactive())
        data_reactive() |>
          count(species_name_common) |>
          slice_max(n, n = top_n) |>
          mutate(species_level = letters[1:n()])
      })


      # Calc Data for Time Heatmap ----------------------------------------------------------------------------------

      top_dets_per_hour <- reactive({
        req(data_reactive())
        req(top_dets_per_species())

        data_reactive() |>
          filter(species_name_common %in% top_dets_per_species()$species_name_common) |>
          # complete hours for each group
          mutate(
            hour = hour(ymd_hms(datetime)),
            day = yday(datetime)
            ) |>
          count(species_name_common, day, hour) |>
          full_join(days_adjustment) |>
          complete(nesting(day, hour, multiplier), species_name_common, fill = list(n = NA)) |>
          filter(!is.na(species_name_common)) |>
          mutate(adjusted_hour = hour + 24 * multiplier) |>
          left_join(select(top_dets_per_species(), -n)) |>
          arrange(desc(day), desc(hour))
      })


      # BarChart --------------------------------------------------------------------------------------------------------
      output$barchart <- renderEcharts4r({
        req(top_dets_per_species())
        top_dets_per_species() |>
          e_charts(n) |>
          e_bar(
            species_name_common,
            legend = FALSE,
            label = list(show = TRUE, position = c(-1, 0), formatter = "{b}", color = "black"),
            itemStyle = list(color = "#90000000")
          ) |>
          e_bar(
            species_name_common,
            legend = FALSE,
            label = list(show = FALSE),
            itemStyle = list(borderRadius = c(0,10,10,0)),
            barGap = "50%"
          ) |>
          e_x_axis_(
            type = "value",
            max = "max",
            position = "top",
            splitArea = list(show = TRUE),
            axisLine = list(show = FALSE),
            splitLine = list(show = FALSE),
            axisLabel = list(show = TRUE),
          ) |>
          e_y_axis(
            type = "category",
            splitArea = list(show = TRUE),
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            axisLabel = list(
              show = FALSE,
              overflow = "breakAll",
              fontSize = "0.6rem",
              fontWeight = 400
            )
          ) |>
          e_grid(
            containLabel = TRUE,
            left = "1%",
            right = "0%",
            top = "14%",
            bottom = "1%"
          )
      })


      # Time Heatmap PLot ----------------------------------------------------------------------------------------------
      output$activity_heatmap <- renderEcharts4r({
        req(top_dets_per_hour())
        top_dets_per_hour() |>
          group_by(species_name_common) |>
          mutate(symbol_height = scales::rescale(n, to = c(2, 30))) |>
          e_charts(adjusted_hour) |>
          e_scatter(species_name_common, symbol = "rect", legend = FALSE) |>
          # e_scale_symbol_size(15, "symbol_height") |>
          e_x_axis(
            name = "Uhrzeit",
            type = "value",
            min = "dataMin",
            max = "dataMax",
            nameLocation = "middle",
            nameGap = 25,
            position = "top",
            splitArea = list(show = FALSE),
            splitLine = list(show = FALSE),
            axisLine = list(show = FALSE),
            axisLabel = list(
              formatter = htmlwidgets::JS("function(value){return value % 24;}")
            )
          ) |>
          e_y_axis(
            type = "category",
            inverse = TRUE,
            axisLabel = list(show = FALSE),
            splitArea = list(show = TRUE, interval = 0),
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE),
            show = TRUE
          ) |>
          e_grid(
            left = "2%",
            right = "4%",
            top = "14%",
            bottom = "1%"
          ) |>
          e_mark_area(
            data = list(list(xAxis = 0), list(xAxis = sunlighttimes$dawn)),
            itemStyle = list(color = "#102a85", opacity = 0.3)
          ) |>
          e_mark_area(
            data = list(list(xAxis = 0), list(xAxis = sunlighttimes$sunrise)),
            itemStyle = list(color = "lightblue", opacity = 0.5)
          ) |>
          e_mark_area(data = list(list(xAxis = sunlighttimes$sunset), list(xAxis = 24))) |>
          e_mark_area(data = list(list(xAxis = sunlighttimes$dusk), list(xAxis = 24))) |>
          # e_mark_line(
          #   silent = TRUE,
          #   label = list(show = FALSE),
          #   data = list(
          #     xAxis = sunlighttimes$dawn / 2,
          #     symbol = symbol_moon,
          #     symbolOffset = c(0, -15),
          #     symbolKeepAspect = TRUE,
          #     symbolSize = 12
          #   ),
          #   symbol = list("none"),
        #   lineStyle = list(width = 0),
        #   animation = FALSE
        # ) |>
        e_mark_line(
          silent = TRUE,
          label = list(show = FALSE),
          data = list(
            xAxis = sunlighttimes$sunrise,
            symbol = symbol_sunrise,
            symbolOffset = c(0, -30), # c(0, 300),
            symbolKeepAspect = TRUE,
            symbolSize = 17
          ),
          symbol = list("none"),
          lineStyle = list(width = 0),
          animation = FALSE
        ) |>
          e_mark_line(
            silent = TRUE,
            label = list(show = FALSE),
            data = list(
              xAxis = sunlighttimes$sunset,
              symbol = symbol_sunset,
              symbolOffset = c(0, -30), # c(0, 300),
              symbolKeepAspect = TRUE,
              symbolSize = 17
            ),
            symbol = list("none"),
            lineStyle = list(width = 0),
            animation = FALSE
          ) |>
          # e_mark_line(
          #   silent = TRUE,
          #   label = list(show = FALSE),
          #   data = list(
          #     xAxis = (sunlighttimes$dusk + 24) / 2,
          #     symbol = symbol_moon,
          #     symbolOffset = c(0, -15),
          #     symbolKeepAspect = TRUE,
          #     symbolSize = 12
          #   ),
          #   symbol = list("none"),
        #   lineStyle = list(width = 0),
        #   animation = FALSE
        # ) |>
        identity()
      })
    })
  }

## To be copied in the UI
# mod_top10_birdnetpi_ui("top10_birdnetpi_1")

## To be copied in the server
# mod_top10_birdnetpi_server("top10_birdnetpi_1")
