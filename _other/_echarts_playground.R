library(httr2)
library(purrr)
library(dplyr)
library(echarts4r)
library(ecopiapi)
library(lubridate)

# Create a vector with start and endtime of the last 24h rounded up to the next hour
this_hour <- Sys.time() |> with_tz("UTC") |> ceiling_date(unit = "hour")
last_24h <- c(
  start_datetime = this_hour - days(1),
  end_datetime = this_hour + hours(1)
)


dets <-
  ecopiapi::get_detections(
    order_by = "-datetime",
    limit = "none",
    confidence__gte = 0.1,
    datetime_recording__gte = last_24h["start_datetime"],
    only = c("species_code", "datetime"),
    project_name = "pam_in_chemnitz"
  ) |>
  mutate(datetime = ymd_hms(datetime, tz = "CET"))

# Bubble Plots -----------------------------------------------------------------------------------------------
timeline_dats <-
  dets |>
  # filter(species_code %in% c("skylar", "sheowl")) |>
  mutate(agg_timeunit = lubridate::floor_date(datetime, unit = "10 mins")) |>
  filter(agg_timeunit > lubridate::ymd("2023-07-30")) |>
  count(species_code, agg_timeunit) |>
  # fill in "time slots" that are missing in the sequence
  tidyr::complete(species_code, agg_timeunit = seq(min(agg_timeunit), max(agg_timeunit), by = "10 min"), fill = list(n = 0L)) |>
  rename(common = species_code) |>
  relocate(agg_timeunit, .before = common) |>
  group_by(common)

timeline_dats |>
  e_charts(
    agg_timeunit,
    #height = "100%",
    #width = "100%"
  ) %>%
  #e_line(common, legend = FALSE, symbol = "none", lineStyle = list(width = 0.5, color = "lightgrey", opacity = 0.4)) %>%
  e_scatter(
    common,
    size = n,
    legend = FALSE,
    scale = \(x)scales::rescale(x, to = c(0,30))
  ) %>%
  e_x_axis(
    name = "",
    type = "time",
    axisLabel = list(
      fontSize = '0.9rem',
      color = "#212529",
      fontFamily = "Arial",
      fontWeight = 400
    )
  ) %>%
  e_y_axis(
    name = "",
    type = "category",
    #margin = 10,
    offset = 16,
    inverse = TRUE,
    axisLabel = list(
      #width = 200,
      #padding = 16,
      overflow = "truncate",
      hideOverlap = FALSE,
      showMinLabel = TRUE,
      showMaxLabel = TRUE,
      align = "right",
      fontSize = '0.9rem',
      color = "#212529",
      #fontFamily = "Arial",
      fontWeight = 400
    ),
    axisTick = list(alignWithLabel = TRUE),
    axisLIne = list(
      onZero = FALSE
    )
  ) %>%
  e_tooltip(trigger = "axis", formatter = htmlwidgets::JS("
              function(params){
                return('<strong>' + params.name + '</strong>' +
                        '<br />detections: ' + params.value[2] +
                        '<br />' + 'Date: ' + params.value[0]
                )
              }
    ")
  ) %>%
  e_toolbox(show = FALSE) %>%
  e_datazoom(type = "slider", xAxisIndex = 0, start = 100, end = 0, brushSelect = FALSE, height = 20) %>%
  e_datazoom(type = "slider", yAxisIndex = 0, start = 0, end = 25, zoomLock = FALSE, brushSelect = FALSE, width = 20) %>%
  identity()



# Rect timeline ---------------------------------------------------------------------------------------------------
source("R/fct_e_scale_symol_size.R")
dets |>
  mutate(agg_timeunit = lubridate::floor_date(datetime, unit = "hour")) |>
  count(species_code, agg_timeunit) |>
  tidyr::complete(species_code, agg_timeunit, fill = list(n = 0)) |>
  #filter(species_code %in% c("skylar", "sheowl")) |>
  rename(common = species_code) |>
  relocate(agg_timeunit, .before = common) |>
  arrange(desc(agg_timeunit)) |>
  filter(agg_timeunit > lubridate::ymd("2023-07-31")) |>
  group_by(common) |>
  mutate(symbol_height = scales::rescale(n, to = c(2, 30))) |>
  e_charts(agg_timeunit) %>%
  e_scatter(common,
            legend = FALSE,
            symbol = "rect") %>%
  e_scale_symbol_size(10, "symbol_height") |>
  e_x_axis(
    name = "",
    type = "time",
    axisLabel = list(
      fontSize = "0.9rem",
      color = "#212529",
      fontFamily = "Arial",
      fontWeight = 400
    )
  ) %>%
  e_y_axis(
    name = "",
    type = "category",
    # margin = 10,
    offset = 16,
    inverse = TRUE,
    axisLabel = list(
      # width = 200,
      # padding = 16,
      overflow = "truncate",
      hideOverlap = FALSE,
      showMinLabel = TRUE,
      showMaxLabel = TRUE,
      align = "right",
      fontSize = "0.9rem",
      color = "#212529",
      fontWeight = 400
    ),
    axisTick = list(alignWithLabel = TRUE),
    axisLIne = list(onZero = FALSE)
  ) %>%
  e_tooltip(triggerOn = "click") %>%
  e_toolbox(show = FALSE) %>%
  e_datazoom(
    type = "slider",
    xAxisIndex = 0,
    start = 100,
    end = 0,
    brushSelect = FALSE,
    height = 20
  ) %>%
  e_datazoom(
    type = "slider",
    yAxisIndex = 0,
    zoomLock = FALSE,
    brushSelect = FALSE,
    width = 20
  ) %>%
  identity()




# stacked area ----------------------------------------------------------------------------------------------------

dets |>
  mutate(agg_timeunit = lubridate::floor_date(datetime, unit = "1 hour")) |>
  count(species_code, agg_timeunit) |>
  #tidyr::complete(species_code, agg_timeunit, fill = list(n = 0)) |>
  filter(species_code %in% c("skylar", "sheowl")) |>
  mutate(symbol_height = scales::rescale(n, to = c(1, 20))) |>
  rename(common = species_code) |>
  relocate(agg_timeunit, .before = common) |>
  tidyr::pivot_wider(id_cols = agg_timeunit, names_from = common, values_from = n, values_fill = 0) |>
  arrange(desc(agg_timeunit)) |>
  slice(1:100) |>
  e_charts(agg_timeunit) |>
  e_area(sheowl, stack = "grp") |>
  e_area (skylar, stack = "grp") |>
  e_toolbox(show = FALSE) %>%
  e_tooltip() |>
  e_datazoom(type = "slider", xAxisIndex = 0, start = 100, end = 0, brushSelect = FALSE, height = 20) %>%
  identity()



# Scatter ---------------------------------------------------------------------------------------------------------


dets |>
  mutate(agg_timeunit = lubridate::floor_date(datetime, unit = "1 hour")) |>
  count(species_code, agg_timeunit) |>
  #tidyr::complete(species_code, agg_timeunit, fill = list(n = 0)) |>
  #filter(species_code %in% c("skylar", "sheowl")) |>
  mutate(symbol_height = scales::rescale(n, to = c(1, 20))) |>
  rename(common = species_code) |>
  relocate(agg_timeunit, .before = common) |>
  group_by(common) |>
  arrange(desc(agg_timeunit)) |>
  slice(1:100) |>
  e_charts(agg_timeunit) |>
  e_scatter(n) |>
  e_toolbox(show = FALSE) %>%
  e_tooltip() |>
  e_datazoom(type = "slider", xAxisIndex = 0, start = 100, end = 0, brushSelect = FALSE, height = 20) %>%
  identity()



# Calendar --------------------------------------------------------------------------------------------------------
