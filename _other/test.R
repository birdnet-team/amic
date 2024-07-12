library(dplyr)
library(tidyr)
library(lubridate)
library(echarts4r)
library(suncalc)
load("data/birdnet_codes_v24.rda")

project = "pam_in_chemnitz"

birdnames <- birdnet_codes_v24 |>
  select(
    species_code = speciesCode,
    species_name_common = comName_de,
    species_name_scientific = sciName
  )


dets <- ecopiapi::get_detections(
  order_by = "-datetime",
  limit = "none",
  confidence__gte = 0.1,
  datetime__gte = Sys.Date() |> paste0("T00:00:00Z"),
  # has_audio = TRUE,
  # only = c("recorder_field_id", "species_code", "datetime", "confidence", "url_media"),
  only = c("species_code", "datetime"),
  project_name = project
) |>
  left_join(birdnames)

top_10_dets_per_species <-
  dets |>
  count(species_name_common) |>
  arrange(desc(n)) |>
  slice(1:10) |>
  mutate(species_level = letters[1:10]) |>
  mutate(total = sum(n), remaining = total - n, prop = (n / total))

top_10_dets_per_hour <-
  dets |>
  filter(species_name_common %in% top_10_dets_per_species$species_name_common) |>
  mutate(hour_of_day = hour(datetime) |> as.numeric()) |>
  count(species_name_common, hour_of_day) |>
  # the following dance is done to interpolate to half hour values to make a nicer graph
  # first, fill in zeros (0) in the range of data that is currently available for full hours where the recorders
  # actually recorded (in this specific deployment)
  # we just want to interpolate intermediate, made up values
  group_by(species_name_common) |>
  group_modify(~ {
    if(nrow(.x) < 1) {
      .x
    } else {
      inter <- .x |>
        tidyr::complete(hour_of_day = seq(min(hour_of_day), max(hour_of_day), by = 1), fill = list(n = 0)) |>
        # then fill in half hour values for interpolation per group
        tidyr::complete(hour_of_day = seq(min(hour_of_day), max(hour_of_day), by = 0.5)) |>
        # now fill in cells where the value does not change (this wouldnt be an issue if we'd use approx(method = "linear"))
        mutate(lag_n = lag(n),
               lead_n = lead(n),
               n = if_else(is.na(n) & lag_n == lead_n, lead_n, n)
        ) |>
        select(-lag_n, -lead_n) |>
        # Then interpolate those NA values using splines to give it some natural feel
        mutate(n_interpol = spline(hour_of_day, n, xout = hour_of_day, method = "natural")$y) |>
        # spliones may become negative and that doesnt make sense in this case
        mutate(n_interpol = if_else(n_interpol < 0, 0, round(n_interpol, 1))) |>
        # fill in zeros for every full hour, where we actually hav recordings
        tidyr::complete(hour_of_day = seq(1, 24, by = 0.5), fill = list(n = NA))
      inter
    }
  }) |>
  ungroup() |>
  arrange(species_name_common) |>
  left_join(select(top_10_dets_per_species, -n)) |>
  arrange(species_level)

sunlighttimes <-
  getSunlightTimes(date = Sys.Date(), lat = 50.8, lon = 12.9, tz = "CET", keep = c("sunrise", "sunset", "dusk", "dawn")) |>
  pivot_longer(cols = c("sunrise", "sunset", "dusk", "dawn")) |>
  mutate(hour_of_day = as.numeric(hms::as_hms(value)) / 3600) |>
  select(name, hour_of_day) |>
  pivot_wider(names_from = "name", values_from = "hour_of_day")




symbol_moon <- "path://M6 .278a.77.77 0 0 1 .08.858 7.2 7.2 0 0 0-.878 3.46c0 4.021 3.278 7.277 7.318 7.277q.792-.001 1.533-.16a.79.79 0 0 1 .81.316.73.73 0 0 1-.031.893A8.35 8.35 0 0 1 8.344 16C3.734 16 0 12.286 0 7.71 0 4.266 2.114 1.312 5.124.06A.75.75 0 0 1 6 .278"
symbol_sunrise <- "path://M7.646 1.146a.5.5 0 0 1 .708 0l1.5 1.5a.5.5 0 0 1-.708.708L8.5 2.707V4.5a.5.5 0 0 1-1 0V2.707l-.646.647a.5.5 0 1 1-.708-.708zM2.343 4.343a.5.5 0 0 1 .707 0l1.414 1.414a.5.5 0 0 1-.707.707L2.343 5.05a.5.5 0 0 1 0-.707m11.314 0a.5.5 0 0 1 0 .707l-1.414 1.414a.5.5 0 1 1-.707-.707l1.414-1.414a.5.5 0 0 1 .707 0M11.709 11.5a4 4 0 1 0-7.418 0H.5a.5.5 0 0 0 0 1h15a.5.5 0 0 0 0-1h-3.79zM0 10a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2A.5.5 0 0 1 0 10m13 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5"
symbol_sunset <- "path://M7.646 4.854a.5.5 0 0 0 .708 0l1.5-1.5a.5.5 0 0 0-.708-.708l-.646.647V1.5a.5.5 0 0 0-1 0v1.793l-.646-.647a.5.5 0 1 0-.708.708zm-5.303-.51a.5.5 0 0 1 .707 0l1.414 1.413a.5.5 0 0 1-.707.707L2.343 5.05a.5.5 0 0 1 0-.707zm11.314 0a.5.5 0 0 1 0 .706l-1.414 1.414a.5.5 0 1 1-.707-.707l1.414-1.414a.5.5 0 0 1 .707 0zM11.709 11.5a4 4 0 1 0-7.418 0H.5a.5.5 0 0 0 0 1h15a.5.5 0 0 0 0-1h-3.79zM0 10a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2A.5.5 0 0 1 0 10m13 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5"


barchart <-
  top_10_dets_per_species |>
  e_charts(n) |>
  # e_bar(
  #   species_name_common,
  #   legend = FALSE,
  #   label = list(show = TRUE, position = c(-3, 0), formatter = "{b}", color = "black"),
  #   itemStyle = list(color = "#90000000"),
  #   showBackground = FALSE
  # ) |>
  e_bar(
    species_name_common,
    legend = FALSE,
    label = list(show = FALSE, position = "insideLeft"),
    itemStyle = list(borderRadius = c(0,10,10,0)),
    barGap = "100%"
  ) |>
  e_x_axis_(
    type = "value",
    # max = "max",
    # position = "top",
    # axisLine = list(show = TRUE),
    # splitLine = list(show = FALSE),
    axisLabel = list(show = FALSE, showMaxLabel = TRUE)
  ) |>
  e_y_axis(
    type = "category",
    splitArea = list(show = TRUE, interval = 0),
    axisLabel = list(
      show = FALSE,
      overflow = "breakAll",
      fontSize = "0.7rem",
      fontWeight = 600
    )
  ) |>
#  e_grid(left = "10%", right = "10%", top = "10%", bottom = "10%")
  identity()
barchart


heatmap <-
  top_10_dets_per_hour |>
  # filter(species_name_common != "Amsel") |>
  # group_by(species_level) |>
  e_charts(hour_of_day) |>
  e_heatmap(species_level, n_interpol, label = list(show = FALSE)) |>
  e_visual_map(
    n,
    type = "continuous",
    min = 1,
    color = rev(
      c(
        "#fcde9c",
        "#faa476",
        "#f0746e",
        "#e34f6f",
        "#dc3977",
        "#b9257a",
        "#7c1d6f"
      )
    ),
    show = FALSE
  ) |>
  e_x_axis(
    name = "Uhrzeit",
    nameLocation = "middle",
    nameGap = 25,
    splitArea = list(show = FALSE),
    type = "value",
    position = "top",
    max = 24
  ) |>
  e_y_axis(
    splitArea = list(show = TRUE, interval = 2),
    type = "category",
    inverse = TRUE,
    axisLabel = list(show = FALSE),
    show = TRUE
  ) |>
  e_grid(
    left = "1%"
  ) |>
  e_mark_area(
    data = list(list(xAxis = 0), list(xAxis = sunlighttimes$dawn)),
    itemStyle = list(color = "#102a85", opacity = 0.3)
  ) |>
  e_mark_area(
    data = list(list(xAxis = 0), list(xAxis = sunlighttimes$sunrise)),
    itemStyle = list(color = "lightblue", opacity = 0.5)
  ) |>
  e_mark_area(
    data = list(list(xAxis = sunlighttimes$sunset), list(xAxis = 24))
  ) |>
  e_mark_area(
    data = list(list(xAxis = sunlighttimes$dusk), list(xAxis = 24))
  ) |>
  # e_mark_line(
  #   silent = TRUE,
  #   label = list(show = FALSE),
  #   data = list(
  #     xAxis = sunlighttimes$dawn / 2,
  #     symbol = symbol_moon,
  #     symbolOffset = c(0, -15),
  #     symbolKeepAspect = TRUE,
  #     symbolSize = 12
  #     ),
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
      symbolOffset = c(0, -15),
      symbolKeepAspect = TRUE,
      symbolSize = 15
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
      symbolOffset = c(0, -15),
      symbolKeepAspect = TRUE,
      symbolSize = 15
    ),
    symbol = list("none"),
    lineStyle = list(width = 0),
    animation = FALSE
  ) |>
  # e_mark_line(
  #   silent = TRUE,
  #   label = list(show = FALSE),
  #   data = list(
  #     xAxis = (sunlighttimes$dusk + 24) / 2 ,
  #     symbol = symbol_moon,
  #     symbolOffset = c(0, 400),
  #     symbolKeepAspect = TRUE,
  #     symbolSize = 12
  #   ),
  #   symbol = list("none"),
  #   lineStyle = list(width = 0),
  #   animation = FALSE
  # ) |>
  identity()

e_arrange(barchart, heatmap, cols = 2)
