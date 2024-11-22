#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import pkgload
#' @importFrom lubridate now floor_date with_tz round_date ymd_hms
#' @importFrom dplyr select left_join mutate
#' @noRd
app_server <- function(input, output, session) {
  #
  project <- "pam_in_chemnitz"
  min_confidence <- 0.85

  ## timezone of the project; could be retrieved from the recorder config
  # project_timezone <- "Europe/Berlin"
  ## there might be a mismatch when the shiny server runs on UTC but its past midnight at the recorder location
  ## find midnight in UTC (DB server time) relative to prject timezone when shiny app runs on a server with an unknown time zone
  # today <- now(tzone = project_timezone) |>
  #   floor_date(unit = "day") |>
  #   with_tz("UTC")

  # Create a vector with start and endtime of the last 24h rounded up to the next hour
  this_hour <- Sys.time() |> with_tz("CET") |> ceiling_date(unit = "hour")
  last_24h <- c(
    start_datetime = this_hour - days(1),
    end_datetime = this_hour + hours(1)
  )
  # Set the class to POSIXct to preserve datetime format
  class(last_24h) <- c("POSIXct", "POSIXt")


  birdnames <- birdnet_codes_v24 |>
    dplyr::select(
      species_code = speciesCode,
      species_name_common = comName_de,
      species_name_scientific = sciName
    )

  recorder_species_count_template <-
   tibble::tibble(
      species_code = character(),
      recorder_field_is = integer(),
      species_count = integer()
    )

  # Get and cache Data
  recorder_species_count_all <- reactive({
    resp <- get_recorderspeciescounts(
      project_name = project,
      start_date = "2020-01-01",
      min_confidence = min_confidence
      )
    if (length(resp) == 0)
      return(recorder_species_count_template)

    resp |>
      left_join(birdnames)
  })


  recorder_species_count_today <- reactive({
    resp <- get_recorderspeciescounts(
      project_name = project,
      min_confidence = min_confidence
      # start_date by default set to today
      )

    if (length(resp) == 0)
      return(recorder_species_count_template)

    resp |>
      left_join(birdnames)
  })

  detections_today <- reactive({
    resp <-
      get_detections(
      order_by = "-datetime",
      limit = "none",
      confidence__gte = min_confidence,
      datetime_recording__gte =  paste0(lubridate::today(), "T00:00:00"), # last_24h["start_datetime"],
      only = c("species_code", "datetime"),
      project_name = project
    )

    if (length(resp) == 0 | is.null(resp))
      return(NULL)

    resp |>
      left_join(birdnames)
  })

  # Use nested modules for value boxes
  mod_value_box_server("box_today", recorder_species_count_today, "Alle Erkennungen Heute")
  mod_value_box_server("box_all", recorder_species_count_all, "Alle Erkennungen")
  mod_top10_birdnetpi_server("top10_birdnetpi_1", detections_today, top_n = 10)

  mod_top_detections_server("top_detections_1", project = project, n = 12)
  # mod_summary_server("valueboxes_detections_1", project = project)
}
