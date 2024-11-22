#' Get Top Detections with Audio
#'
#' This function retrieves the top audio detections for a given project with a confidence level
#' greater than or equal to 0.95. It groups the detections by species, arranges them by datetime
#' and confidence, and returns a specified number of random samples from the top detection of
#' each species.
#'
#' @param project A character string representing the project name.
#' @param n An integer specifying the number of top detections to return.
#'
#' @return A data frame of the top detections with audio for the specified project.
#'
#' @examples
#' get_top_detection("pam_in_chemnitz", n = 4)
#'
#' @import dplyr lubridate
#' @noRd
get_top_detection <- function(project, n) {

  detections_with_audio <- get_detections(
    order_by = "-datetime",
    limit = 1000,
    confidence__gte = 0.99,
    has_audio = TRUE,
    only = c("recorder_field_id", "species_code", "datetime_recording", "start" ,"confidence", "url_media"),
    project_name = project
  )


  detections_with_audio |>
    filter(species_code != "image") |>
    mutate(datetime_snippet = ymd_hms(datetime_recording) + seconds(start)) |>
    group_by(species_code) |>
    arrange(desc(datetime_recording), desc(confidence)) |>
    slice(1) |>
    ungroup() |>
    slice_sample(n = {{n}})
}

