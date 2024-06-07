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
#' get_top_detection("049_bremgarten", n = 4)
#'
#' @importFrom ecopiapi get_detections
#' @importFrom dplyr group_by arrange desc slice ungroup slice_sample
#' @noRd
get_top_detection <- function(project, n) {
  detections_with_audio <- ecopiapi::get_detections(
    limit = 200,
    confidence__gte = 0.95,
    has_audio = TRUE,
    only = c("recorder_field_id", "species_code", "datetime", "confidence", "url_media"),
    project_name = project
  )

  detections_with_audio |>
    dplyr::group_by(species_code) |>
    dplyr::arrange(desc(datetime), desc(confidence)) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::slice_sample(n = {{n}})
}

