#' convert_ogg_audio
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
library(av)
library(httr)

convert_ogg_audio <- function(ogg_url) {
  # Determine the output directory based on the OS
  output_dir <- resourcePaths()["tmp_path"]

  # Create a unique file name
  ogg_file <- tempfile(fileext = ".ogg", tmpdir = output_dir)
  mp4_file <- tempfile(fileext = ".mp4", tmpdir = output_dir)

  # Download the .ogg file
  GET(ogg_url, write_disk(ogg_file, overwrite = TRUE))

  # Convert the .ogg file to .mp4
  av_audio_convert(ogg_file, output = mp4_file)

  # Remove the .ogg file
  file.remove(ogg_file)

  # Return the path to the .mp4 file
  return(mp4_file)
}
