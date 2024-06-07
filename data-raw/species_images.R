## code to prepare `species_images` dataset goes here
library(dplyr)
library(tibble)
library(tidyr)


species_images <-
  list.files("inst/app/www/birdnet_app_images_3k/") |>
  as_tibble_col(column_name = "img_files") |>
  mutate(img_src = paste0("www/birdnet_app_images_3k/", img_files)) |>
  separate_wider_delim(
    img_files,
    delim = "_",
    names = c(
      "species_name_scientific",
      "species_name_common",
      "author"
    ),
    too_few = "align_start"
  ) |>
  mutate(author = sub(pattern = ".webp", "", author)) |>
  select(-species_name_common)

usethis::use_data(species_images, overwrite = TRUE)
