#' play_on_click_js_script
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(shiny)
library(glue)

play_on_click_js <- function(player_id, species_image_id, icon_id) {
  js_code <- sprintf(
    "
    const player = document.getElementById('%s');
    const icon = document.getElementById('%s');
    const speciesImage = document.getElementById('%s');

    speciesImage.addEventListener('click', function () {
      if (player.paused) {
        player.play();
        icon.innerHTML = '<i class=\"bi bi-pause-fill\"></i>';
      } else {
        player.pause();
        icon.innerHTML = '<i class=\"bi bi-play-fill\"></i>';
      }
    });

    player.addEventListener('ended', function () {
      icon.innerHTML = '<i class=\"bi bi-play-fill\"></i>';
    });
    ",
    player_id, icon_id, species_image_id
  )

  js_code
}
