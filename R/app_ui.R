#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import echarts4r
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fillable(
      title = "amic",
      fillable_mobile = TRUE,
      theme = bs_theme(
        base_font = font_google("Inter"),
        font_scale = 0.8
      ) |> bs_add_variables(
        "grid-breakpoints" = "(
          xs: 0px,
          sm: 300px,
          md: 500px,
          lg: 992px,
          xl: 1200px,
          xxl: 1400px
        )"
      ),
      mod_valueboxes_detections_ui("valueboxes_detections_1"),
      mod_top_detections_ui("top_detections_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.10.3/font/bootstrap-icons.min.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "amic"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
