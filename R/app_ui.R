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
    tags$head(
      tags$style(HTML("
      #header, #footer {
        width: calc(100% + 3rem);  /* Full width plus compensation for left and right padding */
        margin-left: -1.5rem;  /* Move left to counteract left padding */
        margin-right: -1.5rem;  /* Move right to counteract right padding */
        margin-top: -1rem;
        padding: 10 !important;  /* Reset all padding */
      }
    "))
    ),

    # Header
    tags$div(
      id = "header",
      style = "height: 100px; background-color: #FFFFFF; display: flex; align-items: flex-end; padding: 10px;",
      tags$img(src = "www/rotkehlchen.png", height = "90px", style = "margin-right: 20px; align-self: flex-end;"),
      tags$div(
        style = "display: flex; flex-direction: column; justify-content: flex-end;",
        tags$h3("Akustisches Monitoring Chemnitz", style = "margin-bottom: 10; margin: 10; font-size: 15px;"),
        tags$h1("Von der Stadt zum Wald", style = "margin: 0; font-size: 20px;")
      )
    ),
    # Main Page
    page_fillable(
      title = "Akustisches Monitoring in Chemnitz",
      fillable_mobile = TRUE,
      padding = "1.5rem",
      theme = bs_theme(
        # base_font = font_google("Inter"),
        # heading_font = font_google("Inter", wght = 600, local = FALSE),
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
      card(
        card_title("Erkannte Rufe und Gesänge", container = htmltools::h3),
        layout_columns(
          width = 1/2,
          fill = FALSE,
          mod_value_box_ui("box_today", "Heute", "purple", "120px", "120px", "150px"),
          mod_value_box_ui("box_all", "Gesamt", "teal", "120px", "120px", "150px")
        ),
        min_height = "220px",
        max_height = "250px"
      ),
      mod_top10_birdnetpi_ui("top10_birdnetpi_1"),
      mod_top_detections_ui("top_detections_1"),
      hr(style = "margin-bottom: 0px;")
      # Footer

      # htmltools::tags$footer(
      #   class = "main-footer",
      #   style = "height: 40px; width: 100%; background-color: #343a40; color: white; text-align: center; padding: 10px 0; position: fixed; bottom: 0;",
      #   shiny::tags$div(class = "pull-right hidden-xs", "text"),
      #   "text"
      # )
    ),
    # Footer
    tags$div(
      class = "footer",
      style = "height: 80px; background-color: #FFFFF; display: flex; justify-content: space-between; align-items: flex-end; padding: 0px; padding-bottom: 1rem;",

      # Left side with contact information
      tags$div(
        style = "text-align: left;",
        tags$p("Kontakt:", style = "margin: 0;"),
        tags$p("Dr. Stefan Kahl", style = "margin: 0;"),
        tags$p("stefan.kahl@cs.tu-chemnitz.de", style = "margin: 0;"),
        tags$p("+49 (0)371 531-32219", style = "margin: 0;")
      ),

      # Right side with BirdNET logo and text
      tags$div(
        style = "display: flex; flex-direction: column; align-items: flex-end; text-align: right;",
        tags$img(src = "www/logo-birdnet_circled.png", height = "40px", style = "margin-bottom: 5px;"),
        tags$a(
          "Powered by BirdNET",
          href = "https://birdnet.cornell.edu/",
          style = "margin: 0; color: blue; text-decoration: underline; cursor: pointer;"
        )
      )
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
      app_title = "Akustisches Monitoring in Chemnitz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
