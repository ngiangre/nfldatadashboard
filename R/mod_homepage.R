#' homepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib page_navbar bs_theme font_google nav_panel tooltip
mod_homepage_ui <- function(id){
  ns <- NS(id)
  bslib::page_navbar(
      theme = bslib::bs_theme(version = 5,base_font = bslib::font_google("Roboto"), font_scale = NULL,
                              `enable-gradients` = TRUE, preset = "zephyr"),
      underline = TRUE,
      bslib::nav_panel(title="Quarterbacks",mod_position_page_ui(ns("qb_page_1"))),
      bslib::nav_panel(title="Wide Receivers",mod_position_page_ui(ns("wr_page_1"))),
      bslib::nav_panel(title="Running Backs",mod_position_page_ui(ns("rb_page_1"))),
      title = bslib::tooltip(list("nfldatadashboard",bsicons::bs_icon("info-circle")),
                             "Dashboard Goal: Visualize NFL player's performance.",
                             "1) Select players and performance indicators.",
                             "2) Compare player performance overall and across games.",
                             "(Future versions: More positions, predict fantasy points, construct fantasy teams.)"
                             )
  )
}

#' homepage Server Functions
#'
#' @noRd
mod_homepage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_obj <- data_object$new()
    mod_position_page_server("qb_page_1",data_obj,ptype="qb")
    mod_position_page_server("wr_page_1",data_obj,ptype="wr")
    mod_position_page_server("rb_page_1",data_obj,ptype="rb")
  })
}

## To be copied in the UI
# mod_homepage_ui("homepage_1")

## To be copied in the server
# mod_homepage_server("homepage_1")
