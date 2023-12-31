#' homepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_homepage_ui <- function(id){
  ns <- NS(id)
  bslib::page_navbar(
      underline = TRUE,
      bslib::nav_panel(title="QB",mod_position_plots_ui(ns("qb_plots_1"))),
      bslib::nav_panel(title="WR",mod_position_plots_ui(ns("wr_plots_1"))),
      bslib::nav_panel(title="RB",mod_position_plots_ui(ns("rb_plots_1"))),
      title = "nfldashboard"
  )
}

#' homepage Server Functions
#'
#' @noRd
mod_homepage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_obj <- data_object$new()
    mod_position_plots_server("qb_plots_1",data_obj,ptype="qb")
    mod_position_plots_server("wr_plots_1",data_obj,ptype="wr")
    mod_position_plots_server("rb_plots_1",data_obj,ptype="rb")
  })
}

## To be copied in the UI
# mod_homepage_ui("homepage_1")

## To be copied in the server
# mod_homepage_server("homepage_1")
