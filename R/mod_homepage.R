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
      bslib::nav_panel(title="QB"),
      bslib::nav_panel(title="WR"),
      bslib::nav_panel(title="RB"),
      title = "nfldashboard"
  )
}

#' homepage Server Functions
#'
#' @noRd
mod_homepage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    nfldata_obj <- nfldata_object$new()
    mod_tables_server("tables_1",nfldata_obj)
    mod_pbp_tables_server("pbp_tables_1",nfldata_obj)
  })
}

## To be copied in the UI
# mod_homepage_ui("homepage_1")

## To be copied in the server
# mod_homepage_server("homepage_1")
