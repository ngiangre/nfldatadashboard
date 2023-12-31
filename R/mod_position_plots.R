#' position_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_position_plots_ui <- function(id){
  ns <- NS(id)
  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
          selectizeInput(ns('player'),label = NULL,choices=NULL,multiple = TRUE),
          selectizeInput(ns('stat'),label = NULL,choices=NULL,multiple = TRUE)
      )
  )
}

#' position_plots Server Functions
#'
#' @noRd
mod_position_plots_server <- function(id,data_obj,ptype = c('qb','wr','rb')){
  ptype <- match.arg(ptype)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    renderSelectizeUI <-
        I("{
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }")
    updateSelectizeInput(session = session,inputId = 'player',
                         choices = data_obj$get_position_named_players(ptype),
                         options = list(render = renderSelectizeUI),
                         label = stringr::str_glue(
                             "Select {stringr::str_to_upper(ptype)} player(s)"
                         ))
    updateSelectizeInput(session = session,inputId = 'stat',
                         choices = data_obj$get_named_position_vars(ptype),
                         options = list(render = renderSelectizeUI),
                         label = stringr::str_glue(
                             "Select {stringr::str_to_upper(ptype)} performance indicators"
                         ))

  })
}

## To be copied in the UI
# mod_position_plots_ui("position_plots_1")

## To be copied in the server
# mod_position_plots_server("position_plots_1")
