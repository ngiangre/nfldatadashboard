#' position_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_position_page_ui <- function(id){
  ns <- NS(id)
  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
          width = 400,
          selectizeInput(ns('player'),label = NULL,choices=NULL,multiple = TRUE),
          selectizeInput(ns('stat'),label = NULL,choices=NULL,multiple = TRUE)
      )
  )
}

#' position_page Server Functions
#'
#' @noRd
mod_position_page_server <- function(id,data_obj,ptype = c('qb','wr','rb')){
  ptype <- match.arg(ptype)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #https://stackoverflow.com/questions/73716725/is-there-a-way-to-display-html-inside-a-selectinput-in-an-r-shiny-app
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
                         selected = data_obj$get_named_position_vars(ptype),
                         options = list(render = renderSelectizeUI),
                         label = stringr::str_glue(
                             "Select {stringr::str_to_upper(ptype)} performance indicators"
                         ))

  })
}

## To be copied in the UI
# mod_position_page_ui("position_page_1")

## To be copied in the server
# mod_position_page_server("position_page_1")
