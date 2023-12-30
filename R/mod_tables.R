#' tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
      bslib::layout_column_wrap(
          width = 1/2,
          shiny::selectizeInput(ns("players"),label = 'Select player',
                                choices = NULL,multiple = TRUE,selected = NULL,
                                options = list(
                                    `actions-box` = TRUE)),
          shiny::selectizeInput(ns("cols"),label = 'Select player data',
                                    choices = NULL,multiple = TRUE,selected = NULL,
                                    options = list(
                                        `actions-box` = TRUE))
      ),
      shiny::dataTableOutput(ns('player_stats'))
  )
}

#' tables Server Functions
#'
#' @noRd
mod_tables_server <- function(id,nfldata_obj){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    shiny::updateSelectizeInput(session = session,inputId = 'cols',
                                choices = nfldata_obj$player_stats_cols,
                                selected = nfldata_obj$player_stats_cols[1])
    shiny::updateSelectizeInput(session = session,inputId = 'players',
                                choices = nfldata_obj$player_names,
                                selected = nfldata_obj$player_names[1])
    output$player_stats <- shiny::renderDataTable({
        req(input$cols)
        nfldata_obj$player_stats |>
            dplyr::select(dplyr::all_of(c("player_display_name",input$cols))) |>
            dplyr::filter(player_display_name %in% input$players)
    })
  })
}

## To be copied in the UI
# mod_tables_ui("tables_1")

## To be copied in the server
# mod_tables_server("tables_1")
