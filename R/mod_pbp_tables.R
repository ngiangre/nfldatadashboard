#' pbp_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pbp_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
      bslib::layout_column_wrap(
          width = 1/2,
          shiny::selectizeInput(ns("games"),label = 'Select game',
                                choices = NULL,multiple = TRUE,selected = NULL,
                                options = list(
                                    `actions-box` = TRUE)),
          shiny::selectizeInput(ns("cols"),label = 'Select game data',
                                choices = NULL,multiple = TRUE,selected = NULL,
                                options = list(
                                    `actions-box` = TRUE))
      ),
      shiny::dataTableOutput(ns('pbp_table'))
  )
}

#' pbp_tables Server Functions
#'
#' @noRd
mod_pbp_tables_server <- function(id,nfldata_obj){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    shiny::updateSelectizeInput(session = session,inputId = 'cols',
                                choices = nfldata_obj$pbp_table_cols,
                                selected = nfldata_obj$pbp_table_cols[1])
    shiny::updateSelectizeInput(session = session,inputId = 'games',
                                choices = nfldata_obj$game_ids,
                                selected = nfldata_obj$game_ids[1])
    output$pbp_table <- shiny::renderDataTable({
        req(input$cols)
        nfldata_obj$pbp_table |>
            dplyr::select(dplyr::all_of(c("game_id",input$cols))) |>
            dplyr::filter(game_id %in% input$games)
    })
  })
}

## To be copied in the UI
# mod_pbp_tables_ui("pbp_tables_1")

## To be copied in the server
# mod_pbp_tables_server("pbp_tables_1")
