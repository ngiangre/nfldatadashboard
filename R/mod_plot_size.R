#' plot_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id character identifier for module
#' @param width_selection numeric Range of values for plot width
#' @param height_selection numeric Range of values for plot height
#'
#' @noRd
#'
#' @importFrom shiny NS div radioButtons
mod_plot_size_ui <- function(id,width_selection=seq(800,2400,200),
                             height_selection=seq(800,2400,200)){
  ns <- NS(id)
  div(
      shiny::radioButtons(ns('width'),label = 'Image Width',
                          choices = width_selection,
                          selected = median(width_selection),
                          inline = TRUE),
      shiny::radioButtons(ns('height'),label = 'Image Height',
                          choices = height_selection,
                          selected = median(height_selection),
                          inline = TRUE)
  )
}

#' plot_size Server Functions
#'
#' @noRd
mod_plot_size_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    list(
        width = reactive(input$width),
        height = reactive(input$height)
    )
  })
}

## To be copied in the UI
# mod_plot_size_ui("plot_size_1")

## To be copied in the server
# mod_plot_size_server("plot_size_1")
