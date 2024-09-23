#' plot_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id character identifier for module.
#' @param width_value numeric Value for initial plot width.
#' @param height_value numeric Value for initial plot height.
#' @param width_selection list List of arguments to splice into do.call for
#' `numericInput`.
#' @param height_selection list List of arguments to splice into do.call for
#' `numericInput`.
#'
#' @noRd
#'
#' @importFrom shiny NS div radioButtons
#' @examples
#' shiny::shinyApp(bslib::page_fluid(
#' mod_plot_size_ui('s')
#' ),function(id,input,output){
#' mod_plot_size_server('s')
#' })
#'
mod_plot_size_ui <- function(id,
                             width_value = 400,
                             height_value = 400,
                             width_input =
                                 list(min=200,max = 4000,step = 100,width = 100),
                             height_input =
                                 list(min=200,max = 4000,step = 100,width = 100),
                             ...){
    stopifnot(is.numeric(width_value))
    stopifnot(is.numeric(height_value))
    stopifnot(is.list(width_input))
    stopifnot(is.list(height_input))
  ns <- NS(id)
  div(
      style = 'display: flex;',
      do.call(shiny::numericInput,c(inputId = ns('width'),
                                    label = 'Image Width',
                                    width_value,
                                    width_input)),
      do.call(shiny::numericInput,c(inputId = ns('height'),
                                    label = 'Image Height',
                                    height_value,
                                    height_input))
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
