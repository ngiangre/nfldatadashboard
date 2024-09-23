#' position_page UI Function
#'
#' @description A shiny Module to display plots for the selected position.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput plotOutput
#' @importFrom ggiraph girafeOutput
#' @importFrom bslib tooltip sidebar layout_sidebar accordion accordion_panel
#' @importFrom bsicons bs_icon
mod_position_page_ui <- function(id){
  ns <- NS(id)
  bslib::navset_card_tab(
      full_screen = FALSE, #ggiraph doesn't work when full screen?
      bslib::nav_panel(
          bslib::tooltip(list("Side-by-Side Player Comparisons",bsicons::bs_icon('info-circle')),
                         "Two plots displaying the average performance of the selected players."
          ),
          tags$b("Select players to compare performance overall and across games. Performance statistics are described within the left sidebar. Hover over label icons for more information. Use cases: 1) Compare the performance of players in each team for the next football game, 2) Show player performance for my favorite player during the 2023 NFL season, 3) Visusalize how player performance varied game-by-game."),
          bslib::accordion(
              open = c("Side-By-Side Overall Performance","Week-By-Week Season Performance"),
              bslib::accordion_panel(
                  "Side-By-Side Overall Performance",
                  mod_plot_size_ui(ns("plot_size_1"),
                                   width_value = 600,
                                   height_value = 500),
                  uiOutput(ns('heatmap_ui'))
              ),
              bslib::accordion_panel(
                  "Week-By-Week Season Performance",
                  mod_plot_size_ui(ns("plot_size_2"),
                                   width_value = 600,
                                   height_value = 500),
                  uiOutput(ns('sw_week_scatterplot_ui'))
              )
          )
      ),
      bslib::nav_panel(
          bslib::tooltip(list("Comparison With All Players",bsicons::bs_icon('info-circle')),
                         "Two plots displaying all players in this position (gray dots).",
                         "Hover over dots to label players and performance."
          ),
          tags$b("Exploratory comparison of player performance in the 2023 NFL season. Performance statistics are described within the left sidebar. Hover over the points in the plots, which are individual players, for more information. Use cases: 1) Identify players with the highest performace for a statistic, 2) Compare player performance across many performance statistics, 3) Identify consistently performant players across maany performance statistics."),
          bslib::accordion(
              open = c("Overall Performance","Overall Performance vs. Season Variability"),
              bslib::accordion_panel(
                  "Overall Performance",
                  mod_plot_size_ui(ns("plot_size_3"),
                                   width_value = 600,
                                   height_value = 500),
                  uiOutput(ns('sa_distribution_plot_ui'))
              ),
              bslib::accordion_panel(
                  "Overall Performance vs. Season Variability",
                  mod_plot_size_ui(ns("plot_size_4"),
                                   width_value = 600,
                                   height_value = 500),
                  uiOutput(ns('sw_scatterplot_ui'))
              )
          )
      )
  )
}

#' position_page Server Functions
#'
#' @noRd
#' @importFrom shiny reactive req renderPlot updateSelectizeInput observeEvent
#' @importFrom ggiraph renderGirafe girafe opts_hover opts_hover_inv
#' @importFrom bslib update_tooltip
#' @importFrom stringr str_glue
#' @importFrom dplyr select all_of filter
#' @importFrom tidyr pivot_longer
mod_position_page_server <- function(id,data_obj,ptype,subdat_sa,subdat_sw,alldat_sa,alldat_sw,player_input){
  stopifnot(ptype %in% data_obj$get_available_positions())
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    plot_obj = plot_object$new()
    plot_size1 <- mod_plot_size_server("plot_size_1")
    observeEvent(c(plot_size1$width(),plot_size1$height()),{
        output$sa_heatmap <- renderPlot({
            req(nrow(subdat_sa())>=1)
            plot_obj$sa_heatmap(subdat_sa())
        })
        output$heatmap_ui <- shiny::renderUI({
            div(
                style = 'display: flex;',
                plotOutput(ns('sa_heatmap'),
                           width = plot_size1$width(),
                           height = plot_size1$height())
            )
        })
    })
    plot_size2 <- mod_plot_size_server("plot_size_2")
    observeEvent(c(plot_size2$width(),plot_size2$height()),{
        output$sw_week_scatterplot <- renderGirafe({
            req(nrow(subdat_sw())>=1)
            p <- plot_obj$sw_week_scatterplot(subdat_sw())
            girafe(code = print(p),
                   width_svg = (1/96)*plot_size2$width(),
                   height_svg = (1/96)*plot_size2$height(),
                   options = list(
                       opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                       opts_hover_inv(css = "opacity:0.1")
                   ))
        })
        output$sw_week_scatterplot_ui <- shiny::renderUI({
            girafeOutput(ns('sw_week_scatterplot'),
                         width = plot_size2$width(),
                         height = plot_size2$height())
        })
    })
    plot_size3 <- mod_plot_size_server("plot_size_3")
    observeEvent(c(plot_size3$width(),plot_size3$height()),{
        output$sa_distribution_plot <- renderGirafe({
            req(nrow(alldat_sa())>=1)
            p <- plot_obj$sa_distribution_plot(alldat_sa(),player_input())
            girafe(code = print(p),
                   width_svg = (1/96)*plot_size3$width(),
                   height_svg = (1/96)*plot_size3$height(),
                   options = list(
                       opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                       opts_hover_inv(css = "opacity:0.4")
                   ))
        })
        output$sa_distribution_plot_ui <- shiny::renderUI({
            girafeOutput(ns('sa_distribution_plot'),
                         width = plot_size3$width(),
                         height = plot_size3$height())
        })
    })
    plot_size4 <- mod_plot_size_server("plot_size_4")
    observeEvent(c(plot_size4$width(),plot_size4$height()),{
        output$sw_scatterplot <- renderGirafe({
            req(nrow(alldat_sw())>=1)
            p <- plot_obj$sw_scatterplot(alldat_sw(),player_input())
            girafe(code = print(p),
                   width_svg = (1/96)*plot_size4$width(),
                   height_svg = (1/96)*plot_size4$height(),
                   options = list(
                       opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                       opts_hover_inv(css = "opacity:0.4")
                   ))
        })
        output$sw_scatterplot_ui <- shiny::renderUI({
            girafeOutput(ns('sw_scatterplot'),
                         width = plot_size4$width(),
                         height = plot_size4$height())
        })
    })

  })
}

## To be copied in the UI
# mod_position_page_ui("position_page_1")

## To be copied in the server
# mod_position_page_server("position_page_1")
