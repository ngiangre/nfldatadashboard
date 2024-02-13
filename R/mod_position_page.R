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
                  plotOutput(ns('sa_heatmap'))
              ),
              bslib::accordion_panel(
                  "Week-By-Week Season Performance",
                  girafeOutput(ns('sw_boxplots'),
                               width = 800,height = 800)
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
                  girafeOutput(ns('sa_distribution_plot'),
                               width = 800,height = 800)
              ),
              bslib::accordion_panel(
                  "Overall Performance vs. Season Variability",
                  girafeOutput(ns('sw_scatterplot'),
                               width = 800,height = 800)
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
mod_position_page_server <- function(id,data_obj,ptype,subdat_sa,subdat_sw){
  stopifnot(ptype %in% data_obj$get_available_positions())
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    plot_obj = plot_object$new()
    output$sa_heatmap <- renderPlot({
        req(nrow(subdat_sa())>=1)
        plot_obj$sa_heatmap(subdat_sa())
    })
    output$sw_boxplots <- renderGirafe({
        req(nrow(subdat_sw())>=1)
        p <- plot_obj$sw_week_scatterplot(subdat_sw())
        girafe(code = print(p),
               width_svg = 15, height_svg = 15,
               options = list(
                   opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                   opts_hover_inv(css = "opacity:0.1")
               ))
    })
    output$sa_distribution_plot <- renderGirafe({
        req(nrow(alldat_sa())>=1)
        p <- plot_obj$sa_distribution_plot(alldat_sa(),input$player_2)
        girafe(code = print(p),
               width_svg = 15, height_svg = 15,
               options = list(
                   opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                   opts_hover_inv(css = "opacity:0.4")
                   ))
    })
    output$sw_scatterplot <- renderGirafe({
        req(nrow(alldat_sw())>=1)
        p <- plot_obj$sw_scatterplot(alldat_sw(),input$player_2)
        girafe(code = print(p),
               width_svg = 15, height_svg = 15,
               options = list(
                   opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                   opts_hover_inv(css = "opacity:0.4")
               ))
    })

  })
}

## To be copied in the UI
# mod_position_page_ui("position_page_1")

## To be copied in the server
# mod_position_page_server("position_page_1")
