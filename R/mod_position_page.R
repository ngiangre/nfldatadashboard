#' position_page UI Function
#'
#' @description A shiny Module to display plots for the selected position.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput plotOuput
#' @importFrom bslib tooltip sidebar layout_sidebar accordion accordion_panel
#' @importFrom bsicons bs_icon
mod_position_page_ui <- function(id){
  ns <- NS(id)
  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
          width = 400,
          selectizeInput(ns('player'),label = bslib::tooltip(
              trigger = list("Select Player(s)",bsicons::bs_icon('info-circle')
              ),"Message",id = ns("player_tooltip")),choices=NULL,multiple = TRUE),
          selectizeInput(ns('stat'),label = bslib::tooltip(
              trigger = list("Select Performance Indicator(s)",bsicons::bs_icon('info-circle')
              ),"Message",id = ns("stat_tooltip")),choices=NULL,multiple = TRUE)
      ),
      bslib::navset_card_tab(
          bslib::nav_panel(
              bslib::tooltip(list("Side-by-Side Player Comparisons",bsicons::bs_icon('info-circle')),
                             "Two plots displaying the average performance of the selected players."
              ),
              bslib::accordion(
                  bslib::accordion_panel(
                      "Side-By-Side Overall Performance",
                      plotOutput(ns('sa_heatmap'))
                  ),
                  bslib::accordion_panel(
                      "Week-By-Week Season Performance",
                      plotOutput(ns('sw_boxplots'),height = "1000px")
                  )
              )
          ),
          bslib::nav_panel(
              bslib::tooltip(list("Comparison With All Players",bsicons::bs_icon('info-circle')),
                             "Two plots displaying all players in this position (gray dots).",
                             "Selected players are labelled for comparison."
                             ),
              bslib::accordion(
                  bslib::accordion_panel(
                      "Overall Performance",
                      plotOutput(ns('sa_distribution_plot'),height = "1000px")
                  ),
                  bslib::accordion_panel(
                      "Overall Performance vs. Season Variability",
                      plotOutput(ns('sw_scatterplot'),height = "1000px")
                  )
              )
          ),
          id = ns('plots')
      )
  )
}

#' position_page Server Functions
#'
#' @noRd
#' @importFrom shiny reactive req renderPlot I updateSelectizeInput observeEvent
#' @importFrom bslib update_tooltip
#' @importFrom stringr str_glue
#' @importFrom dplyr select all_of filter
#' @importFrom tidyr pivot_longer
mod_position_page_server <- function(id,data_obj,ptype = c('qb','wr','rb')){
  ptype <- match.arg(ptype)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    alldat_sw <- reactive({
        req(input$stat)
        data_obj$get_position_season_data(ptype,'sw') |>
            dplyr::select(
                dplyr::all_of(c("player_display_name","team_abbr",'week',
                                input$stat))
            ) |>
            tidyr::pivot_longer(
                cols = input$stat,
                names_to = "statistic"
            )
    })
    subdat_sw <- reactive({alldat_sw() |> filter(player_display_name %in% input$player)})
    alldat_sa <- reactive({
        req(input$stat)
        data_obj$get_position_season_data(ptype,'sa') |>
            dplyr::select(
                dplyr::all_of(c("player_display_name","team_abbr",
                                input$stat))
            ) |>
            tidyr::pivot_longer(
                cols = input$stat,
                names_to = "statistic"
            )
    })
    subdat_sa <- reactive({alldat_sa() |> filter(player_display_name %in% input$player)})

    plot_obj = plot_object$new()
    output$sa_heatmap <- renderPlot({
        req(nrow(subdat_sa())>=1)
        plot_obj$sa_heatmap(subdat_sa())
    })
    output$sw_boxplots <- renderPlot({
        req(nrow(subdat_sw())>=1)
        plot_obj$sw_boxplots(subdat_sw())
    })
    output$sa_distribution_plot <- renderPlot({
        req(nrow(subdat_sa())>=1)
        plot_obj$sa_distribution_plot(alldat_sa(),subdat_sa())
    })
    output$sw_scatterplot <- renderPlot({
        req(nrow(subdat_sw())>=1)
        plot_obj$sw_scatterplot(alldat_sw(),subdat_sw())
    })
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
                         options = list(render = renderSelectizeUI))
    observeEvent(input$player_tooltip,{
        bslib::update_tooltip('player_tooltip',
                              stringr::str_glue("Select atleast one {stringr::str_to_upper(ptype)}. "),
                              "Team abbreviation is in parantheses")
    })
    updateSelectizeInput(session = session,inputId = 'stat',
                         choices = data_obj$get_named_position_vars(ptype),
                         selected = data_obj$get_named_position_vars(ptype),
                         options = list(render = renderSelectizeUI)
                         )
    observeEvent(input$stat_tooltip,{
        bslib::update_tooltip('stat_tooltip',
                              stringr::str_glue("Select atleast one {stringr::str_to_upper(ptype)} Performance indicator calculated by Next Gen Stats. "),
                              "The description of the performance indicator is italicized.")
    })

  })
}

## To be copied in the UI
# mod_position_page_ui("position_page_1")

## To be copied in the server
# mod_position_page_server("position_page_1")
