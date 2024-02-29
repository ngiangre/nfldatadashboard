#' homepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib page_navbar bs_theme font_google nav_panel tooltip nav_spacer nav_item
mod_homepage_ui <- function(id){
  ns <- NS(id)
  link_github <- tags$a(
      shiny::icon("github"), "nfldatadashboard",
      href = "https://github.com/ngiangre/nfldatadashboard",
      target = "_blank"
  )

  bslib::page_navbar(
      theme = bslib::bs_theme(version = 5,base_font = bslib::font_google("Roboto"), font_scale = NULL,
                              `enable-gradients` = TRUE, preset = "zephyr"),
      underline = TRUE,
      sidebar = bslib::sidebar(
          shiny::selectizeInput(ns("analysis"),"Select Player Type",
                                choices = c(
                                    'Passers' = 'passing',
                                    'Receivers' = 'receiving',
                                    'Rushers' = 'rushing'
                                ),selected = 'passing',multiple = FALSE),
          shiny::selectizeInput(ns("position"),"Select Player Position(s)",
                                choices = NULL,multiple = TRUE),
          shiny::selectizeInput(ns("season"),"Season(s)",
                                choices = NULL,multiple = TRUE),
          shiny::selectizeInput(ns("week"),"Week(s)",
                                choices = NULL,multiple = TRUE)
      ),
      bslib::nav_panel(
          'Homepage',
          shiny::selectizeInput(ns('players'),label = "Select Players",
                                choices = NULL,multiple = TRUE,
                                width = "100%"),
          shiny::selectizeInput(ns('stats'),label = bslib::tooltip(
              trigger = list("Select Performance Indicator(s)",bsicons::bs_icon('info-circle')
              ),"Message",id = ns("stat_tooltip")),
              choices = NULL,multiple = TRUE,
              width = "100%"),
          mod_position_page_ui(ns("page_1"))
      ),
      bslib::nav_spacer(),
      bslib::nav_item(link_github),
      title = bslib::tooltip(list("nfldatadashboard",bsicons::bs_icon("info-circle")),
                             "Dashboard Goal: Visualize NFL player's performance.",
                             "1) Select players and performance indicators.",
                             "2) Compare player performance overall and across games.",
                             "See the left sidebar for performance indicators with descriptions.",
                             "(Future versions: More positions, multivariate statistics, predict fantasy points, construct fantasy teams.)"
                             )
  )
}

#' homepage Server Functions
#'
#' @noRd
mod_homepage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_obj <- data_object$new()
    session$userData[['analyses']] <-
        c('passing','receiving',"rushing")
    observeEvent(input$analysis,{
        req(input$analysis)
        positions <-
            data_obj$get_available_levels(input$analysis,
                                          'player_position')
        session$userData[['positions']] <- positions
        shiny::updateSelectizeInput(session = session,
                                    inputId = "position",
                                    choices = positions,
                                    selected = positions[1])
    })
    observeEvent(input$position,{
        req(input$analysis)
        seasons <-
            data_obj$get_available_levels(input$analysis,'season')
        session$userData[['seasons']] <- seasons
        shiny::updateSelectizeInput(session = session,
                                    inputId = "season",
                                    choices = seasons,
                                    selected = seasons[1])
        weeks <-
            setdiff(data_obj$get_available_levels(input$analysis,
                                                  'week'),0L)
        session$userData[['weeks']] <- weeks
        shiny::updateSelectizeInput(session = session,
                                    inputId = "week",
                                    choices = weeks,
                                    selected = weeks)
    })
    sa_analysis_dataset <- eventReactive(c(input$analysis,input$position,input$season),{
        req(input$analysis,input$position,input$season)
        data_obj$get_analysis_data(analysis = input$analysis,
                                   positions = input$position,
                                   seasons = input$season,
                                   weeks = 0L)
    })
    sw_analysis_dataset <- eventReactive(c(input$analysis,input$position,input$season),{
        req(input$analysis,input$position,input$season)
        data_obj$get_analysis_data(analysis = input$analysis,
                                   positions = input$position,
                                   seasons = input$season,
                                   weeks = session$userData[['weeks']])
    })
    observeEvent(c(sa_analysis_dataset(),sw_analysis_dataset()),{
        plyrNames <- paste0(
            sw_analysis_dataset()$player_display_name,
            " (",sw_analysis_dataset()$player_position,
            " ; ",sw_analysis_dataset()$team_abbr,")")
        plyrValues <- sw_analysis_dataset()$player_display_name
        names(plyrValues) <- plyrNames
        shiny::updateSelectizeInput(session = session,
                                    inputId = 'players',
                                    choices = plyrValues,
                                    selected = plyrValues[1])
        statValues <-
            intersect(colnames(sw_analysis_dataset()),
                      nflreadr::dictionary_nextgen_stats$field[-c(1:10,29)])
        stats <- statValues
        names(stats) <- paste0("<b>",statValues,"</b> <i>",
                               nflreadr::dictionary_nextgen_stats$description[
                                   match(statValues,nflreadr::dictionary_nextgen_stats$field)],
                               "</i>")
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
        session$userData[['statistics']] <- stats
        shiny::updateSelectizeInput(session = session,
                                    inputId = 'stats',
                                    choices = stats,
                                    selected = stats[1],
                                    options = list(render = renderSelectizeUI))
    })
    sa_plot_alldata <- reactive({
        req(input$stats)
        tmp <-
            sa_analysis_dataset() |>
            select(any_of(c('season','season_type','player_display_name',
                            'week','player_position','team_abbr',input$stats))) |>
            mutate(
                ID = 1:(dplyr::n())
            ) |>
            tidyr::pivot_longer(
                cols = input$stats,
                names_to = "statistic"
            ) |>
            mutate(
                statistic = factor(statistic,levels=session$userData[['statistics']]),
                season = factor(season,levels=session$userData[['seasons']]),
                week = factor(week,levels=c(0L))
            )

        tmp
    }) |>
        bindCache(input$stats,sa_analysis_dataset()) |>
        bindEvent(input$stats,sa_analysis_dataset())
    sw_plot_alldata <- reactive({
        req(input$stats)
        tmp <-
            sw_analysis_dataset() |>
            select(any_of(c('season','season_type','player_display_name',
                            'week','player_position','team_abbr',input$stats))) |>
            mutate(
                ID = 1:(dplyr::n())
            ) |>
            tidyr::pivot_longer(
                cols = input$stats,
                names_to = "statistic"
            ) |>
            mutate(
                statistic = factor(statistic,levels=session$userData[['statistics']]),
                season = factor(season,levels=session$userData[['seasons']]),
                week = factor(week,levels=session$userData[['weeks']])
            )

        tmp |>
            dplyr::semi_join(
                tmp |>
                    summarise(N=dplyr::n_distinct(week),
                              .by = c('statistic','player_display_name',"season")) |>
                    filter(N>1)
            )
    }) |>
        bindCache(input$stats,sw_analysis_dataset()) |>
        bindEvent(input$stats,sw_analysis_dataset())
    sa_plot_data <- reactive({
        sa_plot_alldata() |>
            filter(.data[['season']] %in% input$season) |>
            filter(.data[['player_display_name']] %in% input$players)
    }) |>
        bindCache(input$players,
                  input$season,
                  sa_plot_alldata()) |>
        bindEvent(input$players,
                  input$season,
                  sa_plot_alldata())
    sw_plot_data <- reactive({
        sw_plot_alldata() |>
            filter(.data[['season']] %in% input$season) |>
            filter(.data[['week']] %in% input$week) |>
            filter(.data[['player_display_name']] %in% input$players)
    }) |>
        bindCache(input$players,
                  input$season,input$week,
                  sw_plot_alldata()) |>
        bindEvent(input$players,
                  input$season,input$week,
                  sw_plot_alldata())
    mod_position_page_server("page_1",data_obj,ptype="qb",
                             sa_plot_data,sw_plot_data,
                             sa_plot_alldata,sw_plot_alldata)
    observeEvent(input$stat_tooltip,{
        bslib::update_tooltip('stat_tooltip',
                              stringr::str_glue("Select atleast one performance indicator calculated by Next Gen Stats."),
                              "The description of the performance indicator is italicized.")
    })
  })
}

## To be copied in the UI
# mod_homepage_ui("homepage_1")

## To be copied in the server
# mod_homepage_server("homepage_1")
