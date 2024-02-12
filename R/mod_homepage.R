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
      shiny::selectizeInput(ns('players'),label = "Select Players",
                            choices = NULL,multiple = TRUE,
                            width = "100%"),
      bslib::nav_panel(title="Quarterbacks",mod_position_page_ui(ns("qb_page_1"))),
      bslib::nav_panel(title="Wide Receivers",mod_position_page_ui(ns("wr_page_1"))),
      bslib::nav_panel(title="Tight Ends",mod_position_page_ui(ns("te_page_1"))),
      bslib::nav_panel(title="Running Backs",mod_position_page_ui(ns("rb_page_1"))),
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
    observeEvent(input$analysis,{
        shiny::updateSelectizeInput(session = session,
                                    inputId = "position",
                                    choices = data_obj$get_available_levels(input$analysis,
                                                                            'player_position'),
                                    selected = data_obj$get_available_levels(input$analysis,
                                                                            'player_position')[1])
    })
    observeEvent(input$position,{
        req(input$analysis)
        shiny::updateSelectizeInput(session = session,
                                    inputId = "season",
                                    choices = data_obj$get_available_levels(input$analysis,'season'),
                                    selected = data_obj$get_available_levels(input$analysis,'season')[1])
        shiny::updateSelectizeInput(session = session,
                                    inputId = "week",
                                    choices = setdiff(data_obj$get_available_levels(input$analysis,
                                                                                    'week'),0L),
                                    selected = setdiff(data_obj$get_available_levels(input$analysis,
                                                                                     'week'),0L))
    })
    analysis_dataset <- eventReactive(c(input$season,input$week,input$position),{
        req(input$analysis,input$position,input$season,input$week)
        data_obj$get_analysis_data(analysis = input$analysis,
                                   positions = input$position,
                                   seasons = input$season,
                                   weeks = input$week)
    })
    observeEvent(analysis_dataset(),{
        plyrs <- paste0(
            analysis_dataset()$player_display_name,
            " (",analysis_dataset()$player_position,
            " ; ",analysis_dataset()$team_abbr,")")
        shiny::updateSelectizeInput(session = session,
                                    inputId = 'players',
                                    choices = plyrs,
                                    selected = plyrs[1])
    })
    mod_position_page_server("qb_page_1",data_obj,ptype="qb",analysis_datasest)
    mod_position_page_server("wr_page_1",data_obj,ptype="wr",analysis_datasest)
    mod_position_page_server("te_page_1",data_obj,ptype="te",analysis_datasest)
    mod_position_page_server("rb_page_1",data_obj,ptype="rb",analysis_datasest)
  })
}

## To be copied in the UI
# mod_homepage_ui("homepage_1")

## To be copied in the server
# mod_homepage_server("homepage_1")
