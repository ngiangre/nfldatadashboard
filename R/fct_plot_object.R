#' plot_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @importFrom ggiraph geom_point_interactive geom_smooth_interactive
#' @importFrom dplyr mutate summarise arrange desc
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_replace str_remove
#' @noRd
#' @examples
#' plotr6 <- plot_object$new()
#' datar6 <- data_object$new()
#'
#' indicators <- datar6$get_position_vars('qb')[1:3]
#' dat_sw <-
#' datar6$get_position_season_data('qb','sw') |>
#'     dplyr::select(
#'         dplyr::all_of(c("player_display_name","team_abbr",'week',
#'                         indicators))
#'     ) |>
#'     tidyr::pivot_longer(
#'         cols = dplyr::all_of(indicators),
#'         names_to = "statistic"
#'     ) |>
#'     dplyr::filter(player_display_name %in% c( datar6$get_position_players('qb')[1:3] ))
#'
#'
#' plotr6$sw_boxplots(dat_sw)
#' plotr6$sw_week_scatterplot(dat_sw)
#'
#' dat_sa <-
#' datar6$get_position_season_data('qb','sa') |>
#'     dplyr::select(
#'         dplyr::all_of(c("player_display_name","team_abbr",
#'                         indicators))
#'     ) |>
#'     tidyr::pivot_longer(
#'         cols = dplyr::all_of(indicators),
#'         names_to = "statistic"
#'     ) |>
#'     dplyr::filter(player_display_name %in% c( datar6$get_position_players('qb')[1:3] ))
#'
#' plotr6$sa_heatmap(dat_sa)
plot_object <- R6::R6Class("PlotObject",
                           public = list(
                               global_text_size = 16,
                               global_theme = theme_classic(base_size = 16),
                               pos_lil_jitter = position_jitter(width = 0.1,height=0.1,seed = 0),
                               pos_big_jitter = position_jitter(width = 0.4,height=0.4,seed = 0),
                               wrap_len = 25,
                               generate_discrete_colors = function(n){
                                   stopifnot(is.numeric(n))
                                   n <- floor(n)
                                   stopifnot(n>0)
                                   pals <- palette.pals()
                                   names(pals) <- pals
                                   color_library <-
                                       purrr::map(pals,~{palette.colors(palette = .x)})
                                   color_library[['R3']] <- NULL
                                   color_library <-
                                       color_library[c(
                                           'Okabe-Ito','Alphabet',"Polychrome 36",'R4',
                                           "Tableau 10",'Dark2','Set1','Set2','Set3'
                                       )]
                                   color_library <-
                                       color_library |>
                                       unlist() |>
                                       unname()
                                   color_library[seq(1,n,1)]
                               },
                               sa_heatmap = function(dat){
                                   dat |>
                                       mutate(statistic = factor(statistic)) |>
                                       mutate(
                                           zvalue = (value - mean(value))/(sd(value)),
                                           .by = statistic
                                       ) |>
                                       arrange(desc(statistic),team_abbr) |>
                                       mutate(
                                           player_label = paste0(player_display_name,"\n(",team_abbr,")") |>
                                               factor()
                                       ) |>
                                       ggplot(aes(forcats::fct_inorder(player_label),
                                                  forcats::fct_inorder(statistic),fill=zvalue)) +
                                       geom_tile() +
                                       scale_fill_gradient2(
                                           low = "dodgerblue",
                                           mid = "white",
                                           high = "indianred"
                                       ) +
                                       geom_label(aes(label = round(value,1)),
                                                 color='black',fill='white',fontface='bold') +
                                       scale_x_discrete(position = "top",labels = label_wrap_gen(width=5)) +
                                       labs(x=NULL,y=NULL,caption="'Bluer' colors indicate lower performance compared to other players, 'redder' colors indicate higher performance compared to other players") +
                                       self$global_theme +
                                       theme(
                                           legend.position = "none",
                                           axis.text.x = element_text(size=self$global_text_size,face = "bold"),
                                           axis.text.y = element_text(size=self$global_text_size,face = "bold")
                                       )
                               },
                               sw_boxplots = function(dat){
                                   dat |>
                                       mutate(
                                           week_tooltip = stringr::str_glue("
                                           Player: {player_display_name}
                                           Week: {week}
                                           {statistic}: {round(value,2)}
                                                                            ")
                                       ) |>
                                       ggplot(aes(value,player_display_name,
                                                  group=player_display_name)) +
                                       geom_boxplot(size=1,fill="gray50",color="gray50",
                                                    alpha=0,outlier.shape = NA) +
                                       geom_point_interactive(
                                           aes(data_id = week,
                                               tooltip = week_tooltip,
                                               fill = week),
                                           position = self$pos_lil_jitter,size = 5,
                                           color="black",shape=21,show.legend = FALSE) +
                                       scale_fill_distiller(palette = "BuPu",type = "div",direction = 1) +
                                       facet_wrap(~stringr::str_replace_all(statistic,"_"," "),
                                                  scales = "free_x",ncol=2,
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       labs(y=NULL,caption="Each dot is a game during the 2023 season\n'Low' colors for games early in the season, and 'high' colors for later in the season") +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           axis.text.y = element_text(size=self$global_text_size,face = "bold")
                                       )
                               },
                               sw_week_scatterplot = function(dat){
                                   dat |>
                                       mutate(
                                           week_tooltip = stringr::str_glue("
                                           Player: {player_display_name}
                                           Week: {week}
                                           {statistic}: {round(value,2)}
                                                                            "),
                                           smooth_tooltip = stringr::str_glue("
                                           Performance Trend Across Season
                                           Player: {player_display_name}
                                                                              ")
                                       ) |>
                                       dplyr::arrange(player_display_name,week) |>
                                       ggplot(aes(week,value,group=player_display_name)) +
                                       geom_smooth_interactive(method='lm',formula='y ~ x',se = FALSE,
                                                   mapping=aes(color=player_display_name,
                                                               tooltip=smooth_tooltip,
                                                               data_id=player_display_name),
                                                   alpha = 0.5,linewidth=2,
                                                   show.legend = FALSE) +
                                       geom_path(aes(color=player_display_name),
                                                 linewidth=.5,show.legend = FALSE) +
                                       geom_point_interactive(aes(
                                           data_id = interaction(player_display_name,week),
                                           tooltip = week_tooltip,
                                           shape=player_display_name,
                                           color=player_display_name),
                                           size = 4) +
                                       scale_x_continuous(breaks = scales::breaks_width(1)) +
                                       scale_color_manual(values = self$generate_discrete_colors(
                                          dplyr::n_distinct(dat$player_display_name)
                                       )) +
                                       guides(shape=guide_legend(title = NULL,override.aes = list(size=5)),
                                              color=guide_legend(title = NULL)) +
                                       facet_wrap(~stringr::str_replace_all(statistic,"_"," "),
                                                  scales = 'free',ncol=3,
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       labs(caption="Each dot is a game during the 2023 season for the selected players.") +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           legend.text = element_text(size = self$global_text_size),
                                           legend.position = "top"
                                       )
                               },
                               sa_distribution_plot = function(dat){
                                   p <-
                                       dat |>
                                       mutate(
                                           player_tooltip = stringr::str_glue("
                                           Player: {player_display_name}
                                           Team: {team_abbr}
                                           {statistic}: {round(value,2)}
                                                                            "),
                                           player_display_name = stringr::str_remove(player_display_name,"\\'")
                                       ) |>
                                       ggplot(aes(value,factor(1))) +
                                       geom_boxplot(color="gray40",size = 1,
                                                    outlier.shape = NA,alpha = 0) +
                                       geom_point_interactive(aes(data_id = player_display_name,
                                                                  tooltip = player_tooltip),
                                                              color = 'black',fill = "gray70",shape=21,
                                                              size = 5,position = self$pos_big_jitter,
                                                              hover_nearest = TRUE) +
                                       facet_wrap(~stringr::str_replace_all(statistic,"_"," "),
                                                  scales = "free_x",ncol=3,
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       labs(y=NULL) +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           axis.text.y = element_blank(),
                                           axis.ticks.y = element_blank()
                                       )
                                   p
                               },
                               sw_scatterplot = function(dat){
                                   dat |>
                                       mutate(
                                           player_display_name = stringr::str_remove(player_display_name,"\\'")
                                       ) |>
                                       summarise(
                                           var = sd(value) |> round(digits=2),
                                           avg = mean(value) |> round(digits=2),
                                           .by = c(player_display_name,statistic,team_abbr)
                                       ) |>
                                       mutate(
                                           player_tooltip = stringr::str_glue("
                                           Player: {player_display_name}
                                           Team: {team_abbr}
                                           {statistic}
                                           Overall: {avg}
                                           Variation: {var}
                                                                            ")
                                       ) |>
                                       ggplot(aes(var,avg)) +
                                       geom_smooth(method='lm',se=FALSE,color='red',linetype='dashed',
                                                   formula = 'y ~ x') +
                                       geom_point_interactive(aes(data_id=player_display_name,
                                                                  tooltip=player_tooltip),
                                                              color='black',fill='gray70',shape=21,
                                                              hover_nearest = TRUE,size = 5) +
                                       facet_wrap(~stringr::str_replace_all(statistic,"_"," "),
                                                  scales="free",ncol=3,
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       scale_x_continuous(limits = c(0,NA)) +
                                       labs(x='Variability in Performance',y="Average Performance",
                                            caption = 'Red dotted line indicates above or below average performance over the season') +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size)
                                       )
                               }
                           )

)
