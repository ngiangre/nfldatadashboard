#' plot_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @importFrom ggiraph geom_point_interactive geom_smooth_interactive
#' @importFrom dplyr mutate summarise arrange desc if_else
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
#'         dplyr::all_of(c("player_unique_id","team_abbr",'week',
#'                         indicators))
#'     ) |>
#'     tidyr::pivot_longer(
#'         cols = dplyr::all_of(indicators),
#'         names_to = "statistic"
#'     ) |>
#'     dplyr::filter(player_unique_id %in% c( datar6$get_position_players('qb')[1:3] ))
#'
#'
#' plotr6$sw_boxplots(dat_sw)
#' plotr6$sw_week_scatterplot(dat_sw)
#'
#' dat_sa <-
#' datar6$get_position_season_data('qb','sa') |>
#'     dplyr::select(
#'         dplyr::all_of(c("player_unique_id","team_abbr",
#'                         indicators))
#'     ) |>
#'     tidyr::pivot_longer(
#'         cols = dplyr::all_of(indicators),
#'         names_to = "statistic"
#'     ) |>
#'     dplyr::filter(player_unique_id %in% c( datar6$get_position_players('qb')[1:3] ))
#'
#' plotr6$sa_heatmap(dat_sa)
plot_object <- R6::R6Class("PlotObject",
                           public = list(
                               global_text_size = 20,
                               get_base_theme = function(){theme_classic(base_size = self$global_text_size)},
                               pos_lil_jitter = position_jitter(width = 0.1,height=0.4,seed = 0),
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
                                           'Set2','Alphabet',"Tableau 10",'R4','Set3',
                                           'Okabe-Ito','Dark2',"Polychrome 36",'Set1','Dark2'
                                       )]
                                   color_library <-
                                       color_library |>
                                       unlist() |>
                                       unname()
                                   color_library[seq(1,n,1)]
                               },
                               sa_heatmap = function(dat){
                                   dat |>
                                       mutate(
                                           zvalue = (value - mean(value))/(sd(value)),
                                           .by = c(statistic)
                                       ) |>
                                       arrange(desc(statistic),team_abbr) |>
                                       mutate(
                                           player_label = player_unique_id |>
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
                                       labs(x=NULL,y=NULL,caption="'Bluer' colors indicate lower performance compared to other players\n'Redder' colors indicate higher performance compared to other players") +
                                       self$get_base_theme() +
                                       facet_wrap(~season,nrow=1) +
                                       theme(
                                           legend.position = "none",
                                           axis.text.x = element_text(size=self$global_text_size,face = "bold"),
                                           axis.text.y = element_text(size=self$global_text_size,face = "bold")
                                       )
                               },
                               sw_week_scatterplot = function(dat){
                                   sub <-
                                       dat |>
                                       mutate(
                                           week = as.integer(week),
                                       ) |>
                                       dplyr::left_join(
                                           dat |>
                                               mutate(
                                                   week = as.integer(week),
                                               ) |>
                                               mutate(nvalue = ((value - min(value))/(max(value)-min(value))),
                                                      .by=c(statistic,player_unique_id)) |>
                                               dplyr::summarise(mod = list(lm(value ~ week)),
                                                                .by=c(statistic,
                                                                      player_unique_id,
                                                                      season)
                                               ) |>
                                               dplyr::mutate(
                                                   coef = purrr::map_dbl(mod,~{coefficients(.x)['week']}),
                                                   chg = purrr::map_dbl(mod,~{exp(coefficients(.x)['week'])-1}),
                                                   perc_chg = scales::percent(round(chg,3))
                                               ),
                                           by = c('statistic','player_unique_id',"season")
                                       ) |>
                                       mutate(
                                           week_tooltip = stringr::str_glue("
                                           Season: {season}
                                           Player: {player_unique_id}
                                           Week: {week}
                                           {statistic}: {round(value,2)}
                                                                            "),
                                           smooth_tooltip = stringr::str_glue("
                                           Season: {season}
                                           Player: {player_unique_id}
                                           {round(coef,2)} {ifelse(coef>0,'more','less')} {statistic}
                                                                              "),
                                           smooth_perc_tooltip = stringr::str_glue("
                                           Season: {season}
                                           Player: {player_unique_id}
                                           {perc_chg} {ifelse(chg>0,'increased','decreased')} {statistic}
                                                                              ")
                                       ) |>
                                       dplyr::arrange(player_unique_id,week)

                                   sub |>
                                       ggplot(aes(week,
                                           value,group=player_unique_id)) +
                                       geom_smooth_interactive(method='lm',formula='y ~ x',se = FALSE,
                                                   mapping=aes(color=player_unique_id,
                                                               tooltip=smooth_perc_tooltip,
                                                               data_id=player_unique_id),
                                                   alpha = 0.5,linewidth=2,
                                                   show.legend = FALSE) +
                                       geom_path(aes(color=player_unique_id),
                                                 linewidth=.5,show.legend = FALSE) +
                                       geom_point_interactive(aes(
                                           data_id = interaction(player_unique_id,week),
                                           tooltip = week_tooltip,
                                           shape=player_unique_id,
                                           color=player_unique_id),
                                           size = 4) +
                                       scale_x_continuous(breaks = as.integer(levels(dat$week))) +
                                       scale_color_manual(values = self$generate_discrete_colors(
                                          dplyr::n_distinct(dat$player_unique_id)
                                       )) +
                                       guides(shape=guide_legend(title = NULL,override.aes = list(size=5)),
                                              color=guide_legend(title = NULL)) +
                                       facet_grid(stringr::str_replace_all(statistic,"_"," ")~season,
                                                  scales = 'free_y',
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       labs(x = "Week",
                                            caption="Each dot is a game during the season for the selected players.") +
                                       self$get_base_theme() +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           legend.text = element_text(size = self$global_text_size),
                                           legend.position = "top"
                                       )
                               },
                               sa_distribution_plot = function(dat,players){
                                   if(length(players)==0){
                                       fill_values <- "gray40"
                                       fill_limits <- ""
                                       fill_labels <- "All players"
                                   }else{
                                       fill_values <- c("gray40",self$generate_discrete_colors(length(players)))
                                       fill_limits <- c("All players",players)
                                       fill_labels <- c("All players",players)
                                   }
                                   dat_labelled <-
                                       dat |>
                                       mutate(
                                           player_tooltip = stringr::str_glue("
                                           Player: {player_unique_id}
                                           Team: {team_abbr}
                                           {statistic}: {round(value,2)}
                                                                            "),
                                           player_unique_id = stringr::str_remove(player_unique_id,"\\'"),
                                           point_size = dplyr::if_else(player_unique_id %in% players,10,5)
                                       ) |>
                                       arrange(point_size)
                                   p <-
                                       dat_labelled |>
                                       ggplot(aes(value,factor(1))) +
                                       geom_boxplot(color="gray40",size = 1,
                                                    outlier.shape = NA,alpha = 0) +
                                       geom_point_interactive(aes(data_id = player_unique_id,
                                                                  tooltip = player_tooltip,
                                                                  fill = player_unique_id,
                                                                  size = point_size),
                                                              color = 'black',
                                                              alpha = .9,
                                                              shape=21,
                                                              position = self$pos_lil_jitter,
                                                              hover_nearest = TRUE) +
                                       scale_fill_manual(values = fill_values,
                                                         limits = fill_limits,
                                                         labels = fill_labels) +
                                       scale_size_identity() +
                                       guides(fill = guide_legend(title=NULL,nrow=1,
                                                                  override.aes = list(size = 5))) +
                                       facet_grid(stringr::str_replace_all(statistic,"_"," ")~season,
                                                  scales = "free_x",
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       labs(y=NULL) +
                                       self$get_base_theme() +
                                       theme(
                                           legend.position = "top",
                                           legend.text = element_text(size = self$global_text_size),
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           axis.text.y = element_blank(),
                                           axis.ticks.y = element_blank()
                                       )
                                   p
                               },
                               sw_scatterplot = function(dat,players){
                                   if(length(players)==0){
                                       fill_values <- "gray40"
                                       fill_limits <- ""
                                       fill_labels <- "All players"
                                   }else{
                                       fill_values <- c("gray40",self$generate_discrete_colors(length(players)))
                                       fill_limits <- c("All players",players)
                                       fill_labels <- c("All players",players)
                                   }
                                   dat_labelled <-
                                       dat |>
                                       mutate(
                                           player_unique_id = stringr::str_remove(player_unique_id,"\\'")
                                       ) |>
                                       summarise(
                                           var = sd(value) |> round(digits=2),
                                           avg = mean(value) |> round(digits=2),
                                           .by = c(player_unique_id,statistic,team_abbr,season)
                                       ) |>
                                       mutate(
                                           player_tooltip = stringr::str_glue("
                                           Season: {season}
                                           Player: {player_unique_id}
                                           Team: {team_abbr}
                                           {statistic}
                                           Overall: {avg}
                                           Variation: {var}
                                                                            "),
                                           point_size = dplyr::if_else(player_unique_id %in% players,10,5)
                                       ) |>
                                       arrange(point_size)
                                   dat_labelled |>
                                       ggplot(aes(var,avg)) +
                                       geom_smooth(method='lm',se=FALSE,color='red',linetype='dashed',
                                                   formula = 'y ~ x') +
                                       geom_point_interactive(aes(data_id=player_unique_id,
                                                                  tooltip=player_tooltip,
                                                                  fill=player_unique_id,
                                                                  size = point_size),
                                                              color='black',alpha = 0.9,shape=21,
                                                              hover_nearest = TRUE) +
                                       scale_fill_manual(values = fill_values,
                                                         limits = fill_limits,
                                                         labels = fill_labels) +
                                       scale_size_identity() +
                                       guides(fill = guide_legend(title=NULL,nrow=1,
                                                                  override.aes = list(size = 5))) +
                                       facet_grid(stringr::str_replace_all(statistic,"_"," ")~season,
                                                  scales = "free",
                                                  labeller = label_wrap_gen(width=self$wrap_len,
                                                                            multi_line = TRUE)) +
                                       scale_x_continuous(limits = c(0,NA)) +
                                       labs(x='Variability in Performance',y="Average Performance",
                                            caption = 'Red dotted line indicates above or below average performance over the season') +
                                       self$get_base_theme() +
                                       theme(
                                           legend.position = 'top',
                                           legend.text = element_text(size = self$global_text_size),
                                           strip.text = element_text(face='bold',size = self$global_text_size)
                                       )
                               }
                           )

)
