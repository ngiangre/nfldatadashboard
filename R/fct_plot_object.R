#' plot_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @importFrom dplyr mutate summarise arrange desc
#' @importFrom forcats fct_inorder
#' @importFrom ggrepel geom_label_repel geom_text_repel
#' @importFrom scico scale_fill_scico
#' @noRd
plot_object <- R6::R6Class("PlotObject",
                           public = list(
                               global_text_size = 16,
                               global_theme = theme_classic(base_size = 16),
                               pos = position_jitter(width = 0.05,height=0.05,seed = 0),
                               global_text_repel = function(sub,pos){
                                   geom_label_repel(data = sub,size=5,color='black',fontface='bold',
                                                   mapping=aes(label=player_display_name),
                                                   force=80,min.segment.length = 0,position = pos)
                               },
                               sa_heatmap = function(dat){
                                   dat |>
                                       mutate(statistic = factor(statistic)) |>
                                       arrange(desc(statistic)) |>
                                       mutate(zvalue = (value - mean(value))/sd(value),.by = statistic) |>
                                       ggplot(aes(player_display_name,
                                                  forcats::fct_inorder(statistic),fill=zvalue)) +
                                       geom_tile() +
                                       scico::scale_fill_scico(palette = "berlin") +
                                       geom_label(aes(label = round(value,1)),
                                                 color='black',fill='white',fontface='bold') +
                                       scale_x_discrete(position = "top",labels = label_wrap_gen(width=5)) +
                                       labs(x=NULL,y=NULL) +
                                       self$global_theme +
                                       theme(
                                           legend.position = "none",
                                           axis.text.x = element_text(size=self$global_text_size,face = "bold"),
                                           axis.text.y = element_text(size=self$global_text_size,face = "bold")
                                       )
                               },
                               sw_boxplots = function(dat){
                                   dat |>
                                       ggplot(aes(value,player_display_name,
                                                  group=player_display_name)) +
                                       geom_boxplot(size=1,fill="gray50",color="gray50",
                                                    alpha=0,outlier.shape = NA) +
                                       geom_jitter(width = 0.05,height=0.05,
                                                   show.legend = FALSE,color="black",fill='gray70',shape=21) +
                                       facet_wrap(~statistic,scales = "free_x",ncol=3,
                                                  labeller = label_wrap_gen(width=15,multi_line = TRUE)) +
                                       labs(y=NULL) +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           axis.text.y = element_text(size=self$global_text_size,face = "bold")
                                       )
                               },
                               sa_distribution_plot = function(dat,sub){
                                   dat |>
                                       ggplot(aes(value,factor(1))) +
                                       geom_boxplot(color="gray40",size = 0.5,
                                                    outlier.shape = NA,alpha = 0, width = 0.25) +
                                       geom_point(color = 'black',fill = "gray70",shape=21,
                                                  position = self$pos) +
                                       self$global_text_repel(sub,self$pos) +
                                       geom_point(data = sub, size = 2,color="black",fill='black',
                                                  shape=21,position = self$pos) +
                                       facet_wrap(~statistic,scales = "free_x",ncol=3) +
                                       labs(y=NULL) +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size),
                                           axis.text.y = element_blank(),
                                           axis.ticks.y = element_blank()
                                       )
                               },
                               sw_scatterplot = function(dat,sub){
                                   dat |>
                                       summarise(
                                           var = sd(value),
                                           avg = mean(value),
                                           .by = c(player_display_name,statistic)
                                       ) |>
                                       ggplot(aes(var,avg)) +
                                       geom_point(color='black',fill='gray70',shape=21) +
                                       geom_point(data=sub |> summarise(
                                           var = sd(value),
                                           avg = mean(value),
                                           .by = c(player_display_name,statistic)
                                       )) +
                                       self$global_text_repel(sub |> summarise(
                                           var = sd(value),
                                           avg = mean(value),
                                           .by = c(player_display_name,statistic)
                                       ),position_identity()) +
                                       facet_wrap(~statistic,scales="free",ncol=3) +
                                       scale_x_continuous(limits = c(0,NA)) +
                                       labs(x='Variability in Performance',y="Average Performance") +
                                       self$global_theme +
                                       theme(
                                           strip.text = element_text(face='bold',size = self$global_text_size)
                                       )
                               }
                           )

)
