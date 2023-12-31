#' plot_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @importFrom scico scale_fill_scico
#' @noRd
plot_object <- R6::R6Class("PlotObject",
                           public = list(
                               global_text_size = 16,
                               global_theme = theme_minimal(base_size = 16),
                               sa_heatmap = function(dat){
                                   dat |>
                                   mutate(zvalue = (value - mean(value))/sd(value),.by = statistic) |>
                                       ggplot(aes(player_display_name,statistic,fill=zvalue)) +
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
                               }
                           )

)
