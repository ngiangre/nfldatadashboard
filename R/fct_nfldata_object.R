#' nfldata_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' nfldata_obj <- nfldata_object$new()
#'
nfldata_object <- R6::R6Class("NFLDataObject",
                              public = list(
                                  player_stats = NULL,
                                  pbp_table = NULL,
                                  player_stats_cols = NULL,
                                  pbp_table_cols = NULL,
                                  game_ids = NULL,
                                  player_names = NULL,
                                  initialize = function(season = 2023){
                                      nflreadr::.clear_cache()
                                      self$player_stats <-
                                          nflreadr::load_player_stats(season)
                                      self$pbp_table <-
                                          nflreadr::load_pbp(season)
                                      self$player_stats_cols <- nflreadr::dictionary_player_stats$Field
                                      names(self$player_stats_cols) <-
                                          paste0(nflreadr::dictionary_player_stats$Field,
                                                 " (",
                                                 nflreadr::dictionary_player_stats$Description,
                                                 ")")
                                      self$pbp_table_cols <- nflreadr::dictionary_pbp$Field
                                      names(self$pbp_table_cols) <-
                                          paste0(nflreadr::dictionary_pbp$Field,
                                                 " (",
                                                 nflreadr::dictionary_pbp$Description,
                                                 ")")
                                      self$game_ids <-
                                          sort(unique(self$pbp_table[['game_id']]))
                                      self$player_names <-
                                          sort(unique(self$player_stats[['player_display_name']]))
                                  }
                              ))
