#' Data Object for nfldatadashboard
#'
#' @description A fct function to store the various data tables and extraction
#' functions used in the dashboard
#'
#' @return object with class of type 'DataObject'
#'
#' @importFrom shiny tags
#' @importFrom targets tar_objects tar_read_raw
#' @importFrom dplyr starts_with mutate all_of select contains pull collect arrange filter
#' @importFrom purrr map map_chr reduce pluck
#' @importFrom arrow open_dataset
#' @importFrom data.table data.table
#' @noRd
#' @examples
#' data_obj <- data_object$new()
#' data_obj$get_common_vars()
#' c('qb','wr','te','rb') |>
#' purrr::set_names() |>
#' purrr::map(~{data_obj$get_position_vars(.x)})
#'
#' data_obj$get_analysis_data(analysis = "passing",
#' positions = "QB",
#' seasons = c(2023),
#' weeks = c(1))
#'
#' data_obj$get_analysis_data(analysis = "passing",
#' positions = "QB",
#' seasons = NULL,
#' weeks = NULL)
data_object <- R6::R6Class("DataObject",
                           public = list(
                               analysis_dataset = NULL,
                               arrow_data_path = "data/nextgen_stats/",
                               get_available_levels = function(analysis,lvl){
                                   arrow::open_dataset(paste0(self$arrow_data_path,
                                                              "ngs_",
                                                              analysis,
                                                              ".parquet")) |>
                                       dplyr::distinct(.data[[lvl]]) |>
                                       dplyr::collect() |>
                                       purrr::pluck(lvl) |>
                                       sort(decreasing = TRUE)
                               },
                               get_analysis_data = function(analysis,positions,seasons,weeks){
                                   arrow::open_dataset(paste0(self$arrow_data_path,
                                                              "ngs_",
                                                              analysis,
                                                              ".parquet")) |>
                                       dplyr::filter(
                                           player_position %in% positions &
                                               season %in% seasons &
                                               week %in% weeks) |>
                                       dplyr::arrange(season,week) |>
                                       dplyr::collect() |>
                                       dplyr::mutate(
                                           player_unique_id =
                                               paste0(.data[['player_display_name']],
                                                      " (",
                                                      .data[['team_abbr']],
                                                      "; ",
                                                      .data[['player_position']],
                                                      ")")
                                       ) |>
                                       data.table::data.table()
                               },
                               get_common_vars = function(){
                                   purrr::map(
                                       targets::tar_objects(),~{
                                           targets::tar_read_raw(.x) |>
                                               colnames()
                                           }) |>
                                       purrr::reduce(intersect)
                               },
                               get_position_vars = function(pos){
                                   stopifnot(pos %in% self$get_available_positions())
                                   purrr::map(
                                       targets::tar_objects(
                                           dplyr::starts_with(pos)
                                       ),~{
                                           targets::tar_read_raw(.x) |>
                                               colnames()
                                       }) |>
                                       purrr::reduce(intersect) |>
                                       setdiff(self$get_common_vars())
                               },
                               get_named_position_vars = function(pos){
                                   stopifnot(pos %in% self$get_available_positions())
                                   tmp <-
                                       purrr::map(self$get_position_season_data(pos,'sa'),
                                                  ~attr(.x,"label"))
                                   vars_named <- tmp[!(names(tmp) %in% self$get_common_vars())]
                                   named_vars <- names(vars_named)
                                   names(named_vars) <-
                                       purrr::map_chr(seq_along(vars_named),~{
                                           paste0(
                                               tags$b(names(vars_named)[.x]),
                                               "\n",
                                               tags$i(unname(vars_named)[.x])
                                           )
                                       })
                                   sort(named_vars,decreasing = FALSE)
                               },
                               get_position_players = function(pos){
                                   stopifnot(pos %in% self$get_available_positions())
                                   purrr::map(
                                       targets::tar_objects(
                                           dplyr::starts_with(pos)
                                       ),~{
                                           targets::tar_read_raw(.x) |>
                                               pull('player_display_name') |>
                                               unique()
                                       }) |>
                                       purrr::reduce(intersect)
                               },
                               get_position_named_players = function(pos){
                                   stopifnot(pos %in% self$get_available_positions())
                                   names_ <-
                                       purrr::map(
                                           targets::tar_objects(
                                               dplyr::starts_with(pos)
                                           ),~{
                                               targets::tar_read_raw(.x) |>
                                                   mutate(np = paste0(player_display_name," (",team_abbr,")")) |>
                                                   purrr::pluck("np")
                                           }) |>
                                       purrr::reduce(intersect)
                                   vals_ <- purrr::map_chr(stringr::str_split(names_," \\("),~.x[1])
                                   names(vals_) <- names_
                                   order_ <- purrr::map_chr(stringr::str_split(names_,"\\("),~.x[2]) |> order()
                                   vals_[order_]
                               },
                               get_position_season_data = function(pos,stype){
                                   stopifnot(pos %in% self$get_available_positions())
                                   stopifnot(stype %in% self$get_available_season_data())
                                   targets::tar_objects(
                                       dplyr::starts_with(pos) &
                                           dplyr::contains(stype)
                                   ) |>
                                       targets::tar_read_raw()
                               },
                               get_available_positions = function(){
                                   c('qb','wr','te','rb')
                               },
                               get_available_season_data = function(){
                                   c('sa','sw')
                               }
                           ),

)
