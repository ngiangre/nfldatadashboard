#' data_object
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' data_obj <- data_object$new()
#'
data_object <- R6::R6Class("DataObject",
                           public = list(
                               get_common_vars = function(){
                                   purrr::map(
                                       targets::tar_objects(),~{
                                           targets::tar_read_raw(.x) |>
                                               colnames()
                                           }) |>
                                       purrr::reduce(intersect)
                               },
                               get_position_vars = function(pos){
                                   stopifnot(pos %in% c('qb','wr','rb'))
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
                               get_position_season_data = function(pos,stype){
                                   stopifnot(pos %in% c('qb','wr','rb'))
                                   stopifnot(stype %in% c('sa','sw'))
                                   targets::tar_objects(
                                       dplyr::starts_with(pos) &
                                           dplyr::contains(stype)
                                   ) |>
                                       targets::tar_read_raw()
                               }
                           )

)
