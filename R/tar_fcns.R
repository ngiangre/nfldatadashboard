download_nextgen_stats_data <- function(){
    data_dir <- "data/"
    if(!dir.exists(data_dir)){
        dir.create(data_dir)
    }
    nflreadr::nflverse_download(
        nextgen_stats,
        folder_path = data_dir,
        file_type = "parquet")
}

get_season_data <- function(type,stattype,pos_filter){
    week <- player_position <- field <- description <- NULL
    week_fcn <- switch(type,
                       'avg' = `==`,
                       'wide' = `!=`)
    tmp <-
        nflreadr::load_nextgen_stats(season = 2023, stat_type = stattype) |>
        dplyr::filter(week_fcn(week,0) & player_position==pos_filter)
    if(type=='avg'){
        tmp <- dplyr::select(tmp,-week)
    }

    dict_ <- nflreadr::dictionary_nextgen_stats
    cols_to_label <- intersect(colnames(tmp),dict_$field)
    label_lst <-
        dict_ |>
        dplyr::filter(field %in% cols_to_label) |>
        dplyr::select(field,description) |>
        tibble::deframe()

    tmp |>
        labelled::set_variable_labels(!!!label_lst)
}
