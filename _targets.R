library(targets)

tar_option_set(
  packages = c("nflreadr","tibble","dplyr","labelled")
)

tar_source(files = c(
    'R/tar_fcns.R'
))

list(
  tar_target(
    name = qb_sa_data,
    command = get_season_data('avg','passing','QB')
  ),
  tar_target(
      name = wr_sa_data,
      command = get_season_data('avg','receiving',"WR")
  ),
  tar_target(
      name = te_sa_data,
      command = get_season_data('avg','receiving',"TE")
  ),
  tar_target(
      name = rb_sa_data,
      command = get_season_data('avg','rushing',"RB")
  ),
  tar_target(
      name = qb_sw_data,
      command = get_season_data('wide','passing','QB')
  ),
  tar_target(
      name = wr_sw_data,
      command = get_season_data('wide','receiving','WR')
  ),
  tar_target(
      name = te_sw_data,
      command = get_season_data('wide','receiving','TE')
  ),
  tar_target(
      name = rb_sw_data,
      command = get_season_data('wide','rushing',"RB")
  )
)
