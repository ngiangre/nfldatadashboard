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
    command = get_season_data('avg','passing')
  ),
  tar_target(
      name = wr_sa_data,
      command = get_season_data('avg','receiving')
  ),
  tar_target(
      name = rb_sa_data,
      command = get_season_data('avg','rushing')
  ),
  tar_target(
      name = qb_sw_data,
      command = get_season_data('wide','passing')
  ),
  tar_target(
      name = wr_sw_data,
      command = get_season_data('wide','receiving')
  ),
  tar_target(
      name = rb_sw_data,
      command = get_season_data('wide','rushing')
  )
)
