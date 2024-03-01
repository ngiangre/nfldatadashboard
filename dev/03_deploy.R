#
#
#
devtools::check()
rhub::check_for_cran()
devtools::build()
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()
golem::add_dockerfile_with_renv()
golem::add_dockerfile_with_renv_shinyproxy()
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  appFiles = c(
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
