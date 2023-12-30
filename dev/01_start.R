#
#
#
golem::fill_desc(
  pkg_name = "nfldatadashboard",
  pkg_title = "PKG_TITLE",
  pkg_description = "PKG_DESC.",
  author_first_name = "AUTHOR_FIRST",
  author_last_name = "AUTHOR_LAST",
  author_email = "AUTHOR@MAIL.COM",
  repo_url = NULL,
  pkg_version = "0.0.0.9000"
)
golem::set_golem_options()
golem::install_dev_deps()
usethis::use_mit_license("Golem User")
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_git()
golem::use_recommended_tests()
golem::use_favicon()
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
rstudioapi::navigateToFile("dev/02_dev.R")
