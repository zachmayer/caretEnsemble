devtools::install_deps()
devtools::install_dev_deps()

devtools::check()
devtools::check(cran=T, remote=T, manual=T, error_on="note")
devtools::check(cran=F, remote=T, manual=T, error_on="note", force_suggests=T)

devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
