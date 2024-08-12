[![CRAN version](https://www.r-pkg.org/badges/version/caretEnsemble)](https://cran.r-project.org/package=caretEnsemble)
[![CRAN status](https://badges.cranchecks.info/worst/caretEnsemble.svg)](https://cran.r-project.org/web/checks/check_results_caretEnsemble.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/last-month/caretEnsemble)](https://www.rdocumentation.org/packages/caretEnsemble)
[![R-CMD-check](https://github.com/zachmayer/caretEnsemble/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/zachmayer/caretEnsemble/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/zachmayer/caretEnsemble/actions/workflows/tests.yaml/badge.svg?branch=main)](https://github.com/zachmayer/caretEnsemble/actions/workflows/tests.yaml)
[![Code Coverage](https://codecov.io/gh/zachmayer/caretEnsemble/graph/badge.svg?token=IEtOlZDYMs)](https://codecov.io/gh/zachmayer/caretEnsemble)
[![CodeFactor](https://www.codefactor.io/repository/github/zachmayer/caretEnsemble/badge)](https://www.codefactor.io/repository/github/zachmayer/caretEnsemble)
[![r-universe status badge](https://zachmayer.r-universe.dev/badges/caretEnsemble)](https://zachmayer.r-universe.dev/caretEnsemble)
[![Code Size](https://img.shields.io/github/languages/code-size/zachmayer/caretEnsemble.svg)](https://github.com/zachmayer/caretEnsemble)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](https://badges.mit-license.org)
[![Discord](https://img.shields.io/discord/1255535052578619524)](https://discord.gg/zgPeSm8a3u)

# caretEnsemble:

Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models. Use `caretList` to fit multiple models, and then use `caretStack` to stack them using a caret model.

```r
library(caretEnsemble)
data(diamonds, package='ggplot2')
dat <- data.table::data.table(diamonds)
dat <- dat[sample.int(nrow(diamonds), 250L),]
models <- caretList(price ~ ., data=dat, methodList = c('rf', 'glmnet'))
greedy_stack <- caretEnsemble(models)
ggplot2::autoplot(greedy_stack, training_data = dat, xvars =  c('carat', 'table'))
```

# Installation
###  Install the stable version from [CRAN](https://CRAN.R-project.org/package=caretEnsemble/):
```r
install.packages('caretEnsemble')
```

### Install the dev version from github:
```r
devtools::install_github('zachmayer/caretEnsemble')
```

There are also tagged versions of caretEnsemble on github you can install via devtools. For example, to install the original draft of the API:
```r
devtools::install_github('zachmayer/caretEnsemble@0.0')
```
caretEnsemble has changed a bit over the years, so the tags let you use older versions of the package (e.g. if you want to use greedy ensembling).

# Package development
This packages uses a Makefile. Run `make all` to run linting, spell checking, tests, test coverage, and R CMD CHECK. This is helpful for locally debugging PR failures.

Run `make dev` for a quicker local develipment loop that just runs linting, spelling, and unit tests.

# Inspiration and similar packages:
caretEnsemble was inspired by [medley](https://github.com/mewo2/medley), which in turn was inspired by Caruana et. al.'s (2004) paper [Ensemble Selection from Libraries of Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

If you want to do something similar in python, check out [vecstack](https://github.com/vecxoz/vecstack)
# Code of Conduct:
Please note that this project is released with a [Contributor Code of Conduct](https://github.com/zachmayer/caretEnsemble/blob/master/.github/CONTRIBUTING.md). By participating in this project you agree to abide by its terms.
