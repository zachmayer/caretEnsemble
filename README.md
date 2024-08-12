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

# caretEnsemble: [(Read the vignette!)](https://htmlpreview.github.io/?https://github.com/zachmayer/caretEnsemble/blob/master/doc/caretEnsemble-intro.html)

Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models. Use `caretList` to fit multiple models, and then use `caretStack` to stack them using a caret model.

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley), which in turn was inspired by Caruana et. al.'s (2004) paper [Ensemble Selection from Libraries of Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

If you want to do something similar in python, check out [vecstack](https://github.com/vecxoz/vecstack)

# Install the stable version from [CRAN](https://CRAN.R-project.org/package=caretEnsemble/):
```{R}
install.packages('caretEnsemble')
```

# Install the dev version from github:
```{R}
devtools::install_github('zachmayer/caretEnsemble')
```

There are also tagged versions of caretEnsemble on github you can install via devtools. For example, to install the [previous release of caretEnsemble](https://github.com/zachmayer/caretEnsemble/releases/tag/) use:
```{R}
devtools::install_github('zachmayer/caretEnsemble@2.0.3')
```
This is useful if the latest release breaks some aspect of your workflow. caretEnsemble is pure R with no compilation, so this command will work in a variety of environments.

# Code of Conduct:
Please note that this project is released with a [Contributor Code of Conduct](https://github.com/zachmayer/caretEnsemble/blob/master/.github/CONTRIBUTING.md). By participating in this project you agree to abide by its terms.

# Package development
This packages uses a Makefile. Use `make help` to see the supported options.

Use `make fix-style` to fix simple linting error. For iterating while writing code, run something like `make clean fix-style document lint spell test`, to re-document the code, lint and spellcheck it, then run unit tests.

use `make all` before making a pull request, which will also run R CMD CHECK and a code coverage check.

## First time dev setup:
run `make install` from the git repository to install the dev version of caretEnsemble, along with the necessary dependencies.