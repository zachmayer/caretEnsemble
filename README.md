[![Build Status](http://img.shields.io/travis/zachmayer/caretEnsemble.svg?style=flat)](https://travis-ci.org/zachmayer/caretEnsemble)
[![Coverage Status](http://img.shields.io/coveralls/zachmayer/caretEnsemble.svg?style=flat)](https://coveralls.io/r/zachmayer/caretEnsemble)
[![Github Issues](https://img.shields.io/github/issues-raw/badges/shields.svg?style=flat)](https://github.com/zachmayer/caretEnsemble/issues)
[![Pending Pull-Requests](http://githubbadges.herokuapp.com/zachmayer/caretEnsemble/pulls.svg?style=flat)](https://github.com/zachmayer/caretEnsemble/pulls)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/caretEnsemble)](http://cran.r-project.org/web/packages/caretEnsemble)
[![Downloads](http://cranlogs.r-pkg.org/badges/caretEnsemble)](http://cran.rstudio.com/package=caretEnsemble)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)
[![Badges](http://img.shields.io/:badges-8/8-ff6799.svg?style=flat)](https://github.com/badges/badgerbadgerbadger)

# caretEnsemble
Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models.  Use `caretList` to fit multiple models, and then use `caretEnsemble` to combine them greedily, or `caretStack` to combine them using a caret model.

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley), which in turn was inspired by Caruana et. al.'s (2004) paper [Ensemble Selection from Libraries of Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

# Install the stable version from [CRAN](http://cran.r-project.org/web/packages/caretEnsemble/):
```{R}
install.packages('caretEnsemble')
```

# Install the dev version from github:
```{R}
devtools::install_github('zachmayer/caretEnsemble')
```

There are also tagged versions of caretEnsemble on github you can install via devtools.  For example, to install the original draft of the API:
```{R}
devtools::install_github('zachmayer/caretEnsemble@0.0')
```

# Code of Conduct:
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
