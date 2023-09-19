[![Build Status](http://img.shields.io/travis/zachmayer/caretEnsemble.svg?style=flat)](https://app.travis-ci.com/zachmayer/caretEnsemble)
[![Coverage Status](http://img.shields.io/coveralls/zachmayer/caretEnsemble.svg?style=flat)](https://coveralls.io/github/zachmayer/caretEnsemble)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/caretEnsemble)](https://CRAN.R-project.org/package=caretEnsemble/)
[![Downloads](http://cranlogs.r-pkg.org/badges/caretEnsemble)](https://CRAN.R-project.org/package=caretEnsemble/)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](https://badges.mit-license.org)
[![Badges](http://img.shields.io/:badges-7/7-438cf0.svg?style=flat)](https://github.com/badges/badgerbadgerbadger)
[![Join the chat at https://gitter.im/zachmayer/caretEnsemble](https://badges.gitter.im/Join%20Chat.svg)](https://app.gitter.im/#/room/#zachmayer_caretEnsemble:gitter.im)

# caretEnsemble: [(Read the vignette!)](https://htmlpreview.github.io/?https://github.com/zachmayer/caretEnsemble/blob/master/doc/caretEnsemble-intro.html)

Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models.  Use `caretList` to fit multiple models, and then use `caretEnsemble` to combine them greedily, or `caretStack` to combine them using a caret model.

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley), which in turn was inspired by Caruana et. al.'s (2004) paper [Ensemble Selection from Libraries of Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

# Install the stable version from [CRAN](https://CRAN.R-project.org/package=caretEnsemble/):
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
Please note that this project is released with a [Contributor Code of Conduct](https://github.com/zachmayer/caretEnsemble/blob/master/.github/CONTRIBUTING.md). By participating in this project you agree to abide by its terms.
