[![Build Status](https://travis-ci.org/zachmayer/caretEnsemble.png?branch=master)](https://travis-ci.org/zachmayer/caretEnsemble)
[![Coverage Status](https://coveralls.io/repos/zachmayer/caretEnsemble/badge.svg)](https://coveralls.io/r/zachmayer/caretEnsemble)

Package: caretEnsemble    
Type: Package     
Title: Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models.  Use `caretList` to fit multiple models, and then use `caretEnsemble` to combine them greedily, or `caretStack` to combine them using a caret model. 

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley), which in turn was inspired by Caruana et. al.'s (2004) paper [Ensemble Selection from Libraries of Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

To install the latest stable version from CRAN ([You can also view the package page on CRAN](http://cran.r-project.org/web/packages/caretEnsemble/)):
```{R}
install.packages('caretEnsemble')
```

To install the latest development version from github:
```{R}
devtools::install_github('zachmayer/caretEnsemble')
```

There are also tagged versions of caretEnsemble on github you can install via devtools.  For example, to install the original draft of the API:
```{R}
devtools::install_github('zachmayer/caretEnsemble@0.0')
```
