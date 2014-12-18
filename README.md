[![Build Status](https://travis-ci.org/zachmayer/caretEnsemble.png?branch=master)](https://travis-ci.org/zachmayer/caretEnsemble)

Package: caretEnsemble    
Type: Package     
Title: Framework for fitting multiple [caret models](https://github.com/topepo/caret) using the same re-sampling strategy as well as creating ensembles of such models.  Use `caretList` to fit multiple models, and then use `caretEnsemble` to combine them in a greedy fasion, or `caretStack` to combine them using a caret model. 

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley).

Install the latest stable version from CRAN as follows:
```{R}
install.packages('caretEnsemble')
```

Install the latest development version as follows:
```{R}
devtools::install_github('zachmayer/caretEnsemble')
```

There are also several tags in github you can install using devtools.  For example, to install the original draft of the API, use:
```{R}
devtools::install_github('zachmayer/caretEnsemble@0.0')
```
