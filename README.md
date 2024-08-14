
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caretEnsemble

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/caretEnsemble)](https://cran.r-project.org/package=caretEnsemble)
[![CRAN
status](https://badges.cranchecks.info/worst/caretEnsemble.svg)](https://cran.r-project.org/web/checks/check_results_caretEnsemble.html)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/last-month/caretEnsemble)](https://www.rdocumentation.org/packages/caretEnsemble)
[![R-CMD-check](https://github.com/zachmayer/caretEnsemble/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/zachmayer/caretEnsemble/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/zachmayer/caretEnsemble/actions/workflows/tests.yaml/badge.svg?branch=main)](https://github.com/zachmayer/caretEnsemble/actions/workflows/tests.yaml)
[![Code
Coverage](https://codecov.io/gh/zachmayer/caretEnsemble/graph/badge.svg?token=IEtOlZDYMs)](https://codecov.io/gh/zachmayer/caretEnsemble)
[![CodeFactor](https://www.codefactor.io/repository/github/zachmayer/caretEnsemble/badge)](https://www.codefactor.io/repository/github/zachmayer/caretEnsemble)
[![r-universe status
badge](https://zachmayer.r-universe.dev/badges/caretEnsemble)](https://zachmayer.r-universe.dev/caretEnsemble)
[![Code
Size](https://img.shields.io/github/languages/code-size/zachmayer/caretEnsemble.svg)](https://github.com/zachmayer/caretEnsemble)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](https://badges.mit-license.org)
[![Discord](https://img.shields.io/discord/1255535052578619524)](https://discord.gg/zgPeSm8a3u)
<!-- badges: end -->

caretEnsemble is a framework for
[stacking](https://en.wikipedia.org/wiki/Ensemble_learning#Stacking)
models fit with the [caret](https://topepo.github.io/caret/) package.

Use `caretList` to fit multiple models, and then use `caretStack` to
stack them with another caret model.

First, use caretList to fit many models to the same data:

``` r
data(diamonds, package = "ggplot2")
dat <- data.table::data.table(diamonds)
dat <- dat[sample.int(nrow(diamonds), 500L), ]
models <- caretEnsemble::caretList(
  price ~ .,
  data = dat,
  methodList = c("rf", "glmnet")
)
print(summary(models))
#> The following models were ensembled: rf, glmnet  
#> 
#> Model accuracy:
#>    model_name metric    value       sd
#>        <char> <char>    <num>    <num>
#> 1:         rf   RMSE 1044.363 239.2775
#> 2:     glmnet   RMSE 1144.750 278.2744
```

Then, use caretEnsemble to make a greedy ensemble of these models

``` r
greedy_stack <- caretEnsemble::caretEnsemble(models)
print(greedy_stack)
#> The following models were ensembled: rf, glmnet  
#> 
#> caret::train model:
#> Greedy Mean Squared Error Optimizer 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 400, 400, 400, 400, 400 
#> Resampling results:
#> 
#>   RMSE      Rsquared   MAE     
#>   1000.307  0.9372951  543.9684
#> 
#> Tuning parameter 'max_iter' was held constant at a value of 100
#> 
#> Final model:
#> Greedy MSE
#> RMSE:  1001.834 
#> Weights:
#>        [,1]
#> rf     0.62
#> glmnet 0.38
```

You can also use caretStack to make a non-linear ensemble

``` r
rf_stack <- caretEnsemble::caretStack(models, method = "rf")
#> note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
print(rf_stack)
#> The following models were ensembled: rf, glmnet  
#> 
#> caret::train model:
#> Random Forest 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 400, 400, 400, 400, 400 
#> Resampling results:
#> 
#>   RMSE      Rsquared   MAE     
#>   924.4763  0.9476619  482.2729
#> 
#> Tuning parameter 'mtry' was held constant at a value of 2
#> 
#> Final model:
#> 
#> Call:
#>  randomForest(x = x, y = y, mtry = param$mtry) 
#>                Type of random forest: regression
#>                      Number of trees: 500
#> No. of variables tried at each split: 2
#> 
#>           Mean of squared residuals: 910334.2
#>                     % Var explained: 94.28
```

Use autoplot from ggplot2 to plot ensemble diagnostics:

``` r
ggplot2::autoplot(greedy_stack, training_data = dat, xvars = c("carat", "table"))
```

<img src="man/figures/README-greedy-stack-6-plot-1.png" alt="6 panel plot of an ensemble of models fit to the diamonds dataset. The RF model is the best and has the highest weight. The residual plots look good. RMSE is about `r round(min(greedy_stack$ens_model$results$RMSE))`." width="100%" />

``` r
ggplot2::autoplot(rf_stack, training_data = dat, xvars = c("carat", "table"))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="6 panel plot of an ensemble of models fit to the diamonds dataset. The RF model is the best and has the highest weight. The residual plots look good. RMSE is about `r round(min(rf_stack$ens_model$results$RMSE))`." width="100%" />

# Installation

### Install the stable version from [CRAN](https://CRAN.R-project.org/package=caretEnsemble/):

``` r
install.packages("caretEnsemble")
```

### Install the dev version from github:

``` r
devtools::install_github("zachmayer/caretEnsemble")
```

There are also tagged versions of caretEnsemble on github you can
install via devtools. For example, to install the [previous release of
caretEnsemble](https://github.com/zachmayer/caretEnsemble/releases/)
use:

``` r
devtools::install_github("zachmayer/caretEnsemble@2.0.3")
```

This is useful if the latest release breaks some aspect of your
workflow. caretEnsemble is pure R with no compilation, so this command
will work in a variety of environments.

# Package development

This package uses a Makefile. Use `make help` to see the supported
options.

Use `make fix-style` to fix simple linting errors.

For iterating while writing code, run `make dev`. This runs just
`make clean fix-style document lint spell test`, for a quicker local dev
loop. Please still run `make all` before making a PR.

Use `make all` before making a pull request, which will also run R CMD
CHECK and a code coverage check. This runs
`make clean fix-style document install build-readme build-vignettes lint spell test check coverage preview-site`.

## First time dev setup:

run `make install` from the git repository to install the dev version of
caretEnsemble, along with the necessary package dependencies. \#
Inspiration and similar packages: caretEnsemble was inspired by
[medley](https://github.com/mewo2/medley), which in turn was inspired by
Caruana et. al.’s (2004) paper [Ensemble Selection from Libraries of
Models.](http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf)

If you want to do something similar in python, check out
[vecstack](https://github.com/vecxoz/vecstack).

# Code of Conduct:

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/zachmayer/caretEnsemble/blob/master/.github/CONTRIBUTING.md).
By participating in this project you agree to abide by its terms.
