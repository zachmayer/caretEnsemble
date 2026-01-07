# Create a list of several train models from the caret package

Build a list of train objects suitable for ensembling using the
[`caretStack`](http://zachmayer.github.io/caretEnsemble/reference/caretStack.md)
function.

## Usage

``` r
caretList(
  ...,
  trControl = NULL,
  methodList = NULL,
  tuneList = NULL,
  metric = NULL,
  continue_on_fail = FALSE,
  trim = TRUE,
  aggregate_resamples = TRUE
)
```

## Arguments

- ...:

  arguments to pass to
  [`train`](https://rdrr.io/pkg/caret/man/train.html). Don't use the
  formula interface, its slower and buggier compared to the X, y
  interface. Use a
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  for X. Particularly if you have a large dataset and/or many models,
  using a data.table will avoid unnecessary copies of your data and can
  save a lot of time and RAM. These arguments will determine which train
  method gets dispatched.

- trControl:

  a [`trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html)
  object. If NULL, will use defaultControl.

- methodList:

  optional, a character vector of caret models to ensemble. One of
  methodList or tuneList must be specified.

- tuneList:

  optional, a NAMED list of caretModelSpec objects. This much more
  flexible than methodList and allows the specification of
  model-specific parameters (e.g. passing trace=FALSE to nnet)

- metric:

  a string, the metric to optimize for. If NULL, we will choose a good
  one.

- continue_on_fail:

  logical, should a valid caretList be returned that excludes models
  that fail, default is FALSE

- trim:

  logical should the train models be trimmed to save memory and speed up
  stacking

- aggregate_resamples:

  logical, whether to aggregate stacked predictions. Default is TRUE.

## Value

A list of [`train`](https://rdrr.io/pkg/caret/man/train.html) objects.
If the model fails to build, it is dropped from the list.

## Examples

``` r
caretList(
  Sepal.Length ~ Sepal.Width,
  head(iris, 50),
  methodList = c("glm", "lm"),
  tuneList = list(
    nnet = caretModelSpec(method = "nnet", trace = FALSE, tuneLength = 1L)
  )
)
#> Warning: There were missing values in resampled performance measures.
#> $nnet
#> Neural Network 
#> 
#> No pre-processing
#> Resampling results:
#> 
#>   RMSE      Rsquared  MAE     
#>   4.019965  NaN       4.004778
#> 
#> Tuning parameter 'size' was held constant at a value of 1
#> Tuning
#>  parameter 'decay' was held constant at a value of 0
#> 
#> $glm
#> Generalized Linear Model 
#> 
#> No pre-processing
#> Resampling results:
#> 
#>   RMSE   Rsquared   MAE      
#>   0.239  0.5661346  0.1976156
#> 
#> 
#> $lm
#> Linear Regression 
#> 
#> No pre-processing
#> Resampling results:
#> 
#>   RMSE   Rsquared   MAE      
#>   0.239  0.5661346  0.1976156
#> 
#> Tuning parameter 'intercept' was held constant at a value of TRUE
#> 
#> attr(,"class")
#> [1] "caretList" "list"     
```
