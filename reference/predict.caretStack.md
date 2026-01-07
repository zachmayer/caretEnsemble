# Make predictions from a caretStack

Make predictions from a caretStack. This function passes the data to
each function in turn to make a matrix of predictions, and then
multiplies that matrix by the vector of weights to get a single,
combined vector of predictions.

## Usage

``` r
# S3 method for class 'caretStack'
predict(
  object,
  newdata = NULL,
  se = FALSE,
  level = 0.95,
  excluded_class_id = 0L,
  return_class_only = FALSE,
  verbose = FALSE,
  aggregate_resamples = TRUE,
  ...
)
```

## Arguments

- object:

  a
  [`caretStack`](http://zachmayer.github.io/caretEnsemble/reference/caretStack.md)
  to make predictions from.

- newdata:

  a new dataframe to make predictions on

- se:

  logical, should prediction errors be produced? Default is false.

- level:

  tolerance/confidence level should be returned

- excluded_class_id:

  Which class to exclude from predictions. Note that if the caretStack
  was trained with an excluded_class_id, that class is ALWAYS excluded
  from the predictions from the caretList of input models.
  excluded_class_id for predict.caretStack is for the final ensemble
  model. So different classes could be excluded from the caretList
  models and the final ensemble model.

- return_class_only:

  a logical indicating whether to return only the class predictions as a
  factor. If TRUE, the return will be a factor rather than a data.table.
  This is a convenience function, and should not be widely used. For
  example if you have a downstream process that consumes the output of
  the model, you should have that process consume probabilities for each
  class. This will make it easier to change prediction probability
  thresholds if needed in the future.

- verbose:

  a logical indicating whether to print progress

- aggregate_resamples:

  logical, whether to aggregate resamples by keys. Default is TRUE.

- ...:

  arguments to pass to
  [`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html)
  for the ensemble model. Do not specify type here. For classification,
  type will always be prob, and for regression, type will always be raw.

## Value

a data.table of predictions

## Details

Prediction weights are defined as variable importance in the stacked
caret model. This is not available for all cases such as where the
library model predictions are transformed before being passed to the
stacking model.

## Examples

``` r
models <- caretList(
  x = iris[1:100, 1:2],
  y = iris[1:100, 3],
  methodList = c("rpart", "glm")
)
#> Warning: There were missing values in resampled performance measures.
meta_model <- caretStack(models, method = "lm")
RMSE(predict(meta_model, iris[101:150, 1:2]), iris[101:150, 3])
#> Warning: argument is not numeric or logical: returning NA
#> [1] NA
```
