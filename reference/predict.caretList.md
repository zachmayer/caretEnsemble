# Create a matrix of predictions for each of the models in a caretList

Make a matrix of predictions from a list of caret models

## Usage

``` r
# S3 method for class 'caretList'
predict(
  object,
  newdata = NULL,
  verbose = FALSE,
  excluded_class_id = 1L,
  aggregate_resamples = TRUE,
  ...
)
```

## Arguments

- object:

  an object of class caretList

- newdata:

  New data for predictions. It can be NULL, but this is ill-advised.

- verbose:

  Logical. If FALSE no progress bar is printed if TRUE a progress bar is
  shown. Default FALSE.

- excluded_class_id:

  Integer. The class id to drop when predicting for multiclass

- aggregate_resamples:

  logical, whether to aggregate resamples by keys. Default is TRUE.

- ...:

  Other arguments to pass to
  [`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html)
