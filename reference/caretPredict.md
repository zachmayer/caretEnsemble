# Prediction wrapper for [`train`](https://rdrr.io/pkg/caret/man/train.html)

This is a prediction wrapper for
[`train`](https://rdrr.io/pkg/caret/man/train.html) with several
features: - If newdata is null, return stacked predictions from the
training job, rather than in-sample predictions. - Always returns
probabilities for classification models. - Optionally drops one
predicted class for classification models. - Always returns a
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

## Usage

``` r
caretPredict(
  object,
  newdata = NULL,
  excluded_class_id = 1L,
  aggregate_resamples = TRUE,
  ...
)
```

## Arguments

- object:

  a [`train`](https://rdrr.io/pkg/caret/man/train.html) object

- newdata:

  New data to use for predictions. If NULL, stacked predictions from the
  training data are returned.

- excluded_class_id:

  an integer indicating the class to exclude. If 0L, no class is
  excluded

- aggregate_resamples:

  logical, whether to aggregate resamples by keys. Default is TRUE.

- ...:

  additional arguments to pass to
  [`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html),
  if newdata is not NULL

## Value

a data.table
