# Extract accuracy metrics from a [`train`](https://rdrr.io/pkg/caret/man/train.html) model

Extract the cross-validated accuracy metrics and their SDs from caret.

## Usage

``` r
# S3 method for class 'train'
extractMetric(x, metric = NULL, ...)
```

## Arguments

- x:

  a train object

- metric:

  a character string representing the metric to extract.

- ...:

  ignored If NULL, uses the metric that was used to train the model.

## Value

A numeric representing the metric desired metric.
