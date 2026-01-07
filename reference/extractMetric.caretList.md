# Extract accuracy metrics from a [`caretList`](http://zachmayer.github.io/caretEnsemble/reference/caretList.md) object

Extract the cross-validated accuracy metrics from each model in a
caretList.

## Usage

``` r
# S3 method for class 'caretList'
extractMetric(x, ...)
```

## Arguments

- x:

  a caretList object

- ...:

  passed to extractMetric.train

## Value

A data.table with metrics from each model.
