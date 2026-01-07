# Extract accuracy metrics from a [`caretStack`](http://zachmayer.github.io/caretEnsemble/reference/caretStack.md) object

Extract the cross-validated accuracy metrics from the ensemble model and
individual models in a caretStack.

## Usage

``` r
# S3 method for class 'caretStack'
extractMetric(x, ...)
```

## Arguments

- x:

  a caretStack object

- ...:

  passed to extractMetric.train and extractMetric.caretList

## Value

A data.table with metrics from the ensemble model and individual models.
