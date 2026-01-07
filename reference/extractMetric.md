# Generic function to extract accuracy metrics from various model objects

A generic function to extract cross-validated accuracy metrics from
model objects.

## Usage

``` r
extractMetric(x, ...)
```

## Arguments

- x:

  An object from which to extract metrics. The specific method will be
  dispatched based on the class of `x`.

- ...:

  Additional arguments passed to the specific methods.

## Value

A
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

## See also

[`extractMetric.train`](http://zachmayer.github.io/caretEnsemble/reference/extractMetric.train.md),
[`extractMetric.caretList`](http://zachmayer.github.io/caretEnsemble/reference/extractMetric.caretList.md),
[`extractMetric.caretStack`](http://zachmayer.github.io/caretEnsemble/reference/extractMetric.caretStack.md)
