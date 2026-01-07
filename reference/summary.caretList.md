# Summarize a caretList

This function summarizes the performance of each model in a caretList
object.

## Usage

``` r
# S3 method for class 'caretList'
summary(object, metric = NULL, ...)
```

## Arguments

- object:

  a caretList object

- metric:

  The metric to show. If NULL will use the metric used to train each
  model

- ...:

  passed to extractMetric

## Value

A data.table with metrics from each model.
