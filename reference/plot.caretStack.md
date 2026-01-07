# Plot a caretStack object

This function plots the performance of each model in a caretList object.

## Usage

``` r
# S3 method for class 'caretStack'
plot(x, metric = NULL, ...)
```

## Arguments

- x:

  a caretStack object

- metric:

  which metric to plot. If NULL, will use the default metric used to
  train the model.

- ...:

  ignored

## Value

a ggplot2 object
