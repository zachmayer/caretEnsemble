# Variable importance for caretStack

This is a function to extract variable importance from a caretStack.

## Usage

``` r
# S3 method for class 'caretStack'
varImp(object, newdata = NULL, normalize = TRUE, ...)
```

## Arguments

- object:

  An object of class caretStack

- newdata:

  the data to use for computing importance. If NULL, will use the
  stacked predictions from the models

- normalize:

  a logical indicating whether to normalize the importances to sum to
  one.

- ...:

  passed to predict.caretList
