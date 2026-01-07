# Extracts the target variable from a set of arguments headed to the caret::train function.

This function extracts the y variable from a set of arguments headed to
a caret::train model. Since there are 2 methods to call caret::train,
this function also has 2 methods.

## Usage

``` r
extractCaretTarget(...)
```

## Arguments

- ...:

  a set of arguments, as in the caret::train function
