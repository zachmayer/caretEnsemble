# Extracts the target variable from a set of arguments headed to the caret::train.formula function.

This function extracts the y variable from a set of arguments headed to
a caret::train.formula model.

## Usage

``` r
# S3 method for class 'formula'
extractCaretTarget(form, data, ...)
```

## Arguments

- form:

  A formula of the form y ~ x1 + x2 + ...

- data:

  Data frame from which variables specified in formula are
  preferentially to be taken.

- ...:

  ignored
