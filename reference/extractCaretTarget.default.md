# Extracts the target variable from a set of arguments headed to the caret::train.default function.

This function extracts the y variable from a set of arguments headed to
a caret::train.default model.

## Usage

``` r
# Default S3 method
extractCaretTarget(x, y, ...)
```

## Arguments

- x:

  an object where samples are in rows and features are in columns. This
  could be a simple matrix, data frame or other type (e.g. sparse
  matrix). See Details below.

- y:

  a numeric or factor vector containing the outcome for each sample.

- ...:

  ignored
