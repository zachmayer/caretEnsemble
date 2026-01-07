# Validate a model type

Validate the model type from a
[`train`](https://rdrr.io/pkg/caret/man/train.html) object. For
classification, validates that the model can predict probabilities, and,
if stacked predictions are requested, that classProbs = TRUE.

## Usage

``` r
isClassifierAndValidate(object, validate_for_stacking = TRUE)
```

## Arguments

- object:

  a [`train`](https://rdrr.io/pkg/caret/man/train.html) object

- validate_for_stacking:

  a logical indicating whether to validate the object for stacked
  predictions

## Value

a logical. TRUE if classifier, otherwise FALSE.
