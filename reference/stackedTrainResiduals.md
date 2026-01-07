# Extracted stacked residuals for the autoplot

This function extracts the predictions, observeds, and residuals from a
`train` object. It uses the object's stacked predictions from
cross-validation.

## Usage

``` r
stackedTrainResiduals(object, show_class_id = 2L)
```

## Arguments

- object:

  a `train` object

- show_class_id:

  For classification only: which class level to use for residuals

## Value

a data.table::data.table with predictions, observeds, and residuals
