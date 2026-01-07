# Construct a default metric

Caret defaults to RMSE for classification and RMSE for regression. For
classification, I would rather use ROC.

## Usage

``` r
defaultMetric(is_class, is_binary)
```

## Arguments

- is_class:

  logical, is this a classification or regression problem.

- is_binary:

  logical, is this binary classification.
