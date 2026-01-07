# Shuffled MAE

Compute the mean absolute error of a model's predictions when a variable
is shuffled.

## Usage

``` r
shuffled_mae(model, original_data, target, pred_type, shuffle_idx)
```

## Arguments

- original_data:

  A data.table of the original data.

- target:

  A matrix of target values.

- shuffle_idx:

  A vector of shuffled indices.

## Value

A numeric vector of mean absolute errors.
