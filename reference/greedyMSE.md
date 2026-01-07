# Greedy optimization for MSE

Greedy optimization for minimizing the mean squared error. Works for
classification and regression.

## Usage

``` r
greedyMSE(X, Y, max_iter = 100L)
```

## Arguments

- X:

  A numeric matrix of features.

- Y:

  A numeric matrix of target values.

- max_iter:

  An integer scalar of the maximum number of iterations.

## Value

A list with components:

- model_weights:

  A numeric matrix of model_weights.

- RMSE:

  A numeric scalar of the root mean squared error.

- max_iter:

  An integer scalar of the maximum number of iterations.
