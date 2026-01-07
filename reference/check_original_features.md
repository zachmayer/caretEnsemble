# Check original_features parameter

Check the original_features parameter for caretStack, predict.caretStack
and varImp.caretStack

## Usage

``` r
check_original_features(original_features, newdata, training = TRUE)
```

## Arguments

- original_features:

  a character vector with the names of the original features to include
  in the stack

- newdata:

  the data to use for train a model or make predictions

- training:

  a logical indicating whether the function is being called during
  training or prediction
