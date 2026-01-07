# Check that the methods supplied by the user are valid caret methods

This function uses modelLookup from caret to ensure the list of methods
supplied by the user are all models caret can fit.

## Usage

``` r
methodCheck(x)
```

## Arguments

- x:

  a list of user-supplied tuning parameters and methods
