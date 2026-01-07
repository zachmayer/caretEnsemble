# caret interface for greedyMSE

caret interface for greedyMSE. greedyMSE works well when you want an
ensemble that will never be worse than any single predictor in the
dataset. It does not use an intercept and it does not allow for negative
coefficients. This makes it highly constrained and in general does not
work well on standard classification and regression problems. However,
it does work well in the case of: \* The predictors are highly
correlated with each other \* The predictors are highly correlated with
the model \* You expect or want positive only coefficients In the worse
case, this method will select one input and use that, but in many other
cases it will return a positive, weighted average of the inputs. Since
it never uses negative weights, you never get into a scenario where one
model is weighted negative and on new data you get were predictions
because a correlation changed. Since this model will always be a
positive weighted average of the inputs, it will rarely do worse than
the individual models on new data.

## Usage

``` r
greedyMSE_caret()
```
