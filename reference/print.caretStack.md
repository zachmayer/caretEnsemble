# Print a caretStack object

This is a function to print a caretStack.

## Usage

``` r
# S3 method for class 'caretStack'
print(x, ...)
```

## Arguments

- x:

  An object of class caretStack

- ...:

  ignored

## Examples

``` r
models <- caretList(
  x = iris[1:100, 1:2],
  y = iris[1:100, 3],
  methodList = c("rpart", "glm")
)
#> Warning: There were missing values in resampled performance measures.
meta_model <- caretStack(models, method = "lm")
print(meta_model)
#> The following models were ensembled: rpart, glm  
#> 
#> caret::train model:
#> Linear Regression 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 80, 79, 81, 80, 80 
#> Resampling results:
#> 
#>   RMSE       Rsquared   MAE      
#>   0.5541616  0.8669472  0.4294123
#> 
#> Tuning parameter 'intercept' was held constant at a value of TRUE
#> 
#> Final model:
#> 
#> Call:
#> lm(formula = .outcome ~ ., data = dat)
#> 
#> Coefficients:
#> (Intercept)        rpart          glm  
#>    -0.06621      0.14816      0.87952  
#> 
```
