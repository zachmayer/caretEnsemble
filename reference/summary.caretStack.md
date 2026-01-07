# Summarize a caretStack object

This is a function to summarize a caretStack.

## Usage

``` r
# S3 method for class 'caretStack'
summary(object, ...)
```

## Arguments

- object:

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
summary(meta_model)
#> The following models were ensembled: rpart, glm  
#> 
#> Model Importance:
#>  rpart    glm 
#> 0.1269 0.8731 
#> 
#> Model accuracy:
#>    model_name metric     value         sd
#>        <char> <char>     <num>      <num>
#> 1:   ensemble   RMSE 0.5709763 0.05535683
#> 2:      rpart   RMSE 0.8325594 0.29191430
#> 3:        glm   RMSE 0.5636149 0.06308322
```
