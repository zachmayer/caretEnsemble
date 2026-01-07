# Generate a specification for fitting a caret model

A caret model specification consists of 2 parts: a model (as a string)
and the arguments to the train call for fitting that model

## Usage

``` r
caretModelSpec(method = "rf", ...)
```

## Arguments

- method:

  the modeling method to pass to caret::train

- ...:

  Other arguments that will eventually be passed to caret::train

## Value

a list of lists

## Examples

``` r
caretModelSpec("rf", tuneLength = 5L, preProcess = "ica")
#> $method
#> [1] "rf"
#> 
#> $tuneLength
#> [1] 5
#> 
#> $preProcess
#> [1] "ica"
#> 
```
