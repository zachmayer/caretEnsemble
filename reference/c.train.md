# S3 definition for concatenating train objects

take N objects of class train and concatenate into an object of class
caretList for future ensembling

## Usage

``` r
# S3 method for class 'train'
c(...)
```

## Arguments

- ...:

  the objects of class train to bind into a caretList

## Value

a
[`caretList`](http://zachmayer.github.io/caretEnsemble/reference/caretList.md)
object

## Examples

``` r
data(iris)
model_lm <- caret::train(Sepal.Length ~ .,
  data = iris,
  method = "lm"
)

model_rf <- caret::train(Sepal.Length ~ .,
  data = iris,
  method = "rf",
  tuneLength = 1L
)

model_list <- c(model_lm, model_rf)
```
