# S3 definition for concatenating caretList

take N objects of class caretList and concatenate them into a larger
object of class caretList for future ensembling

## Usage

``` r
# S3 method for class 'caretList'
c(...)
```

## Arguments

- ...:

  the objects of class caretList or train to bind into a caretList

## Value

a
[`caretList`](http://zachmayer.github.io/caretEnsemble/reference/caretList.md)
object

## Examples

``` r
data(iris)
model_list1 <- caretList(Sepal.Width ~ .,
  data = iris,
  tuneList = list(
    lm = caretModelSpec(method = "lm")
  )
)

model_list2 <- caretList(Sepal.Width ~ .,
  data = iris, tuneLength = 1L,
  tuneList = list(
    rf = caretModelSpec(method = "rf")
  )
)

bigList <- c(model_list1, model_list2)
```
