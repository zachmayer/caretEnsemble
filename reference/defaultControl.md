# Construct a default train control for use with caretList

Unlike caret::trainControl, this function defaults to 5 fold CV. CV is
good for stacking, as every observation is in the test set exactly once.
We use 5 instead of 10 to save compute time, as caretList is for fitting
many models. We also construct explicit fold indexes and return the
stacked predictions, which are needed for stacking. For classification
models we return class probabilities.

## Usage

``` r
defaultControl(
  target,
  method = "cv",
  number = 5L,
  savePredictions = "final",
  index = caret::createFolds(target, k = number, list = TRUE, returnTrain = TRUE),
  is_class = is.factor(target) || is.character(target),
  is_binary = length(unique(target)) == 2L,
  ...
)
```

## Arguments

- target:

  the target variable.

- method:

  the method to use for trainControl.

- number:

  the number of folds to use.

- savePredictions:

  the type of predictions to save.

- index:

  the fold indexes to use.

- is_class:

  logical, is this a classification or regression problem.

- is_binary:

  logical, is this binary classification.

- ...:

  other arguments to pass to
  [`trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html)
