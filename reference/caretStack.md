# Combine several predictive models via stacking

Stack several [`train`](https://rdrr.io/pkg/caret/man/train.html) models
using a [`train`](https://rdrr.io/pkg/caret/man/train.html) model.

## Usage

``` r
caretStack(
  all.models,
  new_X = NULL,
  new_y = NULL,
  metric = NULL,
  trControl = NULL,
  excluded_class_id = 1L,
  original_features = NULL,
  aggregate_resamples = TRUE,
  ...
)
```

## Arguments

- all.models:

  a caretList, or an object coercible to a caretList (such as a list of
  train objects)

- new_X:

  Data to predict on for the caretList, prior to training the stack (for
  transfer learning). if NULL, the stacked predictions will be extracted
  from the caretList models.

- new_y:

  The outcome variable to predict on for the caretList, prior to
  training the stack (for transfer learning). If NULL, will use the
  observed levels from the first model in the caret stack If 0, will
  include all levels.

- metric:

  the metric to use for grid search on the stacking model.

- trControl:

  a trainControl object to use for training the ensemble model. If NULL,
  will use defaultControl.

- excluded_class_id:

  The integer level to exclude from binary classification or multiclass
  problems.

- original_features:

  a character vector of the names of the original features to include in
  the stack or NULL to not include any features. These features will be
  added to the stacked predictions from the models to train the ensemble
  model.

- aggregate_resamples:

  logical, whether to aggregate resamples by keys. Default is TRUE.

- ...:

  additional arguments to pass to the stacking model

## Value

S3 caretStack object

## Details

Uses either transfer learning or stacking to stack models. Assumes that
all models were trained on the same number of rows of data, with the
same target values. The features, cross-validation strategies, and model
types (class vs reg) may vary however. If your stack of models were
trained with different number of rows, please provide new_X and new_y so
the models can predict on a common set of data for stacking.

If your models were trained on different columns, you should use
stacking.

If you have both differing rows and columns in your model set, you are
out of luck. You need at least a common set of rows during training (for
stacking) or a common set of columns at inference time for transfer
learning.

## References

Caruana, R., Niculescu-Mizil, A., Crew, G., & Ksikes, A. (2004).
Ensemble Selection from Libraries of Models.
<https://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf>

## Examples

``` r
models <- caretList(
  x = iris[1:50, 1:2],
  y = iris[1:50, 3],
  methodList = c("rpart", "glm")
)
#> Warning: There were missing values in resampled performance measures.
caretStack(models, method = "glm")
#> The following models were ensembled: rpart, glm  
#> 
#> caret::train model:
#> Generalized Linear Model 
#> 
#> No pre-processing
#> Resampling: Cross-Validated (5 fold) 
#> Summary of sample sizes: 39, 40, 41, 39, 41 
#> Resampling results:
#> 
#>   RMSE       Rsquared    MAE     
#>   0.1789553  0.09387589  0.135978
#> 
#> 
#> Final model:
#> 
#> Call:  NULL
#> 
#> Coefficients:
#> (Intercept)        rpart          glm  
#>      0.6755       0.4072       0.1276  
#> 
#> Degrees of Freedom: 49 Total (i.e. Null);  47 Residual
#> Null Deviance:       1.478 
#> Residual Deviance: 1.444     AIC: -27.34
```
