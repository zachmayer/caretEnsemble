# Combine several predictive models via weights

Find a greedy, positive only linear combination of several
[`train`](https://rdrr.io/pkg/caret/man/train.html) objects

Functions for creating ensembles of caret models: caretList and
caretStack

## Usage

``` r
caretEnsemble(all.models, excluded_class_id = 0L, tuneLength = 1L, ...)
```

## Arguments

- all.models:

  an object of class caretList

- excluded_class_id:

  The integer level to exclude from binary classification or multiclass
  problems. By default no classes are excluded, as the greedy optimizer
  requires all classes because it cannot use negative coefficients.

- tuneLength:

  The size of the grid to search for tuning the model. Defaults to 1, as
  the only parameter to optimize is the number of iterations, and the
  default of 100 works well.

- ...:

  additional arguments to pass caret::train

## Value

a `caretEnsemble` object

## Details

greedyMSE works well when you want an ensemble that will never be worse
than any single model in the dataset. In the worst case scenario, it
will select the single best model, if none of them can be ensembled to
improve the overall score. It will also never assign any model a
negative coefficient, which can help avoid unintuitive cases at
prediction time (e.g. if the correlations between predictors breaks down
on new data, negative coefficients can lead to bad results).

## Note

Every model in the "library" must be a separate `train` object. For
example, if you wish to combine a random forests with several different
values of mtry, you must build a model for each value of mtry. If you
use several values of mtry in one train model, (e.g. tuneGrid =
expand.grid(.mtry=2:5)), caret will select the best value of mtry before
we get a chance to include it in the ensemble. By default, RMSE is used
to ensemble regression models, and AUC is used to ensemble
Classification models. This function does not currently support
multi-class problems

## See also

Useful links:

- <http://zachmayer.github.io/caretEnsemble/>

- <https://github.com/zachmayer/caretEnsemble>

- Report bugs at <https://github.com/zachmayer/caretEnsemble/issues>

## Author

**Maintainer**: Zachary A. Deane-Mayer <zach.mayer@gmail.com>
\[copyright holder\]

Other contributors:

- Jared E. Knowles <jknowles@gmail.com> \[contributor\]

- Antón López <anton.gomez.lopez@rai.usc.es> \[contributor\]

## Examples

``` r
set.seed(42)
models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("rpart", "rf"))
#> Warning: There were missing values in resampled performance measures.
#> note: only 1 unique complexity parameters in default grid. Truncating the grid to 1 .
#> 
ens <- caretEnsemble(models)
summary(ens)
#> The following models were ensembled: rpart, rf  
#> 
#> Model Importance:
#> rpart    rf 
#> 0.952 0.048 
#> 
#> Model accuracy:
#>    model_name metric     value         sd
#>        <char> <char>     <num>      <num>
#> 1:   ensemble   RMSE 0.1725561 0.03171894
#> 2:      rpart   RMSE 0.1679543 0.04899049
#> 3:         rf   RMSE 0.1869204 0.03174027
```
