# Plot Variable Importance from a caretStack Model

This function plots the variable importance from a stacked ensemble
model (\`caretStack\`), separating original features from new
(engineered) features. It optionally includes cross-group summary
statistics (mean, sum, or max) from one feature group into the other for
visual reference. It returns a `ggplot` object; if both original and new
features are present, the plot will contain two facets (original and new
features) within the same figure. This is useful for diagnosing which
group of features contributes more to the stacked model.

## Usage

``` r
plot_variable_importance(stack_model, newdata, stat_type = NULL)
```

## Arguments

- stack_model:

  A `caretStack` model trained using `caretEnsemble`. It should have an
  attribute `original_features` listing the original input variables. If
  this attribute is missing, all variables are treated as "new".

- newdata:

  A data frame containing the data used for calculating variable
  importance. Typically this should be the validation or test set.

- stat_type:

  Optional character string indicating which summary statistic of the
  opposite group to include as a gray bar for reference. Must be one of
  `"mean"`, `"sum"`, or `"max"`. If `NULL`, no statistic is shown. If
  invalid, an error is thrown.

## Value

A `ggplot` object. If the model includes both original and new features,
the plot will contain two facets ("Original Features" and "New
Features"). If `stat_type` is provided, a gray bar appears in each plot
representing the selected summary statistic from the opposite group
(e.g., mean of new features shown in the original features plot).

## Details

\- Variable importance is computed using
[`caret::varImp`](https://rdrr.io/pkg/caret/man/varImp.html). - If the
model lacks the `original_features` attribute, all variables are
considered new. - Requires the packages: `data.table`, `ggplot2`, and
`caret`.
