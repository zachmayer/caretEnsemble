# Add cross-group statistics to the importance table

Classifies each variable as either "Original" or "New" and optionally
appends a summary statistic (mean, sum, or max) of the opposite group.
The added statistic is flagged with \`is_stat = TRUE\` and can be used
for plotting a reference bar.

## Usage

``` r
add_cross_group_stats(imp_dt, original_features, stat_type = NULL)
```

## Arguments

- imp_dt:

  data.table from \`prepare_importance\`.

- original_features:

  Character vector of original features.

- stat_type:

  Character string: "mean", "sum", "max", or NULL.

## Value

List with \`imp_original\` and \`imp_new\` data.tables.
