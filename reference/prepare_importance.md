# Prepare variable importance data.table from a caretStack

Extracts variable importance from a \`caretStack\` model and returns a
\`data.table\` with columns \`method\` and \`weight\` sorted by
importance.

## Usage

``` r
prepare_importance(stack_model, newdata)
```

## Arguments

- stack_model:

  A trained \`caretStack\` model.

- newdata:

  A data frame used for calculating variable importance.

## Value

\`data.table\` with columns \`method\` (variable names) and \`weight\`
(importance).
