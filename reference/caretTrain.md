# Wrapper to train caret models

This function is a wrapper around the \`train\` function from the
\`caret\` package. It allows for the passing of local and global
arguments to the \`train\` function. It also allows for the option to
continue on fail, and to trim the output model. Trimming the model
removes components that are not needed for stacking, to save memory and
speed up the stacking process. It also converts preds to a data.table.
Its an internal function for use with caretList.

## Usage

``` r
caretTrain(
  local_args,
  global_args,
  continue_on_fail = FALSE,
  trim = TRUE,
  aggregate_resamples = TRUE
)
```

## Arguments

- local_args:

  A list of arguments to pass to the \`train\` function.

- global_args:

  A list of arguments to pass to the \`train\` function.

- continue_on_fail:

  A logical indicating whether to continue if the \`train\` function
  fails. If \`TRUE\`, the function will return \`NULL\` if the \`train\`
  function fails.

- trim:

  A logical indicating whether to trim the output model. If \`TRUE\`,
  the function will remove some elements that are not needed from the
  output model.

- aggregate_resamples:

  A logical indicating whether to aggregate stacked predictions Default
  is TRUE.

## Value

The output of the \`train\` function.
