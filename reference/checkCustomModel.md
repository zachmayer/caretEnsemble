# Validate a custom caret model info list

Currently, this only ensures that all model info lists were also
assigned a "method" attribute for consistency with usage of non-custom
models

## Usage

``` r
checkCustomModel(x)
```

## Arguments

- x:

  a model info list (e.g. `getModelInfo("rf", regex=F)\[[1]]`)

## Value

validated model info list (i.e. x)
