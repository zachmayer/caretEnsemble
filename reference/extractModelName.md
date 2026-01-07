# Extract the method name associated with a single train object

Extracts the method name associated with a single train object. Note
that for standard models (i.e. those already prespecified by caret), the
"method" attribute on the train object is used directly while for custom
models the "method" attribute within the model\$modelInfo attribute is
used instead.

## Usage

``` r
extractModelName(x)
```

## Arguments

- x:

  a single caret train object

## Value

Name associated with model
