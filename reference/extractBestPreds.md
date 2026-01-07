# Extract the best predictions from a train object

Extract the best predictions from a train object.

## Usage

``` r
extractBestPreds(x, aggregate_resamples = TRUE)
```

## Arguments

- x:

  a train object

- aggregate_resamples:

  logical, whether to aggregate resamples by keys. Default is TRUE.

## Value

a data.table::data.table with predictions
