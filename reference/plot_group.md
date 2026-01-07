# Plot a group of variable importances

Generates a ggplot bar chart for a given set of variable importances.

## Usage

``` r
plot_group(imp_dt, title, fill_colors, y_max)
```

## Arguments

- imp_dt:

  data.table with columns \`method\`, \`weight\`, \`is_stat\`.

- title:

  Title for the plot.

- fill_colors:

  Named vector specifying colors for each fill group.

- y_max:

  Numeric maximum for the y-axis.

## Value

ggplot object.
