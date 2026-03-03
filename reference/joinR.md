# Join two or more dataframes

Join two or more dataframes as a left, right, full or inner join. The
dataframes can be provided separately or as a list.

## Usage

``` r
joinR(..., by, type = "left", dt = F)
```

## Arguments

- ...:

  dataframes or list of dataframes to be joined

- by:

  variable to join on. This can either be a vector of one or multiple
  keys (e.g. by = c("id", "group")) or a list of vectors containing key
  columns.

- type:

  type of join (e.g. left (default), right, full and inner)

- dt:

  whether the returned dataframe should be a data.table

## Value

a single joined dataframe
