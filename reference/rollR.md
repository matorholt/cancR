# Assign rolling ID.

Useful after sorting a dataset, which changes the ID order. Works with
piping

## Usage

``` r
rollR(data, vars = id, label = order, dt = F)
```

## Arguments

- data:

  dataset

- vars:

  column(s) which unique ids should be based on

- label:

  label for new unique id column

- dt:

  whether a data.table should be returned

## Value

adds a new column to the dataset with unique ids based on original id
conditional on a sorting
