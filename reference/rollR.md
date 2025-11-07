# Assign rolling ID.

Useful after sorting a dataset, which changes the ID order. Works with
piping

## Usage

``` r
rollR(data, id = id, lab = order)
```

## Arguments

- data:

  dataset

- id:

  column which unique ids should be based on

- lab:

  label for new unique id column

## Value

adds a new column to the dataset with unique ids based on original id
conditional on a sorting
