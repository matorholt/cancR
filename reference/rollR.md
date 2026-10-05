# Perform rolling operations

Useful for conditional grouping

## Usage

``` r
rollR(
  data,
  by = NULL,
  order,
  sort = 1L,
  label = grp,
  type = "roll",
  dt = F,
  vars,
  interval,
  lag = 1
)
```

## Arguments

- data:

  dataset

- by:

  vector of grouping column labels

- order:

  vector of variables to order the dataset

- sort:

  vector of the values c(1, -1) of length equal to the order argument

- label:

  label for new unique id column

- type:

  type of grouping operation, see details. Default = "roll"

- dt:

  whether a data.table should be returned

- vars:

  the variable used for computing lagged differences if type = "interval

- interval:

  vector of length 2 for evaluation of whether the lagged differences
  are within the two bounds

- lag:

  length of lag for lagged differences (default = 1)

## Value

adds a new column to the dataset with unique ids based on original id
conditional on a sorting

## Details

Types of roll include:

- roll: Scans the by argument of unique values and assigns these ids

- count: simple row counter by group

- interval: groups based on whether the lagged difference is within the
  bounds of an interval (arguments vars, interval and)
