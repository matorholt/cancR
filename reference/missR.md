# Overview of NAs in a dataframe

Overview of NAs in a dataframe

## Usage

``` r
missR(
  data,
  vars,
  drop.rows = F,
  drop.cols = F,
  return.id = F,
  na.remove = "all.na",
  return.data = F,
  dt = NULL,
  print = T,
  verbose = T
)
```

## Arguments

- data:

  data frame

- vars:

  vars where the na check should be beformed. If missing the whole data
  frame is analysed

- drop.rows:

  whether to remove rows containing all NA values, default = F

- drop.cols:

  whether to remove columns contain all NA values, default = F

- return.id:

  whether rows with any NA should be returned, default = F

- na.remove:

  whether all ("all.na") or any ("any.na") NAs should be removed
  (default = "all.na")

- dt:

  whether the data.frame should be returned as a data.table, default = F

- print:

  whether the NA check should be printed in the console, default = T

- verbose:

  whether cli messages should be printed, default = T

## Value

Prints whether any NAs are detected and returns a data frame with IDs
and columns with NA

## Details

If drop.cols or drop.rows are TRUE, the data.frame is returned as
modified. Otherwise the table of missing data is returned.
