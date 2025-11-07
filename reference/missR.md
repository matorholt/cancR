# Overview of NAs in a dataframe

Overview of NAs in a dataframe

## Usage

``` r
missR(data, vars, id = id, print = T)
```

## Arguments

- data:

  data frame

- vars:

  vars where the na check should be beformed. If missing the whole data
  frame is analysed

- id:

  name of the id-column. If missing a guess at the most common names is
  attempted.

- print:

  whether the NA check should be printed in the console

## Value

Prints whether any NAs are detected and returns a data frame with IDs
and columns with NA
