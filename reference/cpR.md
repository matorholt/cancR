# Fix CPR numbers with removed leading zeros

Fix CPR numbers with removed leading zeros

## Usage

``` r
cpR(
  data,
  cpr = cpr,
  extract = FALSE,
  remove.cpr = FALSE,
  return.cpr = FALSE,
  dt = NULL
)
```

## Arguments

- data:

  dataset

- cpr:

  name of the cpr-column

- extract:

  TRUE if age and date of birth should be extracted

- remove.cpr:

  whether invalid CPRs should be removed, default = F

- return.cpr:

  whether the invalid CPRs should be returned as a vector, default = F

- dt:

  whether the dataframe should be returned as a data.table

## Value

Returns same dataset with correct CPR numbers and optionally age and
date of birth. Invalid CPRs stops the function and returns the invalid
CPRs as a vector.
