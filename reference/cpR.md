# Fix CPR numbers with removed leading zeros

Fix CPR numbers with removed leading zeros

## Usage

``` r
cpR(data, cpr = cpr, extract = F)
```

## Arguments

- data:

  dataset

- cpr:

  cpr-column

- extract:

  TRUE if age and date of birth should be extracted

## Value

Returns same dataset with correct CPR numbers and optionally age and
date of birth. Invalid CPRs stops the function and returns the invalid
CPRs as a vector.
