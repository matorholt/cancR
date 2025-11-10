# Recode multiple variables

Recode multiple variables

## Usage

``` r
recodR(data, namelist, match = "exact", replace = F)
```

## Arguments

- data:

  dataframe

- namelist:

  list og variables with named vector(s) containing diagnosis codes
  (e.g. list(var1 = list(name = diagnosis codes)))

- match:

  Whether the provided diagnosis codes should be matched exactly, start
  with/end with, be bounded by or contain (default)

- replace:

  whether multiple matches should be replaced directly (such as 1,4 to
  head, arm)

## Value

the input data frame with recoded variables
