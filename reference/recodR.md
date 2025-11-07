# Recode multiple variables

Recode multiple variables

## Usage

``` r
recodR(data, namelist, match = "exact")
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

## Value

the input data frame with recoded variables
