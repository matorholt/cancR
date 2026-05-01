# Perform rowwise operations

Perform rowwise operations

## Usage

``` r
rowR(data, vars, type, label, na.rm = T, filter = NULL, direction, dt = F)
```

## Arguments

- data:

  dataset

- vars:

  vector of vars the rowwise operation should be perfomed. If missing,
  all columns are assigned

- type:

  pmin, pmax, all.na, any.na, sum.na, sum or fill

- label:

  label of the new variable. If missing, type is inserted (e.g. pmin)

- na.rm:

  whether NAs should be removed (default = T)

- filter:

  whether rows with all.na or any.na should be removed

- direction:

  the direction of the rowwise fill. Can be "left", "rigth", "leftright"
  and "rightleft". Corresponds to "updown".

## Value

the parent data frame with a new variable based on the selected rowwise
operator or a filtered dataset
