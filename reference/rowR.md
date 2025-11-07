# Perform rowwise operations

Perform rowwise operations

## Usage

``` r
rowR(
  data,
  vars,
  type,
  new,
  na.rm = T,
  filter = NULL,
  match = "contains",
  direction
)
```

## Arguments

- data:

  dataset

- vars:

  vector of vars the rowwise operation should be perfomed

- type:

  pmin, pmax, all.na, any.na, sum.na, sum

- new:

  name of the new variable. If missing, type is inserted (e.g. pmin)

- na.rm:

  whether NAs should be removed (default = T)

- filter:

  whether rows with all.na or any.na should be removed

- match:

  whether the provided diagnosis codes should be matched exactly, start
  with/end with or contain (default)

- direction:

  the direction of the rowwise fill. Can be "left", "rigth", "leftright"
  and "rightleft". Corresponds to "updown".

## Value

the parent data frame with a new variable based on the selected rowwise
operator or a filtered dataset
