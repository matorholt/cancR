# Inclusion criteria for registry-studies

Inclusion criteria for registry-studies

## Usage

``` r
includR(
  data,
  exclusion.ex,
  exclusion.out,
  subgroup = NULL,
  age.limit = NULL,
  period = NULL,
  look.back = 365.25 * 5,
  index.shift = 0,
  fu = "fu",
  birth = "birth",
  remove = NULL,
  export = F,
  dt = F
)
```

## Arguments

- data:

  joined data frame

- exclusion.ex:

  vector of exposure variables that should not be present before index

- exclusion.out:

  vector of outcome variables that should not be present before index

- subgroup:

  filter for subgroups in the format: "level %in% c('a', 'b')" Can also
  be applied as custom filter

- age.limit:

  age limit for inclusion as numeric value

- period:

  character vector of length 2 specifying the enrollement period in the
  format c("1980-01-01", "2022-01-01")

- look.back:

  look back in days period where exposure and outcomes are not allowed
  to happen (default is five years - 365.25\*5)

- index.shift:

  number of days before index where exposure and outcomes are allowed,
  default = 0

- fu:

  name of the follow-up date (default: fu)

- birth:

  name of the birth date variable (default: birth)

- export:

  whether a data frame of the flowchart should be exported

## Value

A data frame with filtered cases and all controls + an rds file for flow
chart
