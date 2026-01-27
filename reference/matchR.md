# Perform exposure-density matching for multilevel data

Function for performing time-dependent exact matching (exposure density
sampling). For each case, n controls will be matched at index with exact
covariate values.

## Usage

``` r
matchR(
  data,
  td.frame,
  index = index,
  case = case,
  fu = fu,
  td.date = from,
  fixed.vars = c(byear, sex),
  td.vars,
  exclude,
  exclude.length = 365.25 * 100,
  n.controls = 4,
  replace = T,
  seed = 1,
  cores = 4,
  pnr = pnr,
  interval = NULL
)
```

## Arguments

- data:

  Dataset containing case/control indicator with index date for cases
  and fixed matching parameters

- td.frame:

  Dataset with time dependent matching covariates. Should be multiple
  rows for each patient with updated values for each parameter.

- index:

  Index date for cases, NA for controls

- case:

  1/0 indicator for case/control status

- fu:

  End of follow up as date

- td.date:

  Name of the date column in the time dependent matching covariate
  dataset

- fixed.vars:

  Vector of fixed matching parameters

- td.vars:

  Vector of time-dependent matching parameters

- exclude:

  Vector of parameters that are not allowed to occur before index

- exclude.length:

  Number of days before index where exclusions must not have occured.
  Default: 50 years (=never)

- n.controls:

  Number of desired controls per case

- replace:

  whether controls should be sampled with replacement (default = T)

- seed:

  Seed

- cores:

  Cores, defaults to 4

- pnr:

  Name of PNR column

- interval:

  Interval to split birthyear into intervals (e.g. 1950-1955). Assigns a
  new variable named "age_group".

## Value

Returns the same dataframe as the original but with n_control matches
and pairs indicated with a "set" column.
