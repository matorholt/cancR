# Detection of positivity violations (empty levels)

Detection of positivity violations (empty levels)

## Usage

``` r
checkR(
  data,
  treatment = NULL,
  outcome = NULL,
  vars = NULL,
  id,
  levels = NULL,
  threshold = 0,
  return.counts = F,
  quantiles = "decile"
)
```

## Arguments

- data:

  data frame to detect positivity violations

- treatment:

  treatment stratum that should be included to all covariate
  combinations (optional)

- outcome:

  outcome stratum that should be included to all covariate combinations
  (optional)

- vars:

  vector of covariates to examine for positivity violations

- id:

  column indicating unique patient identifier for returning specific NAs

- levels:

  the number of covariates for which each treatment and/or outcome level
  will be counted (default = all covariate combinations)

- quantiles:

  quantile argument for categorization of numeric variables. See
  [`cutR()`](cutR.md) for supported quantiles. Default = "decile"

## Value

prints the variables with positivity violations if present, otherwise
none detected.
