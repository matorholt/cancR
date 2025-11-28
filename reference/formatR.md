# Auto-formatting of a data frame with layout option for typical levels and labels

Auto-formatting of a data frame with layout option for typical levels
and labels

## Usage

``` r
formatR(
  data,
  cat.vars = c(case, sex, cci, region, education, income, marital),
  num.vars = c(period, age_group),
  cut_vars = c(age, index),
  labels = list(),
  seqlist = list(),
  names = list(),
  layout = NULL
)
```

## Arguments

- data:

  dataframe. Works with piping

- cat.vars:

  Vector of categorical covariates which should be formatted to factors

- num.vars:

  vector of pseudonumerical variables that should be ordered (e.g.
  1990-2000, 2000-2010).

- cut_vars:

  vector of numerical covariates that should be cut

- labels:

  list of labels (e.g. list("var" = c("a" = "apple", "b" = "banana")))

- seqlist:

  list of sequences for cutting (e.g. list("age" = seq(0,100,10)))

- names:

  names for the cut variable (e.g. list("age" = "age_group")

- layout:

  layout preset for common procedures. "Matching" for typical matching
  studies

## Value

Returns the same dataframe with print-friendly labels for the typical
full matching setup.
