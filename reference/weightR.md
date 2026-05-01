# Weight diagnostics for IPTW

Compute weights and diagnostic plots and mean differences

## Usage

``` r
weightR(
  data,
  model,
  treatment,
  vars,
  labs.headings = list(),
  labs.subheadings = list(),
  difference = "standardized",
  weights = "w",
  tail = 50,
  num.vars,
  simplify = F,
  digits = 2
)
```

## Arguments

- data:

  data.frame

- model:

  logistic regression model for estimating propensity scores

- treatment:

  treatment variable

- vars:

  vector of covariates to estimate weights

- labs.headings:

  list of varible names in the format list(new = "old")

- labs.subheadings:

  list of variable levels in the format list(var = list(new = "old"))

- difference:

  whether the results should be outputtet as absolute or standardized
  weighted differences

- weights:

  name of the column containing weights

- tail:

  the number of covariate-groups with the highest weights to show (e.g.
  top 50)

- num.vars:

  pseudo numerical variables for ordering of levels

- simplify:

  drop first level of binary variables

## Value

list containing a table of standardized differences (table), overall
propensity scores and weights (overall), balance plot (balance) and
weight plots (weightst)
