# Factorize variables

Convenience function for levelling, labelling and referencing multiple
factors in one step. Works with piping. The order of effects is:

1.  Set as factor (fct_infreq setting levels based on frequency, most
    common is reference)

2.  Set reference levels

3.  Set levels manually

4.  Set labels

The list can be non-complete if no modifications should be passed to the
remaining variables

## Usage

``` r
factR(
  data,
  vars,
  num_vars,
  reference = list(),
  levels = list(),
  labels = list(),
  lab_to_lev = FALSE,
  reverse = F
)
```

## Arguments

- data:

  dataframe

- vars:

  Vector of variables that should be factorized

- num_vars:

  vector of variables with pseudonumeric ordering

- reference:

  List of variables with the reference level (e.g. list("v1" = "a"))

- levels:

  List of variables with the corresponding levels (e.g. list("v1" =
  "c("a","b","c","d","e")))

- labels:

  List of variables with the corresponding labels (e.g. list("v3" =
  c("e" = "epsilon", "d" = "delta")))

- lab_to_lev:

  Whether changing labels should change levels if these are not
  specified (defaults to TRUE)

- reverse:

  Whether the levels should be reversed (default is FALSE)

## Value

Returns the inputted dataframe with modified factor variables
