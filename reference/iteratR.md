# Perform multiple estimatR analyses

Perform multiple estimatR analyses

## Usage

``` r
iteratR(data, names, method, multivariable = F, ...)
```

## Arguments

- data:

  data frame, list of data frames or object of class "iteratR"

- names:

  vector of names for the returned list of models. If missing the
  "events" names or names in the iteratR object are used.

- method:

  which cancR function to use. Choose between "estimatR", "incidencR",
  "clustR" and "inferencR". The previous can be passed to "extractR" and
  "plotR"

- ...:

  See arguments in the specific function documentations. Multiple
  arguments should be inputted as lists (e.g. time = list(60,60,120))

## Value

A named list of models of the type specified in "method"
