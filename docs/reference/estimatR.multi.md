# Perform multiple estimatR analyses

Perform multiple estimatR analyses

## Usage

``` r
estimatR.multi(data, timevar, event, group, names, ...)
```

## Arguments

- data:

  list of dataframe(s)

- timevar:

  Character vector of time variables. If missing "t\_" is assumed to be
  prefix for all names in the "events" vector

- event:

  Character vector of event variables

- group:

  Character vector of grouping variables

- names:

  Element names of the returned list of models. If missing the "events"
  names are used.

- ...:

  See arguments in estimatR(). Multiple arguments should be inputted as
  lists (e.g. time = list(60,60,120))

## Value

A named list of models with the estimatR function
