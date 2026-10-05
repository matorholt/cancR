# Causal inference on time-to-event data with clustering and competing risks

Wrapper for the binregATE in the mets package for performing causal
inference on time-to-event data in a competing risk setting with
clustering. All covariates needs to be factorized.

## Usage

``` r
clustR(
  data,
  timevar,
  event,
  treatment,
  vars,
  outcome.vars,
  censoring.vars,
  cluster,
  time = 60,
  breaks = 12,
  digits = 4,
  event.digits = 2,
  cores = detectCores()/2
)
```

## Arguments

- data:

  dataset

- timevar:

  time-to-event variable

- event:

  status indicator as either 0/1 or 0/1/2

- treatment:

  treatment variable, must be coded as factor

- vars:

  vector of covariates. All variables must be coded as factors and
  continouous variables must be binned

- outcome.vars:

  variables that should only be included in the outcome models

- censoring.vars:

  variables that should only be included in the censoring models

- cluster:

  column indicating cluster variable (e.g. ID)

- time:

  time horizon

- breaks:

  subdivisions between time = 0 and fu

- digits:

  for rounding of eventtimes

- event.digits:

  whether eventtimes should be rounded. Default is 2 to preserve exact
  times

- cores:

  number of cores for parallel processing. Defaults to half of the
  available cores

## Value

table: Event table  
plot_data: Data for plotting CIF curves  
risks: Absolute risk estimates at the specified time points  
differences: Absolute risk difference at the specified time point  
info: information on arguments for extraction
