# Causal inference of time-to-event data

Average treatment effect of point intervention. Wrapper for the ATE
function in riskRegression

## Usage

``` r
inferencR(
  data,
  treatment,
  timevar,
  event,
  vars,
  outcome.vars,
  censoring.vars,
  time = 120,
  breaks = 12,
  estimator = "AIPTW",
  survtime = T,
  survscale = "AM",
  bins = 0.5,
  digits = 4,
  event.digits = 2
)
```

## Arguments

- data:

  dataframe

- treatment:

  column indicating treatment variable. Currently only binary treatments
  are supported.

- timevar:

  Time variable

- event:

  Status indicator

- vars:

  variables that the treatment, censoring and event models should
  include

- outcome.vars:

  variables that should only be included in the outcome models

- time:

  Time horizon of interest. Defaults to 60 (e.g. 5-years)

- breaks:

  Interim time points of interest. Defaults to 12 months (1-year gaps)

- estimator:

  whether the estimator should be "IPTW", "GFORMULA" or "AIPTW"
  (default)

- survtime:

  Whether median time to event should be calculated (default = F)

- survscale:

  Whether overall survial should be estimated as survival or all-cause
  mortality (1-survival)

- bins:

  bins for the weights plot, default = 0.5

- digits:

  for rounding of eventtimes

- event.digits:

  whether eventtimes should be rounded. Default is 2 to preserve exact
  times

## Value

time_to_event: Median survival time  
table: Event table  
plot_data: Data for plotting CIF curves  
models: Model objects (cause1 and cause2)  
diag: Diagnostics for assessing proportionality  
risks: Absolute risk estimates at the specified time points  
differences: Absolute risk difference at the specified time horizons  
ratios: Absolute risk ratios at the the specified time horizon  
counts: Event and group counts in the contrasted groups  
info: information on arguments for extraction
