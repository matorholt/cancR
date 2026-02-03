# Causal inference of time-to-event data

Average treatment effect of point intervention with either IPTW,
GFORMULA or AIPTW. Wrapper for the ATE function in riskRegression.

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
  method = "tte",
  treat.form = NULL,
  event.form = NULL,
  cens.form = NULL,
  time = 120,
  breaks = 12,
  cause = 1,
  estimator = "AIPTW",
  plot = T,
  survtime = T,
  survscale = "AM",
  bins = 0.5,
  digits = 4,
  event.digits = 2,
  alpha = 0.05,
  weights.digits = 1,
  weights.breaks = 1
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

  vector of variables that should be included in the treatment,
  censoring and outcome models

- outcome.vars:

  vector of variables that should only be included in the outcome model

- censoring.vars:

  vector of variables that should only be included in the censoring
  model

- method:

  whether the outcome is "tte" (time-to-event) or "binary" (default =
  "tte")

- treat.form:

  custom formula for the RHS of the treatment model

- event.form:

  custom formula for the RHS of the outcome model

- cens.form:

  custom formula for the RHS of the censoring model

- time:

  Time horizon of interest. Defaults to 60 (e.g. 5-years)

- breaks:

  Interim time points of interest. Defaults to 12 months (1-year gaps)

- cause:

  cause of interest, default = 1

- estimator:

  whether the estimator should be "IPTW", "GFORMULA" or "AIPTW"
  (default)

- plot:

  whether plot data should be estimated for time optimization (default =
  T)

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

- alpha:

  alpha level for the estimation of confidence intervals and p-values.
  Default = 0.05

- weights.digits:

  the number of digits for categorization of weights

- weights.breaks:

  breaks for categorization of weights

## Value

table: Event table  
time_to_event: Median survival time  
plot_data: Data for plotting CIF curves  
risks: Absolute risk estimates at the specified time points  
differences: Absolute risk difference at the specified time horizons  
ratios: Absolute risk ratios at the the specified time horizon  
counts: Event and group counts in the contrasted groups  
weights: Object containing:  
data: the raw dataset with propensity scores (ps), weights (w) and
categorized weighting groups (wgroup)  
table_iptw: a table of the crude and iptw adjusted covariates  
table_strat: a table of the weigthing groups stratified on treatment
groups  
plot: plot of the propensity scores and weights for each group models:
Model objects (cause1 and cause2)  
diagnostics: Diagnostics for assessing proportionality  
info: information on arguments for extraction
