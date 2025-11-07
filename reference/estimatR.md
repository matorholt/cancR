# Absolute risk estimation of time-to-event data with competing risks

Absolute risk estimation and comparison of two or more groups with a
time-to-event outcome. Wrapper for the prodlim and ate-functions in
riskRegression.

## Usage

``` r
estimatR(
  data,
  timevar,
  event,
  group,
  survscale = "AM",
  type = "uni",
  vars,
  form,
  time = 120,
  breaks = 12,
  survtime = T,
  proportions = F,
  conditional = F,
  pl = T,
  digits = 4,
  event.digits = 2
)
```

## Arguments

- data:

  dataset

- timevar:

  Time variable

- event:

  Status indicator

- group:

  Grouping variable

- survscale:

  Whether overall survial should be estimated as survival or all-cause
  mortality (1-survival)

- type:

  Model specification, Can be univariate ("uni"), Age- and sex
  standardized ("age-sex"), Multivariate with variable selection
  ("select"). In this case vars should be a vector of the covariates.
  "custom" allows for free modelling where the form-argument contains
  the formula.

- vars:

  Only applicable when "select" is chosen as type. The variables are
  added to the model as an additive model

- form:

  Only applicable when "custom" is chosen as type. Free specification of
  the model as the right-hand side of the formula.

- time:

  Time horizon of interest. Defaults to 60 (e.g. 5-years)

- breaks:

  Interim time points of interest. Defaults to 12 months (1-year gaps)

- survtime:

  Whether median time to event should be calculated (default = F)

- proportions:

  Whether risk of event in different windows should be estimated
  (default = F)

- conditional:

  Whether conditional risk at the time horizon should be calculated
  (default = F)

- pl:

  Whether product.limit in ATE shoulde be T or F (default = T)

- digits:

  for rounding of eventtimes

- event.digits:

  whether eventtimes should be rounded. Default is 2 to preserve exact
  times

## Value

time_to_event: Median survival time  
table: Event table  
plot_data: Data for plotting CIF curves  
hr: Hazard ratio between the groups  
models: Model objects (cause1 and cause2)  
diag: Diagnostics for assessing proportionality  
risks: Absolute risk estimates at the specified time points  
differences: Absolute risk difference at the specified time horizons  
ratios: Absolute risk ratios at the the specified time horizon  
counts: Event and group counts in the contrasted groups  
proportions:  
"Before" estimates the risk of event within a certain timepoint (e.g. x%
of the events occurred within).  
"After" estimates the risk of residual events between timepoint x and
ten years.  
"Window" estimates the percentage of events within six month windows
(e.g. x% of the events occurred between t1 and t2)  
conditional: Conditional risk estimates  
info: information on arguments for extraction
