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
  method = "cox",
  vars,
  event.form = NULL,
  time = 120,
  breaks = 12,
  cause = 1,
  pl = T,
  digits = 4,
  event.digits = 2,
  alpha = 0.05,
  survtime = T,
  proportions = F,
  conditional = F,
  diagnostics = F,
  plot = T
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

- vars:

  Only applicable when "select" is chosen as type. The variables are
  added to the model as an additive model

- time:

  Time horizon of interest. Defaults to 60 (e.g. 5-years)

- breaks:

  Interim time points of interest. Defaults to 12 months (1-year gaps)

- cause:

  cause of interest, default = 1

- pl:

  Whether product.limit in ATE shoulde be T or F (default = T)

- digits:

  number of digits risk estimates in the returned results

- event.digits:

  Rounding of event times. Default is 2 to preserve exact times

- alpha:

  alpha level for the estimation of confidence intervals and p-values.
  Default = 0.05

- survtime:

  Whether median time to event should be calculated (default = T)

- proportions:

  Whether risk of event in different windows should be estimated
  (default = F)

- conditional:

  Whether conditional risk at the time horizon should be calculated
  (default = F)

- diagnostics:

  whether Scaled Schoenfield residuals should be visualized (default =
  F)

- plot:

  whether estimates for plotR should be performed (default = T)

- type:

  Model specification, Can be univariate ("uni"), Age- and sex
  standardized ("age-sex"), Multivariate with variable selection
  ("select"). In this case vars should be a vector of the covariates.
  "custom" allows for free modelling where the form-argument contains
  the formula.

- form:

  Only applicable when "custom" is chosen as type. Free specification of
  the model as the right-hand side of the formula.

## Value

List of class "estimatR" containing the following: table: Event table  
models: Model objects (cause1 and cause2)  
risks: Absolute risk estimates at the specified time points  
plot_data: Data for plotting CIF curves  
time_to_event: Median survival time  
hr: Hazard ratio between the groups  
difference: Absolute risk difference at the specified time horizons  
ratio: Absolute risk ratios at the the specified time horizon  
counts: Event and group counts in the contrasted groups  
info: information on arguments for extraction Optional: diag:
Diagnostics for assessing proportionality  
proportions:  
"Before" estimates the risk of event within a certain timepoint (e.g. x%
of the events occurred within).  
"After" estimates the risk of residual events between timepoint x and
ten years.  
"Window" estimates the percentage of events within six month windows
(e.g. x% of the events occurred between t1 and t2)  
conditional: Conditional risk estimates  

## Examples

``` r
estimatR(analysis_df, ttt, event, X1, type = "select", vars = c(X6,X7))
#> Error in estimatR(analysis_df, ttt, event, X1, type = "select", vars = c(X6,     X7)): unused argument (type = "select")

```
