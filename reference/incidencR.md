# Estimate absolute risks in a single group with competing risks

Estimate absolute risks in a single group with competing risks

## Usage

``` r
incidencR(
  data,
  timevar,
  event,
  group,
  survscale = "AM",
  time = 60,
  breaks = 12,
  digits = 4,
  event.digits = 2,
  cause = 1,
  survtime = T,
  proportions = F,
  conditional = F
)
```

## Arguments

- data:

  dataframe

- timevar:

  time to event column

- event:

  event column with either 0/1 structure or 0/1/2 in case of competing
  risks

- group:

  optional grouping column

- survscale:

  Whether estimates should be presented as Overall survival or All-Cause
  Mortality

- time:

  time horizon (default=60 months)

- breaks:

  Interim time points of interest. Defaults to 12 months (1-year gaps)

- digits:

  for rounding of eventtimes

- event.digits:

  whether eventtimes should be rounded. Default is 2 to preserve exact
  times

- cause:

  cause of interest, default = 1

- survtime:

  Whether median time to event should be calculated (default = F)

- proportions:

  Whether risk of event in different windows should be estimated
  (default = F)

- conditional:

  Whether conditional risk at the time horizon should be calculated
  (default = F)

## Value

Life_table: Event table  
Plot_data: Data for plotting CIF curves  
Median_surv: Median survival time  
Surv: Indicator for the type of time-to-event analysis
