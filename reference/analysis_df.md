# analysis_df

Simulation of a time-to-event data for the estimatR function

## Usage

``` r
analysis_df
```

## Format

A data frame with 2000 rows and 14 columns:

- X1:

  group variable with three levels (T0, T1 and T2)

- X2:

  group variable with two levels 0/1

- X3:

  group variable with three levels (T0, T1, T2 and T3)

- X4, X5:

  binary random covariates

- X6, X7, X8, X9, X10:

  continuous random covariates

- ttt, time2:

  event times

- event:

  binary status indicator (e.g. alive/death)

- event2:

  status indicator where 0 is censoring, 1 is event of interest and 2 is
  death as a competing risk

- event3:

  status indicator with multiple competing risks. 0=censorm, 1=event,
  2=death, 3=competing event

- id:

  patient id

- X6_bin, X7_bin, X8_bin, X9_bin, X10_bin:

  X6-X10 binned based on quintiles
