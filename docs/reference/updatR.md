# Data reduction, updating and insertion of time-dependent covariates

Data reduction, updating and insertion of time-dependent covariates

## Usage

``` r
updatR(td.frame, update.frame, vars, indices = NULL, pnrs = NULL, dt = F)
```

## Arguments

- td.frame:

  data frame of time-dependent covariates with dates in from/to format

- update.frame:

  data.frame of index dates of covariates that should be
  updated/inserted

- vars:

  vector of variables to update/insert

- indices:

  vector of index dates for data reduction

- pnrs:

  vector of pnrs for data reduction

- dt:

  whether the dataframe should be returned as a data.table

## Value

A reduced and updated time-dependent data frame
