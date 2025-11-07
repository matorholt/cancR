# Update of Charlson Comorbidity Index or covariate values over time

Update of Charlson Comorbidity Index or covariate values over time

## Usage

``` r
updatR(data, exclusion, match = "start")
```

## Arguments

- data:

  data frame with case IDS

- exclusion:

  Vector of diagnosis codes to be excluded (e.g. c("DC1", "DC2", "DC3"))

- lpr:

  lpr data frame. Loaded automatically if missing

- td.frame:

  Time-dependent frame containing CCI values to be changed. Loaded
  automatically if missing

## Value

An updated td.frame
