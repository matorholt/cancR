# Calculate median follow-up time using the inverse Kaplan-Meier method

Calculate median follow-up time using the inverse Kaplan-Meier method

## Usage

``` r
followR(df, timevar, event, group)
```

## Arguments

- df:

  Input dataframe

- timevar:

  Time to last follow-up

- event:

  Status indicator (censored=0 or dead=1)

- group:

  Optional group indicator if follow-up time should be reported
  separately

## Value

Returns the median follow-up time with IQR for the whole sample or
groupwise
