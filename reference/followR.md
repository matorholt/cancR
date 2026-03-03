# Calculate median follow-up time using the inverse Kaplan-Meier method

Calculate median follow-up time using the inverse Kaplan-Meier method

## Usage

``` r
followR(df, timevar, event, group, time.unit = "m2y")
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

## Examples

``` r
followR(analysis_df, time2, event, group = X2)
#> Error in FUN(X[[i]], ...): object 'X2' not found
```
